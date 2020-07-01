(ns hyperfiddle.io.datomic.hfql-test
  (:require
    [clojure.test :as t :refer [deftest testing is]]
    [clojure.walk :as walk]
    [hyperfiddle.io.datomic.hfql :as hfql]
    [hyperfiddle.api :as hf]
    [datomic.api :as d]))

(defmethod t/assert-expr 'in-any-order [msg form]
  `(let [op1#     ~(nth form 1)
         op2#     ~(nth form 2)
         rewrite# #(walk/postwalk (fn [x#] (if (and (sequential? x#)
                                                   (not (map-entry? x#)))
                                            (set (seq x#))
                                            x#))
                                  %)
         result#  (= (rewrite# op1#) (rewrite# op2#))]
     (t/do-report
      {:type     (if result# :pass :fail)
       :message  ~msg
       :expected (format "%s should contain the same values as %s, in any order."
                         op1# op2#)
       :actual   result#})
     result#))

(defn ->mock-fn
  [{:keys [mapping expect] :as mock}]
  (let [log (atom {:calls [] :expectations (set expect)})]
    ^{:log log
      :mock-spec mock}
    (fn [& args]
      (let [args (into [] args)]
        (swap! log update :calls conj args)

        (doseq [expect (get @log :expectations)]
          (when (if (fn? expect) (apply expect args) (= args expect))
            (swap! log update :expectations disj args)))
        (if (contains? mapping args)
          (get mapping args)
          (->> mapping
              (filter (fn [[k v]] (fn? k)))
              (filter (fn [[k v]] (apply k args)))
              first
              second))))))

(defn refresh!
  [mock-fn]
  (reset! (:log (meta mock-fn)) {:calls [] :expectations (set (get (meta mock-fn) :mock-spec))}))

(defn satisfied?
  [mock-fn]
  (-> mock-fn meta :log deref :expectations empty?))

(deftest test|unweave
  (testing "Unweave"
    (is (= {:datomic [1] :hyperfiddle []}
           (hfql/unweave :datomic {:datomic [] :hyperfiddle []} 1)))
    (is (= {:datomic [1] :hyperfiddle [2]}
           (hfql/unweave nil {:datomic [] :hyperfiddle []} (hfql/->Result 2 1))))))

(defn a [])

(deftest test|transformer-map
  (testing "Transformer Map"
    (is (= {:as `a :query a}
           (apply (:hyperfiddle/query hfql/transformer-map) [`a])))))

(defn submissions [$ & args]
  (datomic.api/q
    '[:find [?e ...]
      :where [?e :scratch/email]] $))

(defn genders [$ & args]
  (datomic.api/q
    '[:find [(pull ?e [:db/ident]) ...]
      :where [?e :scratch/type :scratch/gender]]
    $))

(defn shirt-sizes [$ & [gender]]
  (datomic.api/q
    '[:in $ ?gender
      :find [?e ...]
      :where
      [?e :scratch/type :scratch/shirt-size]
      [?e :scratch/gender ?gender]]
    $ gender))

(def fiddle
  [{`submissions [:scratch/email
                  {:scratch/gender [:db/ident
                                    {`shirt-sizes [:db/ident]}]}
                  {:scratch/shirt-size [:db/ident]}]}
   {`genders nil}])

(defn with [$ tx]                                           ; can flip between d/transact and d/with
  (:db-after (d/with $ tx)))

(defn connect [param1]
  )

(def $
  (let [db-uri "datomic:mem://app-db"]
    (d/create-database db-uri)
    (-> (d/db (d/connect db-uri))
        (with [{:db/ident :scratch/email :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
               {:db/ident :scratch/gender :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
               {:db/ident :scratch/shirt-size :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
               {:db/ident :scratch/type :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}])
        (with [{:scratch/type :scratch/gender :db/ident :scratch/male}
               {:scratch/type :scratch/gender :db/ident :scratch/female}])
        (with [{:scratch/type :scratch/shirt-size :db/ident :scratch/mens-small :scratch/gender :scratch/male}
               {:scratch/type :scratch/shirt-size :db/ident :scratch/mens-medium :scratch/gender :scratch/male}
               {:scratch/type :scratch/shirt-size :db/ident :scratch/mens-large :scratch/gender :scratch/male}
               {:scratch/type :scratch/shirt-size :db/ident :scratch/womens-small :scratch/gender :scratch/female}
               {:scratch/type :scratch/shirt-size :db/ident :scratch/womens-medium :scratch/gender :scratch/female}
               {:scratch/type :scratch/shirt-size :db/ident :scratch/womens-large :scratch/gender :scratch/female}])
        (with [{:scratch/email "alice@example.com" :scratch/gender :scratch/female :scratch/shirt-size :scratch/womens-large}
               {:scratch/email "bob@example.com" :scratch/gender :scratch/male :scratch/shirt-size :scratch/mens-large}
               {:scratch/email "charlie@example.com" :scratch/gender :scratch/male :scratch/shirt-size :scratch/mens-medium}]))))


(deftest test|parse
  (testing "parse"
    (= [{:as :hyperfiddle.io.datomic.hfql-test/submissions,
         :query submissions,
         :pull [:scratch/email #:scratch{:gender [:db/ident]}],
         :dependencies #:scratch{:gender [{:as :hyperfiddle.io.datomic.hfql-test/shirt-sizes,
                                           :query shirt-sizes,
                                           :pull [:db/ident]}]}}
        {:as :hyperfiddle.io.datomic.hfql-test/genders,
         :query genders}]
       (hfql/parse fiddle))))

(deftest test|interpret
  (testing "Interpret"
    (is (in-any-order
         (hfql/interpret $ (hfql/parse fiddle))
         [{'hyperfiddle.io.datomic.hfql-test/submissions [#:scratch{:email "alice@example.com",
                                                                       :gender {:db/ident :scratch/female,
                                                                                'hyperfiddle.io.datomic.hfql-test/shirt-sizes [#:db{:ident :scratch/womens-medium}
                                                                                                                               #:db{:ident :scratch/womens-large}
                                                                                                                               #:db{:ident :scratch/womens-small}]}}
                                                             #:scratch{:email "bob@example.com",
                                                                       :gender {:db/ident :scratch/male,
                                                                                'hyperfiddle.io.datomic.hfql-test/shirt-sizes [#:db{:ident :scratch/mens-small}
                                                                                                                               #:db{:ident :scratch/mens-medium}
                                                                                                                               #:db{:ident :scratch/mens-large}]}}
                                                             #:scratch{:email "charlie@example.com",
                                                                       :gender {:db/ident :scratch/male,
                                                                                'hyperfiddle.io.datomic.hfql-test/shirt-sizes [#:db{:ident :scratch/mens-small}
                                                                                                                               #:db{:ident :scratch/mens-medium}
                                                                                                                               #:db{:ident :scratch/mens-large}]}}]}
            {'hyperfiddle.io.datomic.hfql-test/genders [#:db{:ident :scratch/male} #:db{:ident :scratch/female}]}]))))
