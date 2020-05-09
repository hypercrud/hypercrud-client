(ns hyperfiddle.io.datomic.hfql-test
  (:require
    [clojure.test :refer :all]
    [hyperfiddle.io.datomic.hfql :as hfql]
    [datascript.core :as d]))

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


(def query-result
  (into [] (range 1 3)))

(def pull-result
  [{:scratch/email "alice@example.com"}
   {:scratch/email "bob@example.com"}
   {:scratch/email "charlie@example.com"}])

(defn query
  [& _]
  query-result)

(def fiddle
  [{`query [:pull/attr0 :pull/attr1]}])

(def $ (fn []))

(deftest test|parse
  (testing "Parse"
    (= [{:as    `query
         :query query
         :pull  [:pull/attr0 :pull/attr1]}]
       (hfql/parse fiddle))))


(deftest test|interpret
  (testing "Interpret"
    (with-redefs [datomic.api/pull-many (->mock-fn {:mapping {(constantly true) pull-result}})]
      (is
        (= (hfql/interpret $ (hfql/parse fiddle))
           '[#:hyperfiddle.io.datomic.hfql-test{query [#:scratch{:email "alice@example.com"}
                                                       #:scratch{:email "bob@example.com"}
                                                       #:scratch{:email "charlie@example.com"}]}])))))