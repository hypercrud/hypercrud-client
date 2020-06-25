(ns contrib.datomic-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [contrib.datomic :refer [pull-shape tree-derivative pull-enclosure pull-level
                             pull-traverse normalize-result
                             validate-qfind-attrs! result-enclosure!
                             pull-seq pp-recursion?]]
    [contrib.ct]
    [contrib.try$]
    [fixtures.ctx]
    [fixtures.domains]
    [fixtures.hfhf]
    [fixtures.tank]
    [datascript.parser #?@(:cljs [:refer [FindRel FindColl FindTuple FindScalar]])])
  #?(:clj (:import (datascript.parser FindRel FindColl FindTuple FindScalar))))


(def pull-pattern-1 '[:dustingetz.reg/email
                      :dustingetz.reg/age
                      *
                      {:dustingetz.reg/gender
                       [:db/ident]
                       :dustingetz.reg/shirt-size
                       [:db/ident
                        *]}
                      :db/id])

(def pp-1 '[:a/a
            [:a/comp-one :as "comp-one"]
            :a/comp-many
            *
            (limit :a/v 1)
            {(default :a/s {}) [* :b/a :b/b {:c/a [*]} {:c/b [:d/a]}]
             [:a/t :as "T"] [*]
             (limit :a/u 2) [*]
             :a/x [*]}
            (default :a/z "a/z default")])


(def pp-2 [:db/id
           :person/name
           {:person/parents 3}])

(deftest pull-seq-
  (is (= (->> (pull-seq pp-1)
              (filter (partial = '*))
              count)
         6))

  (is (= (->> (pull-seq pp-2)
              (filter (partial = '*))
              count)
         0))

  (is (= (->> (pull-seq pp-2)
              (filter pp-recursion?)
              count)
         1))

  (is (= (->> (pull-seq pp-1)
              (filter pp-recursion?)
              count)
         0))
  )

(deftest pull-shape-
  (is (= (pull-shape @(fixtures.tank/schemas "$soup") pp-1)
         [:a/a
          #:a{:comp-one [:db/id]}
          #:a{:comp-many [:db/id]}
          :a/v
          #:a{:s [:b/a :b/b #:c{:a []} #:c{:b [:d/a]}], :t [], :u [], :x []}
          :a/z]))
  (is (= (pull-shape fixtures.tank/schema pull-pattern-1)
         [:dustingetz.reg/email
          :dustingetz.reg/age
          #:dustingetz.reg{:gender [:db/ident],
                           :shirt-size [:db/ident]}
          :db/id]))
  (is (= (pull-shape fixtures.tank/schema
                     [:db/id
                      :hyperblog.post/sort-index1
                      :hyperblog.post/published
                      :hyperblog.post/hidden
                      :fiddle/ident
                      {:hyperblog.nav/children 1}
                      {:hyperblog.post/related 1}
                      :hyperblog.post/title])
         [:db/id
          :hyperblog.post/sort-index1
          :hyperblog.post/published
          :hyperblog.post/hidden
          :fiddle/ident
          :hyperblog.post/title])))

(def pulled-tree-1 {:db/id 17592186046765,
                    :hyperfiddle/owners [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"],
                    :reg/email "elizabeth@example.com",
                    :reg/age 65,
                    :reg/gender {:db/ident :gender/female},
                    :reg/shirt-size
                    {:db/id 17592186046209,
                     :db/ident :shirt-size/womens-medium,
                     :hyperfiddle/owners [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"],
                     :reg/gender {:db/id 17592186046204}}})

(def derivative-tests
  [["nested :many"
    fixtures.domains/schema
    {:db/id 17592186045942,
     :domain/home-route "",
     :domain/databases
     [{:db/id 17592186046525,
       :domain.database/name "$",
       :domain.database/record
       {:db/id 17592186046087,
        :database/uri
        #uri "datomic:free://datomic:4334/foo-ak"}}],}
    [:db/id :domain/home-route {:domain/databases [:db/id :domain.database/name {:domain.database/record [:db/id :database/uri]}]}]]
   ])

(deftest pulled-tree-derivative-2
  (is (= (tree-derivative
           fixtures.domains/schema
           {:db/id 17592186045942,
            :domain/home-route "",
            :domain/databases
            [{:db/id 17592186046525,
              :domain.database/name "$",
              :domain.database/record
              {:db/id 17592186046087,
               :database/uri
               #uri "datomic:free://datomic:4334/foo-ak"}}]})
         [:db/id
          :domain/home-route
          {:domain/databases
           [:db/id
            :domain.database/name
            {:domain.database/record
             [:db/id
              :database/uri]}]}]))

  (let [t derivative-tests]
    (doseq [[doc schema tree derivative] t]
      (is (= (tree-derivative schema tree)
             derivative)
          doc)))
  )

(deftest pulled-tree-derivative-1
  []
  (is (= [:db/id]
         (tree-derivative fixtures.ctx/schema {:db/id 17592186046204})))

  (is (= [:db/id :db/ident :hyperfiddle/owners #:reg{:gender [:db/id]}]
         (tree-derivative fixtures.ctx/schema {:db/id 17592186046209,
                                               :db/ident :shirt-size/womens-medium,
                                               :hyperfiddle/owners [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"],
                                               :reg/gender {:db/id 17592186046204}})))

  (is (= [:db/id
          :hyperfiddle/owners
          :reg/email
          :reg/age
          #:reg{:gender [:db/ident]}
          #:reg{:shirt-size [:db/id :db/ident :hyperfiddle/owners #:reg{:gender [:db/id]}]}]
         (tree-derivative fixtures.ctx/schema pulled-tree-1)))
  )

#_(deftest pull-union-
    (is (= (pull-union [:db/id {:reg/gender [:db/id]}]
                       [:db/ident {:reg/gender [:db/ident]}])
           [:db/id :db/ident #:reg{:gender [:db/id :db/ident]}]))

    (is (= (pull-union {:reg/gender [:db/id]}
                       {:reg/gender [:db/ident]})
           #:reg{:gender [:db/id :db/ident]}))

    (is (= (pull-union [:db/id {:reg/gender [:db/id]}]
                       [:db/ident {:reg/gender [:db/ident]}])
           [:db/id :db/ident #:reg{:gender [:db/id :db/ident]}]))

    (is (= (pull-union []
                       [:db/id {:reg/gender [:db/id]}]
                       [:db/ident {:reg/gender [:db/ident]}])
           [:db/id :db/ident #:reg{:gender [:db/id :db/ident]}]))

    #_(testing "pull order"
        ; Busted: The maps get merged (and maps don't have order) and :db/id hoists to before the maps.
        (is (= (pull-union [:dustingetz.reg/email
                            :dustingetz.reg/name
                            ; :dustingetz.reg/age
                            ; :dustingetz.reg/birthdate
                            {:dustingetz.reg/gender [:db/ident]}
                            {:dustingetz.reg/shirt-size [:db/ident]}
                            :db/id])
               [:dustingetz.reg/email :dustingetz.reg/name
                #:dustingetz.reg{:gender [:db/ident]}
                #:dustingetz.reg{:shirt-size [:db/ident]}
                :db/id])))

    (is (= (apply pull-union
                  [:reg/email :reg/age #:reg{:gender [:db/ident], :shirt-size [:db/ident]} :db/id]
                  [[:db/id
                    :hyperfiddle/owners
                    :reg/email
                    :reg/age
                    #:reg{:gender [:db/ident]}
                    #:reg{:shirt-size [:db/id :db/ident :hyperfiddle/owners #:reg{:gender [:db/id]}]}]
                   [:db/id
                    :hyperfiddle/owners
                    :reg/email
                    :reg/name
                    :reg/age
                    #:reg{:gender [:db/ident]}
                    #:reg{:shirt-size [:db/id :db/ident :hyperfiddle/owners #:reg{:gender [:db/id]}]}]])
           [:reg/email
            :reg/age
            :db/id
            :hyperfiddle/owners
            :reg/name
            #:reg{:gender [:db/ident], :shirt-size [:db/ident :db/id :hyperfiddle/owners #:reg{:gender [:db/id]}]}]))
    )


(def p [:db/id
        :community/name
        :community/neighborhood])
(def pt {:db/id 17592186045520,
         :community/name "15th Ave Community",
         :community/neighborhood {:db/id 17592186045519}})

(deftest enclosing-pull-shape-
  []
  (pull-enclosure fixtures.ctx/schema [] [{:db/ident :foo} {:db/id 10 :db/ident :yo}])
  (pull-enclosure fixtures.ctx/schema [:db/ident] [])
  (pull-enclosure fixtures.ctx/schema [] [{:db/id 17592186046209,
                                           :db/ident :shirt-size/womens-medium,
                                           :hyperfiddle/owners [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"],
                                           :reg/gender {:db/id 17592186046204}}
                                          {:db/id 17592186046209,
                                           :db/ident :shirt-size/womens-medium,
                                           :reg/gender {:db/id 17592186046204}}
                                          ])
  (pull-enclosure fixtures.ctx/schema (pull-shape fixtures.ctx/schema pull-pattern-1) fixtures.ctx/result-coll)
  (pull-enclosure fixtures.ctx/schema (pull-shape fixtures.ctx/schema pull-pattern-1) [])

  (testing "{:user/a-ref [:db/id]} is already in canonical pull shape"
    (is (= (pull-enclosure fixtures.tank/schema
                           [:db/id
                            :community/name
                            {:community/neighborhood [:db/id]}]
                           [pt])
           [:db/id :community/name #:community{:neighborhood [:db/id]}]))

    (is (= (tree-derivative fixtures.tank/schema pt)
           [:db/id :community/name #:community{:neighborhood [:db/id]}]))

    (is (= (pull-shape fixtures.tank/schema [:db/id
                                             :community/name
                                             {:community/neighborhood [:db/id]}])
           [:db/id :community/name #:community{:neighborhood [:db/id]}]))

    (is (= (pull-shape fixtures.tank/schema [:dustingetz.reg/age
                                             :dustingetz.reg/gender
                                             {:dustingetz.reg/gender [:db/id]}
                                             {:dustingetz.reg/gender [:db/ident]}])
           [:dustingetz.reg/age
            #:dustingetz.reg{:gender [:db/id]}
            #:dustingetz.reg{:gender [:db/id]}
            #:dustingetz.reg{:gender [:db/ident]}]))
    )

  (testing "normalize {:user/a-ref [:db/id]} relative to the requested pull shape"
    (is (= (pull-shape fixtures.tank/schema p)
           [:db/id :community/name #:community{:neighborhood [:db/id]}]))

    (is (= (pull-enclosure fixtures.tank/schema (pull-shape fixtures.tank/schema p) [pt])
           [:db/id :community/name #:community{:neighborhood [:db/id]}])))
  )

(deftest pull-traverse1
  (is (= (pull-traverse fixtures.ctx/schema [:reg/gender
                                             :db/id         ; In place order
                                             {:reg/shirt-size [:db/ident
                                                               :reg/gender
                                                               :db/ident
                                                               :db/id]}
                                             :db/id         ; Ignored, use first
                                             ])
         [[:reg/gender]
          [:db/id]
          [:reg/shirt-size]
          [:reg/shirt-size :db/ident]
          [:reg/shirt-size :reg/gender]
          [:reg/shirt-size :db/id]]))
  )

(def queries
  {FindColl '[:find [(pull ?e [:db/id                       ; self
                               :reg/email                   ; not a ref
                               :reg/age                     ; not a ref
                               {:reg/gender [:db/ident]     ;self at gender level
                                :reg/shirt-size [:db/ident]}]) ; self at gender level
                     ...]
              :where [?e :reg/email]]
   FindRel '[:find (sum ?age) (pull ?g [:db/ident]) :where [?g :age ?age]]
   FindTuple '[:find [(pull ?e [:db/id :fiddle/ident]) (max ?tx) ?entrypoint] :where [?e :a ?entrypoint ?tx]]
   FindScalar '[:find (sum ?age) . :where [:age ?age]]})

(defn parse-query [q] (->> (contrib.try$/try-either (datascript.parser/parse-query q))
                           (contrib.ct/unwrap (constantly nil))))

(def qparsed (into {} (map (juxt key (comp parse-query val)) queries)))

(deftest datalog-parsers
  (testing "parse FindColl"
    (let [{:keys [qfind]} (get qparsed FindColl)]
      (is (= (type qfind) FindColl))))

  (testing "parse FindRel"
    (let [{:keys [qfind]} (get qparsed FindRel)]
      (is (= (type qfind) FindRel))))

  (testing "parse FindTuple"
    (let [{:keys [qfind]} (get qparsed FindTuple)]
      (is (= (type qfind) FindTuple))))

  (testing "parse FindScalar"
    (let [{:keys [qfind]} (get qparsed FindScalar)]
      (is (= (type qfind) FindScalar))))
  )

(def results
  {FindColl [{:reg/email "alice"} {:reg/email "bob"}]
   FindRel [[20 {:db/ident :gender/male}] [65 {:db/ident :gender/female}]]
   FindTuple [{:db/id 136, :fiddle/ident :dustingetz/games} 13194139536334 true]
   FindScalar nil})

(deftest result-normalization
  []
  (is (= (normalize-result (:qfind (qparsed FindColl)) (results FindColl))
         [[{:reg/email "alice"}] [{:reg/email "bob"}]]))
  (is (= (normalize-result (:qfind (qparsed FindColl)) [])
         []))
  (is (= (normalize-result (:qfind (qparsed FindRel)) (results FindRel))
         (results FindRel)))
  (is (= (normalize-result (:qfind (qparsed FindTuple)) (results FindTuple))
         [[{:db/id 136, :fiddle/ident :dustingetz/games} 13194139536334 true]]))
  )

(def q-with-nesting-order '[:find
                            (pull ?e [:db/id
                                      {:community/neighborhood
                                       [:neighborhood/name
                                        {:neighborhood/district
                                         [:district/name
                                          {:district/region
                                           [:db/ident]}]}]}
                                      :community/name])
                            .
                            :where
                            [?e :community/name ?name]
                            [(< ?name "A")]])

(deftest pull-enclosure1
  (testing "Doesn't muck with order if no splat"
    (is (= (result-enclosure! fixtures.tank/schemas
                              (:qfind (parse-query q-with-nesting-order))
                              [])
           [[:db/id
             #:community{:neighborhood [:neighborhood/name
                                        #:neighborhood{:district [:district/name #:district{:region [:db/ident]}]}]}
             :community/name]]))))


(deftest pull-strata-
  (def pull [:dustingetz.reg/email
             :dustingetz.reg/name
             ; :dustingetz.reg/age
             ; :dustingetz.reg/birthdate
             {:dustingetz.reg/gender [:db/ident]}
             {:dustingetz.reg/shirt-size [:db/ident]}
             :db/id])
  (testing "nested pulls"
    (is (= (contrib.datomic/pull-level pull)
           '(:dustingetz.reg/email :dustingetz.reg/name :dustingetz.reg/gender :dustingetz.reg/shirt-size :db/id)))
    (is (= (contrib.datomic/pullshape-get pull :dustingetz.reg/gender)
           '(:db/ident))))

  (testing "identity"
    (is (= (pull-level [:db/id :db/ident :hyperfiddle/owners #:reg{:gender [:db/id]}])
           '(:db/id :db/ident :hyperfiddle/owners :reg/gender))))
  )

(deftest schema
  (testing "schema helpers"
    (is (satisfies? contrib.datomic/SchemaIndexedNormalized fixtures.hfhf/schema))
    (is (= (contrib.datomic/valueType fixtures.hfhf/schema :link/fiddle) :db.type/ref))
    (is (contrib.datomic/valueType? fixtures.hfhf/schema :link/fiddle :db.type/ref))
    (is (= (contrib.datomic/cardinality fixtures.hfhf/schema :link/fiddle) :db.cardinality/one))
    (is (contrib.datomic/cardinality? fixtures.hfhf/schema :link/fiddle :db.cardinality/one))
    (is (contrib.datomic/ref-one? fixtures.hfhf/schema :link/fiddle))
    (is (contrib.datomic/unique? fixtures.hfhf/schema :db/ident :db.unique/identity))
    (is (contrib.datomic/unique? fixtures.hfhf/schema :fiddle/ident :db.unique/identity))
    (is (contrib.datomic/attr fixtures.hfhf/schema :fiddle/ident))
    (is (contains? fixtures.hfhf/schema :fiddle/ident))
    (is (not (contains? fixtures.hfhf/schema :fiddle/ident1)))
    (is (contains? fixtures.hfhf/schema :link/fiddle))
    (is (= (contrib.datomic/find-identity-attr fixtures.hfhf/schema {:a 1 :fiddle/ident :yo}) :fiddle/ident))
    (is (= (contrib.datomic/find-identity-attr fixtures.hfhf/schema {:a 1}) nil))
    (is (= (contrib.datomic/find-identity-attr fixtures.hfhf/schema nil) nil))
    (is (= (contrib.datomic/attr nil :fiddle/ident) nil))
    )

  (testing "reverse navigation"
    (is (contrib.datomic/attr fixtures.hfhf/schema :link/_fiddle))
    (is (= (contrib.datomic/valueType fixtures.hfhf/schema :link/_fiddle) :db.type/ref))
    (is (= (contrib.datomic/attr fixtures.hfhf/schema :link/_fiddle)
           #:db{:ident :link/_fiddle, :valueType :db.type/ref, :cardinality :db.cardinality/many}))
    (is (= (contrib.datomic/cardinality fixtures.hfhf/schema :link/_fiddle) :db.cardinality/many))
    (is (= (contrib.datomic/cardinality fixtures.hfhf/schema :fiddle/_links) :db.cardinality/one))
    )
  )


(def pull-link [:db/id :link/class :link/formula :link/path :link/rel :link/tx-fn
                #:link{:fiddle [:db/id :fiddle/ident :fiddle/query :fiddle/type]}])
(def pull-fiddle [:db/id :fiddle/ident #:fiddle{:links pull-link}])

(deftest query-parsing
  (testing ""
    (is (= (contrib.datomic/qfind-collapse-findrel-1 (:qfind (datascript.parser/parse-query '[:find ?e :where [?e]])))
           (contrib.datomic/qfind-collapse-findrel-1 (:qfind (datascript.parser/parse-query '[:find [?e ...] :where [?e]])))))
    )
  )

(deftest pull-validation
  (let [qparsed (datascript.parser/parse-query '[:find
                                                 (pull ?e
                                                       [:db/id
                                                        :dustingetz.reg/email
                                                        :yo
                                                        :dustingetz.reg/age
                                                        {:dustingetz.reg/gender [:db/ident :foo]
                                                         :baz [:db/id :buzz]
                                                         :dustingetz.reg/shirt-size [:db/ident]}])
                                                 (max ?tx)
                                                 :where [?e _ _ ?tx]])]
    (is (= (validate-qfind-attrs! fixtures.tank/schemas (:qfind qparsed))
           [:yo :foo :baz :buzz])))

  )
