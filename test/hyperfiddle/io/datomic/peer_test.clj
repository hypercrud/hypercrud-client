(ns hyperfiddle.io.datomic.peer-test
  (:require
    [clojure.test :refer [deftest is]]
    [hyperfiddle.config]
    [hyperfiddle.domain]
    [hyperfiddle.io.datomic.core :as d]
    [hyperfiddle.io.datomic.peer]                           ; ?
    [hyperfiddle.api :as hf]))


(def config {:domain {:databases
                      {"$" {:database/uri #uri "datomic:mem://app-db"}
                       ; $hyperfiddle is required by the spec despite not being used in this context
                       "$hyperfiddle" {:database/uri #uri "datomic:mem://hyperfiddle"}
                       #_#_"$users" {:database/uri #uri "datomic:mem://hyperfiddle-users"}}}})

(def config (hyperfiddle.config/get-config config))
(def domain (hyperfiddle.config/get-domain config))

(defn with [$ tx]
  (:db-after (hf/with $ {:tx-data tx})))

(declare $ q)

(deftest limit-1
  (def conn (hf/connect domain "$"))
  (def $ (-> (hf/db conn)
           (with [{:db/ident :dustingetz/email :db/valueType :db.type/string :db/cardinality :db.cardinality/one}
                  {:db/ident :dustingetz/gender :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
                  {:db/ident :dustingetz/shirt-size :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
                  {:db/ident :dustingetz/type :db/valueType :db.type/keyword :db/cardinality :db.cardinality/one}])
           (with [{:dustingetz/type :dustingetz/gender :db/ident :dustingetz/male}
                  {:dustingetz/type :dustingetz/gender :db/ident :dustingetz/female}])
           (with [{:dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/mens-small :dustingetz/gender :dustingetz/male}
                  {:dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/mens-medium :dustingetz/gender :dustingetz/male}
                  {:dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/mens-large :dustingetz/gender :dustingetz/male}
                  {:dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/womens-small :dustingetz/gender :dustingetz/female}
                  {:dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/womens-medium :dustingetz/gender :dustingetz/female}
                  {:dustingetz/type :dustingetz/shirt-size :db/ident :dustingetz/womens-large :dustingetz/gender :dustingetz/female}])
           (with [{:dustingetz/email "alice@example.com" :dustingetz/gender :dustingetz/female :dustingetz/shirt-size :dustingetz/womens-large}
                  {:dustingetz/email "bob@example.com" :dustingetz/gender :dustingetz/male :dustingetz/shirt-size :dustingetz/mens-large}
                  {:dustingetz/email "charlie@example.com" :dustingetz/gender :dustingetz/male :dustingetz/shirt-size :dustingetz/mens-medium}])))
  (def q (hyperfiddle.io.datomic.core/qf2 (hf/database domain "$")))

  (is (= [17592186045428 17592186045429 17592186045430]
        (q {:query '[:find [?e ...] :where [?e :dustingetz/email]] :args [$] :limit 50})
        (q {:query '[:find [?e ...] :where [?e :dustingetz/email]] :args [$] :limit -1})
        (q {:query '[:find [?e ...] :where [?e :dustingetz/email]] :args [$]})))
  )
