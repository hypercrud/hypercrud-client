(ns hyperfiddle.io.datomic.hydrate-route-test
  (:require
    [cats.monad.exception :as exception]
    [clojure.java.io :as io]
    [clojure.test :refer [deftest is testing use-fixtures]]
    [contrib.reader :as reader]
    [contrib.uri :refer [->URI]]
    [hyperfiddle.api :as hf]
    [hyperfiddle.database.fixtures :as fixtures]
    [hyperfiddle.io.datomic.hydrate-route :as hydrate-route]
    [hyperfiddle.io.datomic.peer :as peer]                  ; todo run tests for client as well
    [hyperfiddle.io.datomic.sync :as datomic-sync]
    [hyperfiddle.route :as route]
    [contrib.promise :as promise]
    [taoensso.timbre :as timbre])
  (:import
    (clojure.lang ExceptionInfo)
    (java.util.regex Pattern)))


(def fiddle-ontology (-> (io/resource "schema/fiddle.edn") slurp reader/read-edn-string!))

(def test-domain
  (reify hf/Domain
    (connect [domain dbname] (-> (hf/database domain dbname) :database/uri peer/connect))
    (fiddle-dbname [domain] "$src")
    (database [domain dbname] (get (hf/databases domain) dbname))
    (databases [domain]
      {"$" {:database/uri (->URI (str "datomic:mem://" 'hyperfiddle.io.datomic.hydrate-route-test "$"))}
       "$src" {:database/uri (->URI (str "datomic:mem://" 'hyperfiddle.io.datomic.hydrate-route-test "$src"))}})
    ))

(use-fixtures :each
  (fixtures/init-domain
    test-domain
    :schemas {"$" [{:db/ident :person/name
                    :db/valueType :db.type/string
                    :db/unique :db.unique/identity
                    :db/cardinality :db.cardinality/one}
                   {:db/ident :person/age
                    :db/valueType :db.type/long
                    :db/cardinality :db.cardinality/one}]
              "$src" fiddle-ontology}
    :init-txs {"$" [{:person/name "Bob"
                     :person/age 40}
                    {:person/name "Sue"
                     :person/age 50}]
               "$src" [{:fiddle/ident :persons
                        :fiddle/type :query
                        :fiddle/query (str '[:find (pull ?e [*]) :where [?e :person/name]])}]}))

(deftest duplicate-datoms []
  (testing "non source db"
    (let [pid hf/root-pid
          response (timbre/with-config {:enabled? false}
                     (let [local-basis (datomic-sync/sync test-domain ["$" "$src"])
                           route {::route/fiddle :persons}
                           partitions {hf/root-pid {:is-branched true
                                                            :stage {"$" [[:db/add [:person/name "Bob"] :person/age 41]
                                                                         [:db/add [:person/name "Bob"] :person/age 42]]}}}
                           subject nil]
                       @(hydrate-route/hydrate-route test-domain local-basis route pid partitions subject)))]
      (is (exception/failure? (get-in response [pid :schemas "$"])))
      (is (exception/success? (get-in response [pid :schemas "$src"])))))

  (testing "source db"
    (let [response+ (timbre/with-config {:enabled? false}
                      (let [local-basis (datomic-sync/sync test-domain ["$" "$src"])
                            route {::route/fiddle :persons}
                            pid hf/root-pid
                            partitions {hf/root-pid {:is-branched true
                                                             :stage {"$src" [[:db/add [:fiddle/ident :persons] :db/doc "foo"]
                                                                             [:db/add [:fiddle/ident :persons] :db/doc "bar"]]}}}
                            subject nil]
                        @(promise/branch (hydrate-route/hydrate-route test-domain local-basis route pid partitions subject)
                                   exception/success
                                   exception/failure)))]
      ; issue #1026
      #_(is (thrown-with-msg? ExceptionInfo (re-pattern (Pattern/quote ":db.error/datoms-conflict")) @response+)))))

(comment

  (duplicate-datoms)
  (clojure.test/run-tests)
  )
