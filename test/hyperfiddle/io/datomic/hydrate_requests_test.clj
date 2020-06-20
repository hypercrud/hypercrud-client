(ns hyperfiddle.io.datomic.hydrate-requests-test
  (:require
    [cats.monad.exception :as exception]
    [clojure.test :refer [deftest is use-fixtures]]
    [contrib.uri :refer [->URI]]
    [datomic.api :as d]
    [hyperfiddle.api :as hf]
    [hyperfiddle.io.datomic.hydrate-requests :as datomic-hydrate-requests]
    [hyperfiddle.io.datomic.peer :as peer]                  ; todo run tests for client as well
    ))


(def test-uri "datomic:mem://test")
(def test-dbname "$test")

(def test-domain
  (reify hf/Domain
    (connect [domain dbname] (-> (hf/database domain dbname) :database/uri peer/connect))
    (database [domain dbname] (get (hf/databases domain) dbname))
    (databases [domain]
      {test-dbname {:database/uri (->URI test-uri)}})))

(def schema [{:db/ident :person/name
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}
             {:db/ident :person/age
              :db/valueType :db.type/long
              :db/cardinality :db.cardinality/one}])

(use-fixtures :each (fn [f]
                      (d/create-database test-uri)
                      (d/transact (d/connect test-uri) schema)
                      (f)
                      (d/delete-database test-uri)))

(defn build-get-secure-db-with [partitions & args]
  (let [get-secure-db-with+ (apply datomic-hydrate-requests/build-get-secure-db-with+ test-domain (constantly partitions) args)]
    (fn [& args] (exception/extract (apply get-secure-db-with+ args)))))

(deftest schema-alteration []
  (let [conn (d/connect test-uri)
        partitions {hf/root-pid {:is-branched true
                                         :stage {test-dbname [[:db/add "-1" :db/ident :x/y]
                                                              [:db/add "-1" :db/valueType :db.type/string]
                                                              [:db/add "-1" :db/cardinality :db.cardinality/one]]}}}
        local-basis {"$test" (d/basis-t (d/db conn))}
        get-secure-db-with (build-get-secure-db-with partitions (atom {}) local-basis nil)
        $ (:db (get-secure-db-with "$test" hf/root-pid))]
    (is (= (as-> (d/touch (d/entity $ :x/y)) entity
             (into {} entity)
             (dissoc entity :db/id))
           {:db/ident :x/y
            :db/valueType :db.type/string
            :db/cardinality :db.cardinality/one}))))

(deftest branch-once []
  (let [conn (d/connect test-uri)
        partitions {hf/root-pid {:is-branched true
                                         :stage {test-dbname [{:db/id "-1" :person/name "John" :person/age 30}]}}}
        local-basis {test-dbname (d/basis-t (d/db conn))}
        get-secure-db-with (build-get-secure-db-with partitions (atom {}) local-basis nil)
        $ (:db (get-secure-db-with test-dbname hf/root-pid))]
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "John"]] $) 30))))

(deftest branch-once-stale []
  (let [conn (d/connect test-uri)
        partitions {hf/root-pid {:is-branched true
                                         :stage {test-dbname [{:db/id "-1" :person/name "John" :person/age 30}]}}}
        local-basis {test-dbname (d/basis-t (d/db conn))}   ; get a stale basis before another user transacts
        _ @(d/transact conn [{:db/id "-1" :person/name "Bob" :person/age 50}])
        get-secure-db-with (build-get-secure-db-with partitions (atom {}) local-basis nil)]
    (is (thrown-with-msg? RuntimeException (re-pattern datomic-hydrate-requests/ERROR-BRANCH-PAST)
                          (get-secure-db-with test-dbname hf/root-pid)))))

(deftest branch-popover []
  (let [conn (d/connect test-uri)
        partitions {hf/root-pid {:is-branched true
                                         :stage {test-dbname nil}
                                         :partition-children #{"2"}}
                    "2" {:parent-pid hf/root-pid
                         :is-branched true
                         :stage {test-dbname [{:db/id "-1" :person/name "John" :person/age 30}]}}}
        local-basis {test-dbname (d/basis-t (d/db conn))}
        get-secure-db-with (build-get-secure-db-with partitions (atom {}) local-basis nil)
        $-nil (:db (get-secure-db-with test-dbname hf/root-pid))
        $-2 (:db (get-secure-db-with test-dbname "2"))]
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "John"]] $-nil) nil))
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "John"]] $-2) 30))))

(deftest branch-popover-stale []
  (let [conn (d/connect test-uri)
        partitions {hf/root-pid {:is-branched true
                                         :partition-children #{"2"}
                                         :stage {test-dbname nil}}
                    "2" {:is-branched true
                         :parent-pid hf/root-pid
                         :stage {test-dbname [{:db/id "-1" :person/name "John" :person/age 30}]}}}
        local-basis {test-dbname (d/basis-t (d/db conn))}   ; get a stale basis before another user transacts
        _ @(d/transact conn [{:db/id "-1" :person/name "Bob" :person/age 50}])
        get-secure-db-with (build-get-secure-db-with partitions (atom {}) local-basis nil)
        $-nil (:db (get-secure-db-with test-dbname hf/root-pid))]
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "John"]] $-nil) nil))
    (is (thrown-with-msg? RuntimeException (re-pattern datomic-hydrate-requests/ERROR-BRANCH-PAST)
                          (get-secure-db-with test-dbname "2")))))

(deftest branch-twice []
  (let [conn (d/connect test-uri)
        partitions {hf/root-pid {:is-branched true
                                         :partition-children #{"2"}
                                         :stage {test-dbname [{:db/id "-1" :person/name "John" :person/age 30}]}}
                    "2" {:is-branched true
                         :parent-pid hf/root-pid
                         :stage {test-dbname
                                 [{:db/id "-2" :person/name "Alice" :person/age 40}]}}}
        local-basis {test-dbname (d/basis-t (d/db conn))}
        get-secure-db-with (build-get-secure-db-with partitions (atom {}) local-basis nil)
        $-nil (:db (get-secure-db-with test-dbname hf/root-pid))
        $-2 (:db (get-secure-db-with test-dbname "2"))]
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "John"]] $-nil) 30))
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "Alice"]] $-nil) nil))
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "John"]] $-2) 30))
    (is (= (d/q '[:find ?age . :where [?person :person/age ?age] [?person :person/name "Alice"]] $-2) 40))))

(deftest branch-twice-stale []
  (let [conn (d/connect test-uri)
        partitions {hf/root-pid {:is-branched true
                                         :stage {test-dbname [{:db/id "-1" :person/name "John" :person/age 30}]}
                                         :partition-children #{"2"}}
                    "2" {:is-branched true
                         :parent-pid hf/root-pid
                         :stage {test-dbname [{:db/id "-2" :person/name "Alice" :person/age 40}]}}}
        local-basis {test-dbname (d/basis-t (d/db conn))}   ; get a stale basis before another user transacts
        _ @(d/transact conn [{:db/id "-1" :person/name "Bob" :person/age 50}])
        get-secure-db-with (build-get-secure-db-with partitions (atom {}) local-basis nil)]
    (is (thrown-with-msg? RuntimeException (re-pattern datomic-hydrate-requests/ERROR-BRANCH-PAST)
                          (get-secure-db-with test-dbname hf/root-pid)))
    (is (thrown-with-msg? RuntimeException (re-pattern datomic-hydrate-requests/ERROR-BRANCH-PAST)
                          (get-secure-db-with test-dbname "2")))))
