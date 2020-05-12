;(ns hyperfiddle.security.owner-only-test
;  (:require
;    [clojure.test :refer [compose-fixtures deftest is use-fixtures testing]]
;    [hyperfiddle.api :as hf]
;    [hyperfiddle.directory.fixtures :as fixtures] ; todo these tests can run without a directory service
;    [hyperfiddle.directory.provisioning :as dp]
;    [hyperfiddle.domain :as domain]
;    [hyperfiddle.io.datomic.transact :as transact]
;    [hyperfiddle.security :as security]
;    [hyperfiddle.security.entity-ownership])
;  (:import
;    (java.util UUID)))
;
;
;(def schema
;  [{:db/ident :person/email
;    :db/valueType :db.type/string
;    :db/unique :db.unique/identity
;    :db/cardinality :db.cardinality/one}
;   {:db/ident :person/name
;    :db/valueType :db.type/string
;    :db/cardinality :db.cardinality/one}
;   {:db/ident :person/age
;    :db/valueType :db.type/long
;    :db/cardinality :db.cardinality/one}])
;
;(def db-owner (UUID/randomUUID))
;
;(use-fixtures :each
;  (compose-fixtures
;    (fixtures/domains-fixture #{db-owner})
;    (fixtures/db-fixture fixtures/test-uri #{db-owner} db-owner :schema schema :security ::security/owner-only)))
;
;(defn process-tx [domains-uri subject hf-db-uri tx]
;  (let [hf-db (-> (dp/build-util-domain domains-uri)
;                  (hf/database hf-db-uri))]
;    (transact/process-tx $ domain dbname subject tx)))
;
;(defn transact! [domains-uri subject tx-groups]
;  (transact/transact! (dp/build-util-domain domains-uri) subject tx-groups))
;
;(deftest test-merging-tx-statements []
;  (let [email "asdf@example.com"]
;    (transact! fixtures/test-domains-uri db-owner
;               {fixtures/test-uri
;                [[:db/add "-1" :person/email email]
;                 [:db/add "-1" :person/name "Asdf"]]})
;    (let [mergable-tx [[:db/add "-1" :person/email email]
;                       [:db/add "-1" :person/age 1]]]
;      (testing "fails for non-owner"
;        (is (thrown-with-msg?
;              RuntimeException #"user tx failed validation"
;              (process-tx fixtures/test-domains-uri "someone-else" fixtures/test-uri mergable-tx))))
;
;      (testing "succeeds for owner"
;        (is (= mergable-tx (process-tx fixtures/test-domains-uri db-owner fixtures/test-uri mergable-tx)))))))
;
;(deftest test-merging-tx-maps []
;  (let [email "asdf@example.com"]
;    (transact! fixtures/test-domains-uri db-owner
;               {fixtures/test-uri
;                [{:person/email email
;                  :person/name "Asdf"}]})
;    (let [mergable-tx [{:person/email email
;                        :person/age 1}]]
;      (testing "fails for non-owner"
;        (is (thrown-with-msg?
;              RuntimeException #"user tx failed validation"
;              (process-tx fixtures/test-domains-uri "someone-else" fixtures/test-uri mergable-tx))))
;
;      (testing "succeeds for owner"
;        (is (= mergable-tx (process-tx fixtures/test-domains-uri db-owner fixtures/test-uri mergable-tx)))))))
