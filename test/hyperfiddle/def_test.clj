(ns hyperfiddle.def-test
  (:require
    [clojure.test :refer [deftest is]]
    [hyperfiddle.def :as hf-def]
    [clojure.spec.alpha :as s]
    [clojure.tools.reader :as reader]))


(deftest read-schema|
  (is (= (vals (hf-def/read-schema {::a [:string]
                                    ::b [:string :foo true]
                                    ::c [:string :isComponent]
                                    ::d [:string :identity :many]
                                    ::e [:string :identity :many :index]}))
        [#:db{:ident :hyperfiddle.def-test/a, :cardinality :db.cardinality/one, :valueType :db.type/string}
         {:db/ident :hyperfiddle.def-test/b, :db/cardinality :db.cardinality/one, :db/valueType :db.type/string, :foo true}
         #:db{:ident :hyperfiddle.def-test/c, :cardinality :db.cardinality/one, :valueType :db.type/string, :isComponent true}
         #:db{:ident :hyperfiddle.def-test/d, :cardinality :db.cardinality/many, :valueType :db.type/string, :unique :db.unique/identity}
         #:db{:ident :hyperfiddle.def-test/e, :cardinality :db.cardinality/many, :valueType :db.type/string, :unique :db.unique/identity, :index true}])))

(def idos-schema-def
  #:idos.cpc
      {:website-url     [:string]
       :email           [:string :identity]
       :practice-name   [:string :identity "Name of the legal entity participating in the model"]
       :state           [:ref]
       :primary-contact [:ref :isComponent]
       :sites           [:ref* :isComponent]
       :confirm-email   [:boolean]})

(deftest def-schema|1
  (is (= (vals (hf-def/read-schema idos-schema-def))
        [#:db{:ident :idos.cpc/website-url, :cardinality :db.cardinality/one, :valueType :db.type/string}
         #:db{:ident :idos.cpc/email, :cardinality :db.cardinality/one, :valueType :db.type/string, :unique :db.unique/identity}
         #:db{:ident :idos.cpc/practice-name,
              :cardinality :db.cardinality/one,
              :valueType :db.type/string,
              :unique :db.unique/identity,
              :doc "Name of the legal entity participating in the model"}
         #:db{:ident :idos.cpc/state, :cardinality :db.cardinality/one, :valueType :db.type/ref}
         #:db{:ident :idos.cpc/primary-contact, :cardinality :db.cardinality/one, :valueType :db.type/ref, :isComponent true}
         #:db{:ident :idos.cpc/sites, :cardinality :db.cardinality/many, :valueType :db.type/ref, :isComponent true}
         #:db{:ident :idos.cpc/confirm-email, :cardinality :db.cardinality/one, :valueType :db.type/boolean}]
        )))

(deftest annotate-source|namespace-aliases
  (is (= (list :asdf.qwer/zxcv)
         (binding [reader/*alias-map* {'hfdt (create-ns 'asdf.qwer)}]
           (hf-def/annotate-source "::hfdt/zxcv"))))
  (is (= (list :user/zxcv)  ; Tests execute in the user namespace
         (binding [*ns* *ns*
                   reader/*alias-map* {}]
           (hf-def/annotate-source "::zxcv")))))
