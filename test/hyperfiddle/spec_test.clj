(ns hyperfiddle.spec-test
  (:require [hyperfiddle.spec :as sut]
            [hyperfiddle.spec.datomic :as sd]
            [clojure.test :as t :refer [deftest are is testing]]
            [clojure.spec.alpha :as s]))

(defmacro with-local-semantics
  "Rebinds the clojure.spec global registry to a temporary atom. Specs defined in
  body will therefor only be defined locally.
  The immutable local scope inherits the immutable global scope.
  TODO: make it thread-safe."
  [& body]
  `(with-redefs [s/registry-ref (atom (s/registry))]
     ~@body))

(defmacro with-empty-spec
  "Provide an empty local spec context in which no specs are defined. Useful to
  avoid naming conflict from the global registry in test fixtures."
  [& body]
  `(with-redefs [s/registry-ref (atom {})]
     ~@body))

(t/use-fixtures :each (fn [f] (with-empty-spec (f))))

(def defs (comp sut/defs sut/parse))
(def form (comp sut/form sut/parse))

(deftest parsing-forms
  (are [x y] (= x y)
    ::foo                                              (form ::foo)
    `string?                                           (form `string?)
    `(s/coll-of ::foo)                                 (form `(s/coll-of ::foo))
    `(s/coll-of string?)                               (form `(s/coll-of string?))
    `(s/coll-of string?
                :min-count 0
                :max-count 42
                :kind vector?)                         (form `(s/coll-of string? :min-count 0 :max-count 42 :kind vector?))
    `(s/keys :req-un [:a]
             :opt-un [:b]
             :req    [::c]
             :opt    [::d])                            (form `(s/keys :req-un [:a] :opt-un [:b] :req [::c] :opt [::d]))))

(deftest parsing-defs
  (are [x y] (= x y)
    `(s/def ::foo string?)                             (first (defs `(s/def ::foo string?)))
    `(s/def ::foo (s/coll-of string?))                 (first (defs `(s/def ::foo (s/coll-of string?))))
    `(s/def ::foo string?)                             (do (s/def ::foo string?)
                                                           (first (defs ::foo)))
    `((s/def ::foo string?)
      (s/def ::bar (s/keys :req [::foo]))
      (s/def ::baz (s/coll-of ::bar)))                 (do (s/def ::foo string?)
                                                           (s/def ::bar (s/keys :req [::foo]))
                                                           (s/def ::baz (s/coll-of ::bar))
                                                           (defs ::baz))
    `((s/def ::foo string?)
      (s/def ::bar (s/coll-of (s/keys :req [::foo])))) (do (s/def ::foo string?)
                                                           (s/def ::bar (s/coll-of (s/keys :req [::foo])))
                                                           (defs ::bar))))


(def datomic-attr (comp sd/from-spec sut/parse))

(deftest ref-and-cardinality
  (testing "A coll-spec should pick the :ref type when appropriate"
    (are [spec result] (= result ((juxt :db/valueType :db/cardinality) (datomic-attr spec)))
      `(s/coll-of map?)     [:db.type/ref :db.cardinality/many]
      `(s/coll-of (s/keys)) [:db.type/ref :db.cardinality/many]
      `(s/coll-of keyword?) [:db.type/keyword :db.cardinality/many]
      `(s/coll-of string?)  [:db.type/string :db.cardinality/many]
      `(s/coll-of double?)  [:db.type/double :db.cardinality/many]
      `(s/coll-of number?)  [:db.type/long :db.cardinality/many]
      `(s/coll-of inst?)    [:db.type/instant :db.cardinality/many]
      `(s/coll-of symbol?)  [:db.type/symbol :db.cardinality/many]
      `(s/coll-of uuid?)    [:db.type/uuid :db.cardinality/many]
      `map?                 [:db.type/ref :db.cardinality/one]
      `(s/keys)             [:db.type/ref :db.cardinality/one]
      `keyword?             [:db.type/keyword :db.cardinality/one]
      `string?              [:db.type/string :db.cardinality/one]
      `number?              [:db.type/long :db.cardinality/one]
      `inst?                [:db.type/instant :db.cardinality/one]
      `symbol?              [:db.type/symbol :db.cardinality/one]
      `uuid?                [:db.type/uuid :db.cardinality/one])))
