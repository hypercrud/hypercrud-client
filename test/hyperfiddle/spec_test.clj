(ns hyperfiddle.spec-test
  (:require [hyperfiddle.spec :as sut]
            [clojure.test :as t :refer [deftest are is testing]]
            [clojure.spec.alpha :as s]))

(t/use-fixtures :each (fn [f]
                        (sut/with-empty-spec
                          (f))))

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
