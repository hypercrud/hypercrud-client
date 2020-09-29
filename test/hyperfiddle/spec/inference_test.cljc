(ns hyperfiddle.spec.inference-test
  (:require #?(:clj [clojure.test :as t :refer [deftest is testing are]]
               :cljs [cljs.test :as t :include-macros true :refer [deftest is testing are]])
            [clojure.string :as str]
            [hyperfiddle.spec.inference :as sut]))

(def now #inst "2020-09-29T07:34:18.924-00:00")
(def zero-uuid #uuid "00000000-0000-0000-0000-000000000000")

(deftest about-types
  (testing "Infering a type from a email"
    (testing "must find correct types"
      (are [x y] (= y (sut/matching-types sut/*hierarchy* sut/predicates x))
        nil           #{:any :nil}
        ""            #{:any :string}
        1             #{:any :number :int :nat-int}
        -1            #{:any :number :int}
        1.5           #{:any :number :double}
        []            #{:any :coll :seqable :sequential :vector}
        ()            #{:any :coll :seq :seqable :sequential :list}
        (lazy-seq ()) #{:any :coll :seq :seqable :sequential}
        #{}           #{:any :coll :seqable :set}
        {}            #{:any :coll :seqable :map}
        true          #{:any :boolean}
        false         #{:any :boolean}
        #()           #{:any :fn}
        now           #{:any :inst}
        zero-uuid     #{:any :uuid}
        ))
    (testing "must shrink to the closest type"
      (are [x y] (= y (sut/type-of sut/*hierarchy* sut/predicates x))
        nil           :nil
        ""            :string
        1             :nat-int
        -1            :int
        1.5           :double
        []            :vector
        ()            :list
        (lazy-seq ()) :sequential
        #{}           :set
        {}            :map
        true          :boolean
        false         :boolean
        #()           :fn
        now           :inst
        zero-uuid     :uuid
        ))))

(def ast-for (comp first sut/infer))

(deftest about-ast
  (testing "Infering a simple email type should produce a correct AST"
    (are [x y] (= y (ast-for x))
      nil           {:type :nil}
      ""            {:type :string}
      1             {:type :nat-int}
      -1            {:type :int}
      1.5           {:type :double}
      []            {:type :vector}
      ()            {:type :list}
      (lazy-seq ()) {:type :sequential}
      #{}           {:type :set}
      {}            {:type :map}
      true          {:type :boolean}
      false         {:type :boolean}
      #()           {:type :fn}))

  (testing "Infering a complex email type should produce a correct AST"
    (are [x y] (= y (ast-for x))
      [1 2 3]               {:type :vector, :child {:type :nat-int}}
      [-1 1 1.5]            {:type  :vector,
                             :child {:type     :or,
                                     :branches {:int     {:type :int}
                                                :double  {:type :double}
                                                :nat-int {:type :nat-int}}}}
      #{:kw "str"}          {:type  :set
                             :child {:type     :or
                                     :branches {:keyword {:type :keyword}
                                                :string  {:type :string}}}}
      {"json-str" "json-val"
       "json-int" 1}        {:type :map
                             :keys {:type :string}
                             :vals {:type     :or
                                    :branches {:string  {:type :string}
                                               :nat-int {:type :nat-int}}}}
      {:user/name "John"}   {:type   :keys
                             :keyset #{:user/name}}
      [{:user/name "John"}] {:type  :vector
                             :child {:type   :keys
                                     :keyset #{:user/name}}}
      [{:user/id   1
        :user/name "John"}
       {:user/id  1
        :user/age 42}]      {:type  :vector
                             :child {:type   :keys
                                     :keyset #{:user/id :user/name :user/age}
                                     :req    #{:user/id}
                                     :opt    #{:user/name :user/age}}}

      [{:user/id      1
        :user/address {:address/id     2
                       :address/number 42}}
       {:user/id      2
        :user/address {:address/id     3
                       :address/street "Sesame"}}] {:type  :vector
                                                    :child {:type   :keys
                                                            :keyset #{:user/id :user/address}
                                                            :req    #{:user/id :user/address}
                                                            :opt    #{}}})))


(def registry-for (comp last sut/infer))

(deftest about-registry
  (testing "key-value pairs should build a registry of names and their respective definitions."
    (are [x y] (= y (registry-for x))
      ;; no global semantics → no registry
      nil                                          {}
      1                                            {}
      ""                                           {}
      {"json" "email"}                             {} ; Strings aren’t names
      {:ambiguous :key}                            {} ; :ambiguous can’t have global semantics
      ;; ---------------------
      {:user/name "John"}                          {:user/name {:type :string}}
      [{:user/id "string"}
       {:user/id 1}
       {:user/id zero-uuid}]                       {:user/id {:type     :or
                                                              :branches {:string  {:type :string}
                                                                         :nat-int {:type :nat-int}
                                                                         :uuid    {:type :uuid}}}}
      [{:user/id      1
        :user/address {:address/id     2
                       :address/number 42}}
       {:user/id      2
        :user/address {:address/id     3
                       :address/street "Sesame"}}] {:user/id        {:type :nat-int}
                                                    :user/address   {:type   :keys
                                                                     :keyset #{:address/id :address/number :address/street}
                                                                     :req    #{:address/id}
                                                                     :opt    #{:address/number :address/street}}
                                                    :address/id     {:type :nat-int}
                                                    :address/number {:type :nat-int}
                                                    :address/street {:type :string}})))

(deftest about-or
  (testing "In a branched structure, inner `s/or` branches should merge properly."
    (are [x y] (= x y)
      (ast-for [#{1} [1] #{""} [""] #{1.5}]) {:type  :vector
                                              :child {:type     :or
                                                      :branches {:set    {:type  :set
                                                                          :child {:type     :or
                                                                                  :branches {:nat-int {:type :nat-int}
                                                                                             :string  {:type :string}
                                                                                             :double  {:type :double}}}}
                                                                 :vector {:type  :vector
                                                                          :child {:type     :or
                                                                                  :branches {:nat-int {:type :nat-int}
                                                                                             :string  {:type :string}}}}}}}
      (registry-for [{:foo/bar [""]}
                     {:foo/bar #{1}}
                     {:foo/bar '((1))}
                     {:foo/bar '((""))}])    {:foo/bar {:type     :or
                                                        :branches {:vector {:type  :vector
                                                                            :child {:type :string}}
                                                                   :set    {:type  :set
                                                                            :child {:type :nat-int}}
                                                                   :list   {:type  :list
                                                                            :child {:type  :list
                                                                                    :child {:type     :or
                                                                                            :branches {:nat-int {:type :nat-int}
                                                                                                       :string  {:type :string}}}}}}}})))

(deftest about-extentions
  (testing "We should be able to provide new types"
    (let [h      (sut/derive sut/*hierarchy* :email :string)
          email? #(str/includes? % "@") ; good enough
          ps     (assoc sut/predicates :email email?)
          email  "foo@example.com"]
      (is (= #{:any :string :email} (sut/matching-types h ps email)))
      (is (= {:type :email} (first (sut/infer h ps email {}))))
      (is (= email? (sut/render h ps (sut/infer h ps email {})))))))

(comment
  (use 'criterium.core)
  (require '[clojure.test.check.generators :as gen])
  (require '[clojure.spec.alpha :as s])
  (require '[clj-async-profiler.core :as prof])
  (prof/serve-files 8081)
  (let [dataset (take 10000 (repeatedly #(gen/generate (s/gen (s/coll-of string?)))))]
    #_(with-progress-reporting
      (quick-bench))
    (sut/render sut/predicates (sut/infer dataset)))
  )

