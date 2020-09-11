(ns hyperfiddle.spec.inference-test
  (:require [hyperfiddle.spec.inference :as sut]
            #?(:clj [clojure.test :as t :refer [deftest is testing are]]
               :cljs [cljs.test :as t :include-macros true :refer [deftest is testing are]])))

(deftest infering-a-type
  (testing "Infering a type from a value"
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
        ))))
