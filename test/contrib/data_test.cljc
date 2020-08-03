(ns contrib.data-test
  (:require
    [clojure.test :refer [deftest is testing]]
    [contrib.string :refer [blank->nil]]
    [contrib.data :refer [xorxs
                          cond-let map-pad rtrim-coll fix-arity fvor take-to ungroup dissoc-nils
                          compare-by-index ancestry-common ancestry-divergence merge-by collect orp
                          assoc-if keywordize
                          qualify unqualify distinct-by]]))


(comment
  (map + [1 1 1] [1 1 1 1])                                 ;=> (2 2 2)
  ((map-pad 0) + [1 1 1] [1 1 1 1])                         ;=> (2 2 2 1)
  )

(deftest pad-1
  []
  (is (= ((map-pad 0) + [1 1 1] [1 1 1 1]) '(2 2 2 1)))
  )

(deftest test-take-to []
  (is (= (take-to #(not= % 4) (range 10))
         (range 5)))

  (is (= (take-to #{:a :b} [:a :b :a :c :a :b :a])
         (list :a :b :a :c))))

(deftest test-cond-let []
  (is (= 2 (cond-let
             [a 1] (inc a)
             [a 3] "it's 3!")))

  (is (= "it's 3!"
         (cond-let
           [a nil] (inc a)
           [b 3] "it's 3!")))

  (is (nil? (cond-let
              [a nil] (inc a)))))

(deftest ungroup-1
  []
  (ungroup {[] [:hf/self :hf/remove :hf/new],
            [:reg/email] [],
            [:reg/age] [],
            [:reg/gender] [:hf/self :hf/affix :hf/detach],
            [:reg/shirt-size] [:hf/self :hf/affix :hf/detach],
            [:reg/shirt-size :reg/gender] [:hf/self :hf/affix :hf/detach]})

  (ungroup '([[] [:hf/self :hf/remove :hf/new]]
              [[:reg/email] []]
              [[:reg/age] []]
              [[:reg/gender] [:hf/self :hf/affix :hf/detach]]
              [[:reg/shirt-size] [:hf/self :hf/affix :hf/detach]]
              [[:reg/shirt-size :reg/gender] [:hf/self :hf/affix :hf/detach]]))

  )

(deftest seq-rtrim-1
  []
  (is (= (rtrim-coll nil? [:post nil]) [:post]))
  (is (= (rtrim-coll nil? [:post nil nil "frag"]) [:post nil nil "frag"]))
  )

(deftest fix-arity-1 []
  (is (= ((fix-arity (fnil inc 0) 1) 42) 43))
  (is (= ((fix-arity (fnil inc 0) 1) nil) 1))
  (is (= ((fix-arity (fnil inc 0) 1) nil :a) 1)))

(deftest fvor-1 []
  (is (= ((fvor #(.toLocaleDateString %) "–") nil) "–")))

(def ordering [:fiddle/ident :fiddle/type :fiddle/pull-database :fiddle/pull :fiddle/query
               :fiddle/renderer :fiddle/markdown :fiddle/links :fiddle/hydrate-result-as-fiddle])
(deftest compare-by-index-
  []
  (is (= (apply sorted-set-by
                (compare-by-index ordering)
                [:fiddle/renderer :fiddle/ident :fiddle/markdown :fiddle/type])
         (sorted-set :fiddle/ident :fiddle/type :fiddle/renderer :fiddle/markdown))))

(deftest ancestry-common-1
  []
  (is (= (ancestry-common [1 2 3 4 5 6] [1 2 3 4 10 11]) '(1 2 3 4)))
  (is (= (ancestry-common [0 :reg/gender] [0 :reg/shirt-size]) '(0)))
  (is (= (ancestry-common [0 :user/user-id] '(0 :reg/gender)) '(0)))
  (is (= (ancestry-common [] [0 :fiddle/links :link/fiddle]) '()))
  (is (= (ancestry-common [0] [0 :fiddle/links :link/fiddle]) '(0)))
  (is (= (ancestry-common [0 :fiddle/links :link/fiddle] [0]) '(0)))
  (is (= (ancestry-common [0 :fiddle/links :link/fiddle] []) '()))
  )

(deftest ancestry-divergence-1
  []
  (is (= (ancestry-divergence [1 2 3 4 5 6] [1 2 3 4 10 11]) '(5 6)))
  (is (= (ancestry-divergence [0 :reg/gender] [0 :reg/shirt-size]) '(:reg/gender)))
  (is (= (ancestry-divergence [0 :user/user-id] '(0 :reg/gender)) '(:user/user-id)))
  (is (= (ancestry-divergence [] [0 :fiddle/links :link/fiddle]) '()))
  (is (= (ancestry-divergence [0] [0 :fiddle/links :link/fiddle]) '()))
  (is (= (ancestry-divergence [0 :fiddle/links :link/fiddle] [0]) '(:fiddle/links :link/fiddle)))
  (is (= (ancestry-divergence [0 :fiddle/links :link/fiddle] []) '(0 :fiddle/links :link/fiddle)))
  )

(deftest merge-by-1
  []
  (is (= (merge-by (juxt :link/rel (comp blank->nil :link/path))
                   [{:link/rel :hf/edit :link/path nil}
                    {:link/rel :hf/edit :link/path :reg/gender}
                    {:link/rel :hf/edit :link/path :reg/shirt-size}]
                   [{:link/rel :hf/edit :link/path :reg/gender :link/extra-stuff true}])
         '({:link/rel :hf/edit, :link/path nil}
            {:link/rel :hf/edit, :link/path :reg/gender, :link/extra-stuff true}
            {:link/rel :hf/edit, :link/path :reg/shirt-size}))))

(deftest collect-
  []
  (is (= (collect [[[0 :foo/bar] :missing]
                   [[0 :foo/bar] :b]])
         {[0 :foo/bar] [:missing :b]}))
  )

(deftest dissoc-nils-
  (is (= (dissoc-nils {:a 42 :b nil nil 43})
         {:a 42 nil 43}))
  )

(deftest orp-test []
  (is (= false (orp some? nil false 1)))
  (is (= 6 (orp even? 1 3 5 6 7))))

(deftest xorxs-test
  []
  (is (= (xorxs :a [])
         (xorxs :a)
         (xorxs [:a])
         [:a]))
  (is (= (xorxs :a #{})
         (xorxs #{:a})
         #{:a}))
  (is (= (xorxs nil) nil))
  (is (= (xorxs nil #{}) #{}))
  )

(def result (->> (iterate inc 0)
                 (take 10)
                 (map #(assoc {} :id %))))

(last result)

(deftest group-by-
  (testing "maintains order"
    (is (= (->> result
                reverse
                (contrib.data/group-by-unique-ordered :id)
                last)
           [0 {:id 0}]))

    )
  )

(deftest assoc-if'
  (is (= (assoc-if {:a 1} :b 2) {:a 1, :b 2}))
  (is (= (assoc-if {:a 1} :b nil) {:a 1}))
  )

(deftest keywordize'
  (is (= (keywordize ":yo") (keywordize "yo") :yo))
  )

(deftest unqualify'
  (is (= (unqualify nil) nil))
  (is (= (unqualify :db.cardinality/one) :one))
  (is (= (unqualify :one) :one))
  )

(deftest qualify'
  (is (= (qualify :foo nil) nil))
  (is (thrown? #?(:clj AssertionError :cljs js/Error) (qualify nil nil)))
  (is (= (qualify :foo :bar) :foo/bar))
  (is (thrown? #?(:clj AssertionError :cljs js/Error) (qualify :baz/flux :bar)))
  (is (thrown? #?(:clj AssertionError :cljs js/Error) (qualify nil :bar)))
  )

(deftest distinct-by'
  (is (= (list 1 2) (distinct-by even? (range 1 10))))
  (is (= (list {:x 1} {:x 2} {:x 3}) (distinct-by :x (concat [{:x 1} {:x 2}] (repeat 10 {:x 3}))))))
