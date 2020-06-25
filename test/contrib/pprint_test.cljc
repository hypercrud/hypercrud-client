(ns contrib.pprint-test
  (:require
    [clojure.pprint]))


(def stage
  {nil
   {#uri "datomic:free://datomic:4334/hyperfiddle-blog-source"
    [[:db/retract 17592186045579 :fiddle/type :blank]
     [:db/add 17592186045579 :fiddle/type :entity]
     [:db/retract 17592186045579 :fiddle/pull "[:db/id *]"]
     [:db/add 17592186045579 :fiddle/pull "[:db/id :post/title]"]
     [:db/add 17592186046902 :link/disabled? true]
     [:db/add 17592186046578 :link/disabled? true]]}})

(def stage-str "{nil\n   {#uri \"datomic:free://datomic:4334/hyperfiddle-blog-source\"\n    [[:db/retract 17592186045579 :fiddle/type :blank]\n     [:db/add 17592186045579 :fiddle/type :entity]\n     [:db/retract 17592186045579 :fiddle/pull \"[:db/id *]\"]\n     [:db/add 17592186045579 :fiddle/pull \"[:db/id :post/title]\"]\n     [:db/add 17592186046902 :link/disabled? true]\n     [:db/add 17592186046578 :link/disabled? true]\n ]}}")

;(deftest pprint-datoms-1
;  []
;  (is (= (pprint-datoms-str stage) stage-str)))


(def code-form
  '(fn [ctx]
     (let [hide-datomic (reagent.core/atom true)
           db-attr? #(<= (:db/id %) 62)
           do-filter-reactive (fn [xs]
                                (as-> xs xs
                                      (if @hide-datomic (remove db-attr? xs) xs)))])))

#_(deftest pprint-performance-1 []

                                (time
                                  (with-out-str
                                    (clojure.pprint/pprint code-form)))

                                (time
                                  (with-out-str
                                    (packed-printer/pprint code-form)))

                                )
