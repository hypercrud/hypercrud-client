(ns hyperfiddle.fiddle-test
  (:require
    [clojure.test :refer [deftest is]]
    [contrib.datomic :refer [smart-lookup-ref-no-tempids]]
    [hyperfiddle.fiddle :refer [apply-defaults]]))


(deftest test-data-defaults []
  #_(is (not (nil? (:fiddle/query (apply-defaults {:fiddle/type :query})))))
  (is (not (nil? (:fiddle/pull (apply-defaults {:fiddle/type :entity}))))))

#?(:cljs
   (deftest test-ui-defaults []
     (let [fiddle-val (apply-defaults {})]
       (is (not (nil? (:fiddle/markdown fiddle-val)))))))
