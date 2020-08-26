(ns hyperfiddle.fiddle-test
  (:require
    [clojure.test :refer [deftest is]]
    [hyperfiddle.fiddle :refer [apply-defaults]]))

#?(:cljs
   (deftest test-ui-defaults []
     (let [fiddle-val (apply-defaults {})]
       (is (not (nil? (:fiddle/markdown fiddle-val)))))))
