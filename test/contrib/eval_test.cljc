(ns contrib.eval-test
  (:require [cats.monad.either :refer [#?(:cljs Left)]]
            [clojure.test :refer [deftest is]]
            [contrib.eval :refer [eval-expr-str! eval-expr-str!+]]
            [taoensso.timbre :as timbre])
  #?(:clj
     (:import [cats.monad.either Left])))


(deftest test-eval-string []
  #?@(:clj
      [(is (= 1 (eval-expr-str! "1")))
       (is (= 2 ((eval-expr-str! "(constantly 2)"))))

       (timbre/with-config {:enabled? false}
         (is (instance? Left (eval-expr-str!+ nil)))
         (is (instance? Left (eval-expr-str!+ ")"))))]))
