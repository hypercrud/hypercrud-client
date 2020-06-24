(ns contrib.eval
  (:require
    [cats.monad.either :as either]
    [contrib.try$ :refer [try-either]]))

(def eval-expr-str! #?(:clj load-string))

(defn eval-expr-str!+ [code-str]
  (try-either (eval-expr-str! code-str)))

(let [eval-fn (memoize eval-expr-str!+)]
  (defn eval-apply
    "Evaluate a serialized function's code `s` and return a 1-argument function `f x
  where f = eval s`. Print an error and return nil if `s` can't be parsed to a
  function. Print an error if `f x` throws."
    [s]
    (if-not (string? s)
      s
      (either/branch
        (eval-fn s)
        #(constantly (pr-str %))                            ; print the compile error when invoked
        (fn [user-fn]                                       ; eventually invoke this unsafe fn
          #(either/branch
             (try-either (user-fn %))
             str                                            ; print the runtime error when invoked
             identity))))))
