(ns contrib.eval
  (:require
   [contrib.try$ :refer [try-either]]))

(def eval-expr-str!
  "Deprecated in cljs, so left unbound on purpose."
  #?(:clj load-string))

(defn eval-expr-str!+ [code-str]
  (try-either (eval-expr-str! code-str)))

