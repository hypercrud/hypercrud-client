(ns contrib.ct
  (:require [cats.monad.either :as either]
            [cats.monad.maybe :as maybe :refer [just nothing]]))

(defn unwrap [lf v+]
  (either/branch v+ lf identity))

(defn silence! [v+]
  (unwrap (constantly nil) v+))

(defn maybe [v]
  (if (some? v)
    (just v)
    (nothing)))

;(defn maybe-branch [mv lf rf]
;  {:pre [(maybe? mv)]}
;  (if (nothing? mv)
;    (lf)
;    (rf (extract mv))))
;
;(defn maybe->either [err mv]
;  (maybe-branch mv #(left err) right))
