(ns contrib.big-decimal
  "For JVM equivalent, see java.math.BigDecimal"
  (:refer-clojure :exclude [bigdec format])
  (:require [clojure.string :as str])
  #?(:clj (:import (java.math BigDecimal RoundingMode))))

#?(:cljs
   (deftype BigDecimal [value]
     IPrintWithWriter
     (-pr-writer [_ writer _]
       (write-all writer "#bigdec \"" (str value) "\""))
     Object
     (toString [this]
       (str value "M"))))

(defn bigdec [^String x]
  (let [x (-> (str x)
              (str/replace #"M$" ""))]
    #?(:clj (-> (BigDecimal. x)
                (.stripTrailingZeros))
       :cljs (BigDecimal. x))))

#?(:cljs
   (defn editable-repr
     "Given a BigDecimal, return a user-friendly string representation for easy manual editing"
     [x]
     (when x
       (.-value x))))

(defn bigdec? [x] (instance? BigDecimal x))

(defn to-precision
  "Return a *string* representation at precision `n`, truncating."
  [n ^BigDecimal x]
  #?(:clj (.setScale x n RoundingMode/DOWN)
     :cljs
     (let [[lhs rhs] (-> (.-value x) (str/split #"\."))]
       (bigdec (cond-> lhs
                 (pos? n) (str "." (apply str (take n (concat rhs (repeat "0"))))))))))
