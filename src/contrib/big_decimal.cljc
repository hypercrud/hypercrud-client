(ns contrib.big-decimal
  "For JVM equivalent, see java.math.BigDecimal"
  (:refer-clojure :exclude [bigdec])
  (:require [clojure.string :as str])
  #?(:clj (:import (java.math BigDecimal))))

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
               (str/replace #"M$" "")
               (str/replace #"\.0+$" ""))]
     (BigDecimal. x)))

#?(:cljs
   (defn editable-repr
     "Given a BigDecimal, return a user-friendly string representation for easy manual editing"
     [x]
     (when x
       (.-value x))))

(defn bigdec? [x] (instance? BigDecimal x))
