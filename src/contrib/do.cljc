(ns contrib.do

  (:require
    [clojure.core.async :as async]
    [cats.monad.either :as either]
    [promesa.core :as p]
    ;[promesa.async]
    )
  #?(:cljs (:require-macros [contrib.do])))

(defn as-either [v]
  (if (either/either? v) v (either/right v)))

(defmacro do-result [& body]
  `(as-either (try ~@body
                   (catch ~(if (:ns &env) 'js/Error 'Exception) e# (either/left e#)))))

(defmacro from-result [& body]
  `(either/branch (do-result ~@body)
     (fn [e#] (throw e#))
     identity))

(defn as-p [v]
  (p/then v identity))

(defmacro do-async [& body]
  `(as-p (p/resolved (try ~@body
                          (catch ~(if (:ns &env) 'js/Error 'Exception) e# (p/rejected e#))))))

(defn from-async [v]
  (.join (do-async v)))

(defmacro do-async-as-chan [& body]
  `(let [c# (async/chan)]
     (p/branch (do-async ~@body)
       #(async/put! c# %)
       #(async/put! c# %))
     c#))
