(ns contrib.do
  (:require
    [clojure.core.async :as async]
    [cats.monad.either :as either]
    [promesa.core :as p]
    ;[promesa.async]
    ))

(defn as-either [v]
  (if (either/either? v) v (either/right v)))

(defmacro do-result [& body]
  `(as-either (try ~@body
                   (catch Exception e# (either/left e#)))))

(defmacro from-result [& body]
  `(either/branch (do-result ~@body)
     (fn [e#] (throw e#))
     identity))

(defn as-p [v]
  (p/then v identity))

(defmacro do-async [& body]
  `(as-p (p/resolved (try ~@body
                          (catch Error e# (p/rejected e#))))))

#?(:clj
   (do
     (defn from-async [v]
       (.join (do-async v)))))

(defmacro do-async-as-chan [& body]
  `(let [c# (async/chan)]
     (p/branch (do-async ~@body)
       #(async/put! c# %)
       #(async/put! c# %))
     c#))
