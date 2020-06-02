(ns contrib.do
  (:require
    [clojure.core.async :as async]
    [cats.monad.either :as either]
    [promesa.core :as p]
    ;[promesa.async]
    )
  #?(:cljs (:require-macros [contrib.do])))

(defn tag-type [tag]
  (some->> (re-matches #"^([A-Z][-a-z]*)(.*)$" (name tag)) second keyword))

(defn typed-tag? [tag]
  (not (nil? (tag-type tag))))

(defn typed-action? [val]
  (and (vector? val)
       (typed-tag? (first val))))

(defn action-tag [val]
  (assert (vector? val))
  (first val))

(defn action-type [val]
  (assert typed-action? val)
  (tag-type (action-tag val)))

(defn as-either [v]
  (if (either/either? v) v (either/right v)))

(defmacro do-result [& body]
  `(as-either (try ~@body
                   (catch ~(if (:ns &env) 'js/Error 'Exception) e# (either/left e#)))))

(defmacro from-result
  "Map an either into an exception or a value"
  [& body]
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

; ----------------------------

(def ^:dynamic *scope nil)

(defmacro scope [desc & run]
  `(binding [*scope (conj (or *scope []) ~desc)] ~@run))

(defn push-scope [desc]
  (set! *scope (conj (or *scope []) desc)))

(defn with-scope [f]
  (let [s *scope]
    (fn [& args] (binding [*scope s] (apply f args)))))

; ----------------------------

(defprotocol Do-via
  (resolver-for [H]))

(def ^:dynamic *stack [])
(def ^:dynamic *resolve {})
(def ^:dynamic *state)

(defmacro via* [R & body]
  `(let [R# ~R
         fns# (resolver-for R#)]

     (assert (every? typed-tag? (keys fns#)))

     (let [n# (count *stack)
           resolvers#
           (->> fns#
                (group-by (comp tag-type key))
                (reduce-kv (fn [m# k# v#]
                             (assoc m# k# (into {::nth n#} v#))) {}))]

       (binding [*stack (conj *stack R#)
                 *resolve (merge *resolve resolvers#)]

         ~@body
         ))))

(defn ! [& action]
  (let [action (vec action)]

    (assert (typed-action? action))

    (let [R (get *resolve (action-type action))]
      (binding [*state (nth *stack (::nth R))]

        (as-> ((get R (action-tag action)) action) result
          (do (set! *stack (assoc *stack (::nth R) *state))
              result))
        ))))

(defn get-state []
  (or *state
      (last *stack)))
