(ns contrib.do
  (:require
    [clojure.core.async :as async]
    [cats.monad.either :as either]
    [promesa.core :as p]
    [contrib.promise :as promise]
    [contrib.try$ :refer [try*]]
    ;[promesa.async]
    )
  #?(:cljs (:require-macros [contrib.do :refer [do-async]]
                            [contrib.try$ :refer [try*]])))

(defn tag-type
  "Extract the type from an action. Action is any keyword that starts with an uppercase letter.
  The type is encoded as the first segment.

  Action: :Eval.set-var
  Type: :Eval"
  [tag]
  (some->> (name tag) (re-matches #"^([A-Z][-a-z]*)(.*)$") second keyword))

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
  `(as-either (try* ~@body
                   (catch :default e# (either/left e#)))))

(defmacro from-result
  "Map an either into an exception or a value"
  [& body]
  `(either/branch (do-result ~@body)
     (fn [e#] (throw e#))
     identity))

(defn as-p [v]
  (p/then v identity))

(defmacro do-async [& body]
  `(as-p (try* (p/resolved ~@body)
              (catch :default e# (p/rejected e#)))))

(defn from-async [v]
  (.join (do-async v)))

(defmacro do-async-as-chan [& body]
  `(let [c# (async/chan)]
     (promise/branch (do-async ~@body)
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

(def ^:dynamic *stack [])                                   ; each via* layer has its own mutable state
(def ^:dynamic *resolve {})                                 ; methods available in this dynamic scope
(def ^:dynamic *this)                                       ; points to the active state record in a ! frame

(defmacro via* [R & body]
  `(let [R# ~R                                              ; R is the user defined state instance, e.g. a defrecord
         fns# (resolver-for R#)]                            ; This is a protocol to allow for user defined state type

     (assert (every? typed-tag? (keys fns#)))               ; An action-type identifies a set of methods available on an object "of that action-type"

     (let [n# (count *stack)
           resolvers#                                       ; methods for an action-type (no inheritance, )
           (->> fns#
                (group-by (comp tag-type key))              ; Override resolver methods as a single unit (no inheritance, via* must provide complete impl). GT said this is an optimization?
                (reduce-kv (fn [m# action-type# methods#]
                             (assoc m# action-type# (into {::nth n#} methods#))) ; ?
                  {}))]

       (binding [*stack (conj *stack R#)                    ; save the state pointer
                 *resolve (merge *resolve resolvers#)]      ; other action types are still available in dynamic scope

         ~@body
         ))))

(defn ! "call methods on object from stack variable"
  [& action]
  (let [action (vec action)]

    (assert (typed-action? action))

    (let [R (get *resolve (action-type action))]
      (binding [*this (nth *stack (::nth R))]               ; for backtracking state, let these unwind

        (as-> ((get R (action-tag action)) action) result   ; for state monad, computation can continue forward by going deeper into this frame with re-entrant !
          (do (set! *stack (assoc *stack (::nth R) *this))  ; *this may be mutated by actions
              result))
        ))))

(defn get-state []
  (or *this
      (last *stack)))                                       ; ?
