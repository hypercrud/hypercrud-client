(ns hxes.dataflow
  (:require
    [clojure.core :as clojure]
    #?(:cljs
        ["/hxes/hyperfiddle/hx/DF.js" :as df :refer [Origin View]]))
  #?(:clj
     (:import
      haxe.root.Array
      hyperfiddle.hx.Origin))

  (:refer-clojure :exclude [filter map reduce remove apply constantly * -reduce]))

(defmulti clj->hx
  (fn [any]
    (cond
      #?@(:clj
          [(fn? any)
           clojure.lang.Fn])

      :else
      (type any))))

#?(:clj
   (defmethod clj->hx clojure.lang.Fn
     [cljf]
     (proxy [haxe.lang.VarArgsBase]
       [-1 -1]
       (__hx_invokeDynamic
        [args]
        (clojure/apply cljf (seq args))))))

(defmethod clj->hx :default
  [any]
  any)

(defn haxe-array
  [seq]
  #?(:cljs
     (object-array seq)
     :clj
     (let [haxe-array (haxe.root.Array.)]
       (doseq [s seq]
         (.push haxe-array s))
       haxe-array)))

(defprotocol IDataflow
  (-put [_ value])
  (-filter [_ f])
  (-reduce [_ f init])
  (-on [_ f])
  (-value [_]))

(declare ->dataflow)

(deftype Dataflow
  [impl value]
  IDataflow
  (-put
    [_ value]
    (.put impl value))
  (-filter
    [_ f]
    (->dataflow ((.-filter Origin) impl (clj->hx f)) value))
  (-reduce
    [_ f init]
    (->dataflow ((.-reduce Origin) (clj->hx f) init impl) value))
  (-on
    [_ f]
    ((.-on Origin) impl (clj->hx f)))
  (-value
    [_]
    @value)

  #?@(:clj
      [clojure.lang.IRef
       (deref [this]
              (-value this))
       (getWatches
        [this oldval newval]
        (.getWatches value))
       (-add-watch
        [this key f]
        (.addWatch value key f))
       (-remove-watch
        [this key]
        (.addWatch value key))]
      :cljs
      [IDeref
       (-deref [this]
               (-value this))])

  #?@(:cljs
      [IWatchable
       (-notify-watches
        [this oldval newval]
        (-notify-watches value oldval newval))
       (-add-watch
        [this key f]
        (-add-watch value key f))
       (-remove-watch
        [this key]
        (-remove-watch value key))]))


(defn ->dataflow
  [impl & [value]]
  (assert impl)
  (let [value (atom value)
        df (Dataflow. impl value)]
    (-on df
         (fn [new-value]
           (reset! value new-value)))
    df))

(defn input
  [& [f]]
  (->dataflow ((.-input Origin) f)))

(defn put
  [input v]
  (-put input v))

(defn filter
  [f df]
  (-filter df f))

(defn remove
  [f df]
  (filter (complement f) df))

(defn map
  [f & df]
  (->dataflow
   ((.-apply Origin)
    (haxe-array (clojure/map #(.-impl %) df))
    (clj->hx f))
   (clojure/apply f (clojure/map #(-value %) df))))

(defn mmap
  [f df]
  (map (fn [coll] (doall (clojure/map (partial map f) coll))) df))

(defn map-by
  [key-fn coll]
  (into {} (clojure/map (juxt key-fn identity) coll)))

(defn *
  [key-fn f df]
  (let [streams (atom {})
        df (map #(map-by key-fn %) df)]
    (->> df
         (map
           (fn [vals]
             (clojure/map
               (fn [[key row]]
                 (when-not (contains? @streams key)
                   (swap! streams assoc key (map #(get % key) df))
                   (f (get @streams key))))
               vals))))))

(defn reduce
  [f init df]
  (-reduce df f init))

(defn consume
  [f df]
  (-on df f))

(defn example
  []
  (let [input (input)]
    (->> input
         (map inc)
         (filter even?)
         (consume println))
    (doseq [n (range 1 20)]
      (put input n))))
