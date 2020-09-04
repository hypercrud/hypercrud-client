(ns contrib.dataflow
  (:require
    [clojure.core :as clojure]
    [clojure.set :as set]
    [contrib.data :refer [contains-in?]]
    #?(:cljs
        ["DF.js" :as df :refer [Origin View]]))
  #?(:clj
     (:import
      haxe.lang.VarArgsBase
      haxe.root.Array
      hyperfiddle.hx.Origin))

  (:refer-clojure :exclude [filter map reduce remove apply
                            constantly * -reduce dedupe sequence]))

(defmulti clj->hx
  (fn [any]
    (cond
      #?@(:clj
          [(ifn? any)
           clojure.lang.IFn]
          :cljs
          [(ifn? any)
           :function])

      :else
      (type any))))

#?(:clj
   (defmethod clj->hx clojure.lang.IFn
     [cljf]
     (proxy [haxe.lang.VarArgsBase]
       [-1 -1]
       (__hx_invokeDynamic
        [args]
        (try
          (clojure/apply cljf (seq args))
          (catch Exception e (.printStackTrace e))))))
   :cljs
   (defmethod clj->hx :function
     [cljf]
     (fn [& args]
       (try
         (clojure/apply cljf (seq args))
         (catch js/Error e
           (println 'ERROR!)
           (println e))))))


(defmethod clj->hx :default
  [any]
  any)

#?(:cljs (set! (.-onError Origin) (clj->hx println)))

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
  (-on [_ f]))

(defprotocol IEndable
  (-end [this]))

(defprotocol IValued
  (-value [this]))

(deftype Output
  [impl value]
  IEndable
  (-end
    [_]
    (.end #?(:clj impl :cljs ^js impl)))
  IValued
  (-value
    [_]
    @value)

  #?@(:clj
      [clojure.lang.IRef
       (deref [this]
              (-value this))
       (getWatches
        [this]
        (.getWatches value))
       (addWatch
        [this key f]
        (.addWatch value key f))
       (removeWatch
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

(declare ->dataflow
         map)

(deftype Dataflow
  [impl value]
  IDataflow
  (-put
    [_ value]
    (.put impl value))
  (-filter
    [_ f]
    (->dataflow (#?(:clj Origin/filter :cljs (.-filter Origin)) impl (clj->hx f)) @value))
  (-reduce
    [_ f init]
    (->dataflow (#?(:clj Origin/reduce :cljs (.-reduce Origin)) impl init (clj->hx f)) @value))
  (-on
    [_ f]
    (when-not (nil? @value)
      (f @value))
    (->Output (#?(:clj Origin/on :cljs (.-on Origin)) impl (clj->hx f)) value))
  IEndable
  (-end
    [_]
    (.end impl))
  IValued
  (-value
    [_]
    @value)

  ; contrib.reactive/IFunctor
  ; (-fmap [this f args]
  ;   (map #(clojure/apply f % args) this))

  #?@(:clj
      [clojure.lang.IRef
       (deref [this]
              (-value this))
       (getWatches
        [this]
        (.getWatches value))
       (addWatch
        [this key f]
        (.addWatch value key f))
       (removeWatch
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

(defn dataflow?
  [any]
  (instance? Dataflow any))

(defn input
  [& [value f]]
  (let [input #?(:clj (Origin/input f) :cljs ((.-input Origin) f))
        df (->dataflow input)]
    (when value
      (-put df value))
    df))

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
  {:pre [f df]}
  (->dataflow
   (#?(:clj Origin/apply :cljs (.-apply Origin))
    (haxe-array (clojure/map #(.-impl #?(:cljs ^js % :clj %)) df))
    (clj->hx f))
   (clojure/apply f (clojure/map #(-value %) df))))

(defn mmap
  [f df]
  (map (fn [value] (clojure/mapv f value)) df))

(defn map-by
  [key-fn coll]
  (into {} (clojure/map (juxt key-fn identity) coll)))

(defn end
  [df]
  (-end df))

(defn end-when
  [pred df]
  (map
   (fn [val]
     (if (pred val)
       (-end df)
       val))
   df))

(defn cursor
  [path df]
  (->> df
       (end-when (complement #(contains-in? % path)))
       (map #(get-in % path))))

(defn reduce
  [f init df]
  (-reduce df f init))

(defn consume
  [f df]
  (-on df f))

(defn split
  [fs df]
  (clojure/mapv #(map % df) fs))

(defn cap
  [df]
  (let [cap (atom nil)]
    (consume (partial reset! cap) df)
    cap))

(defn dedupe
  ([df]
   (dedupe identity df))
  ([by df]
   (let [last (volatile! nil)]
     (->> df
          (remove
           (fn [value] (= (by value) @last)))
          (map
           (fn [value] (vreset! last (by value)) value))))))

(defn nil-safe-f
  [f]
  (fn [first & args]
    (when-not (nil? first)
      (clojure/apply f first args))))

(defn sequence-map
  [df]
  (let [streams (atom {})]
    (map
     (fn [xs]
       (let [removals (set/difference (set (keys @streams)) (set (keys xs)))]
         (when-not (empty? removals)
           (doseq [[k dropped] (select-keys @streams removals)]
             (end dropped))
           (swap! streams (fn [streams] (clojure/apply dissoc streams removals)))))

       (into {}
         (clojure/mapv
           (fn [[k v]]
             [k (or (get @streams k nil)
                    (get (swap! streams assoc k (map #(get % k) df)) k))])
           xs)))
     df)))

(defn sequence
  [df]
  (let [streams (atom [])]
    (map
     (fn [xs]
       (when (< (count xs) (count @streams))
         (doseq [dropped (subvec @streams (count xs))]
           (end dropped))
         (swap! streams subvec 0 (count xs)))

       (clojure/mapv
         (fn [i x]
           (or (nth @streams i nil)
               (nth (swap! streams assoc i (map #(nth % i) df)) i)))
         (range)
         xs))
     df)))

(defn example
  []
  (let [input (input)]
    (->> input
         (map inc)
         (filter even?)
         (consume println))
    (doseq [n (range 1 20)]
      (put input n))))
