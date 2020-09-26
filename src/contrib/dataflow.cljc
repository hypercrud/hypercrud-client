(ns contrib.dataflow
  (:require
    [cats.protocols :as p]
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


(declare ->dataflow
         map)

(defprotocol IInput
  (-put [this val]))

(defprotocol IView
  (-filter [_ f])
  (-reduce [_ f init])
  (-on [_ f]))

(defprotocol IDataflow
  (-end [this])
  (-value [this]))

(declare context)

(deftype Output
  [impl ^:volatile-mutable meta]
  IDataflow
  (-end
    [_]
    (.end #?(:clj impl :cljs ^js impl)))
  (-value
    [_]
    (.. #?(:clj impl :cljs ^js impl) -node -last))

  #?@(:clj  [clojure.lang.IObj
             (withMeta   [this m] (set! meta m) this)]
      :cljs [IObj
             (-with-meta [this m] (set! meta m) this)])

  #?@(:clj  [clojure.lang.IMeta
             (meta  [_] meta)]
      :cljs [IMeta
             (-meta [_] meta)])

  #?@(:clj
      [clojure.lang.IRef
       (deref [this]
              (-value this))
       (getWatches
        [this])
        ; (.getWatches value))
       (addWatch
        [this key f])
        ; (.addWatch value key f))
       (removeWatch
        [this key])]
        ; (.addWatch value key))]
      :cljs
      [IDeref
       (-deref [this]
               (-value this))]))

(deftype View
  [impl ^:volatile-mutable meta]
  IView
  (-filter
    [this f]
    (->dataflow (#?(:clj Origin/filter :cljs (.-filter Origin)) impl (clj->hx f)) (-value this)))
  (-reduce
    [this f init]
    (->dataflow (#?(:clj Origin/reduce :cljs (.-reduce Origin)) impl init (clj->hx f)) (-value this)))
  (-on
    [this f]
    (when-not (nil? (-value this))
      (f (-value this)))
    (->Output (#?(:clj Origin/on :cljs (.-on Origin)) impl (clj->hx f)) nil))

  IDataflow
  (-end
    [_]
    (.end impl))
  (-value
    [_]
    (.. #?(:clj impl :cljs ^js impl) -node -last))

  #?@(:clj  [clojure.lang.IObj
             (withMeta   [this m] (set! meta m) this)]
      :cljs [IObj
             (-with-meta [this m] (set! meta m) this)])

  #?@(:clj  [clojure.lang.IMeta
             (meta  [_] meta)]
      :cljs [IMeta
             (-meta [_] meta)])

  #?@(:clj
      [clojure.lang.IRef
       (deref [this]
              (-value this))
       (getWatches
        [this])
        ; (.getWatches value))
       (addWatch
        [this key f])
        ; (.addWatch value key f))
       (removeWatch
        [this key])]
        ; (.addWatch value key))]
      :cljs
      [IDeref
       (-deref [this]
               (-value this))]))

(deftype Input
  [impl ^:volatile-mutable meta]
  IInput
  (-put [this val] (.put impl val))
  IView
  (-filter
    [this f]
    (->dataflow (#?(:clj Origin/filter :cljs (.-filter Origin)) impl (clj->hx f)) (-value this)))
  (-reduce
    [this f init]
    (->dataflow (#?(:clj Origin/reduce :cljs (.-reduce Origin)) impl init (clj->hx f)) (-value this)))
  (-on
    [this f]
    (when-not (nil? (-value this))
      (f (-value this)))
    (->Output (#?(:clj Origin/on :cljs (.-on Origin)) impl (clj->hx f)) nil))

  IDataflow
  (-end
    [_]
    (.end impl))
  (-value
    [_]
    (.. #?(:clj impl :cljs ^js impl) -node -last))

  #?@(:clj  [clojure.lang.IFn
             (invoke [this v] (-put this v))]
      :cljs [IFn
             (-invoke [this v] (-put this v))])

  #?@(:clj  [clojure.lang.IObj
             (withMeta   [this m] (set! meta m) this)]
      :cljs [IObj
             (-with-meta [this m] (set! meta m) this)])

  #?@(:clj  [clojure.lang.IMeta
             (meta  [_] meta)]
      :cljs [IMeta
             (-meta [_] meta)])

  #?@(:clj
      [clojure.lang.IRef
       (deref [this]
              (-value this))
       (getWatches
        [this])
        ; (.getWatches value))
       (addWatch
        [this key f])
        ; (.addWatch value key f))
       (removeWatch
        [this key])]
        ; (.addWatch value key))]
      :cljs
      [IDeref
       (-deref [this]
               (-value this))]))


(defn ->dataflow
  [impl & [value]]
  (assert impl)
  (let [df (View. impl nil)]
    (set! (.. #?(:clj impl :cljs ^js impl) -node -val) value)
    (set! (.. #?(:clj impl :cljs ^js impl) -node -last) value)
    df))

(defn dataflow?
  [any]
  (or (instance? View any)
      (instance? Output any)
      (instance? Input any)))

(defn input
  [& [value f]]
  (let [input #?(:clj (Origin/input f) :cljs ((.-input Origin) f))
        df (Input. input nil)]
    (when value
      (-put df value))
    df))

(defn pure
  [v]
  (->dataflow #?(:clj (Origin/pure v) :cljs ((.-pure Origin) v)) v))

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
   (try

     (clojure/apply f (clojure/map -value df))
     (catch #?(:clj Exception :cljs js/Error) e
       (println (f (-value (first df))))))))

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


; --- Frame 0
; <[1   2   3   4]>
;   \   \   \   \
;  <<1> <2> <3> <4>>
;   \   \   \   \
;   *   *   *   *

; (df/put stream [1 2 3])
; --- Frame 1
; <[1   2   3]>
;   \   \   \   \
;  <<1> <2> <3> <4>>
;   \   \   \   \
;   *   *   *   *



; [input]        -> [map map map map] ->           [output]
;                            | |
                       ; -> [map map]
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

(def ^{:no-doc true}
  context
  (reify
    p/Context
    ; p/Semigroup
    ; (-mappend [ctx mv mv']
    ;   (cond
    ;     (nothing? mv) mv'
    ;     (nothing? mv') mv
    ;     :else (just (let [mv (p/-extract mv)
    ;                       mv' (p/-extract mv')]
    ;                   (p/-mappend (p/-get-context mv) mv mv')))))
    ;
    ; p/Monoid
    ; (-mempty [_]
    ;   (df/input))

    p/Functor
    (-fmap [_ f mv]
      (map f mv))

    p/Applicative
    (-pure [_ v]
      (pure v))
    (-fapply [m af av]
      (map (fn [f v] (clojure/apply f v)) af av))
    ;
    p/Monad
    (-mreturn [_ v])
    (-mbind [_ mv f]
      (consume f mv))
    ;
    ; p/MonadZero
    ; (-mzero [_]
    ;   (nothing))
    ;
    ; p/MonadPlus
    ; (-mplus [_ mv mv']
    ;   (if (just? mv)
    ;     mv
    ;     mv'))
    ;
    ; p/Foldable
    ; (-foldl [_ f z mv]
    ;   (if (just? mv)
    ;     (f z (p/-extract mv))
    ;     z))
    ;
    ; (-foldr [_ f z mv]
    ;   (if (just? mv)
    ;     (f (p/-extract mv) z)
    ;     z))
    ;
    ; p/Traversable
    ; (-traverse [_ f mv]
    ;   (if (just? mv)
    ;     (let [a (f (p/-extract mv))]
    ;       (p/-fmap (p/-get-context a) just a))
    ;     (p/-pure (ctx/infer) mv)))

    p/Printable
    (-repr [_]
      "<Dataflow>")))

(defn example
  []
  (let [input (input)]
    (->> input
         (map inc)
         (filter even?)
         (consume println))
    (doseq [n (range 1 20)]
      (put input n))))
