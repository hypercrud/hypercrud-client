(ns contrib.mursik
  (:require
    [clojure.core.async :refer [go-loop]])
  (:refer-clojure :exclude [map filter constantly repeatedly
                            reduce empty foreach drop take into
                            take-while drop-while]))

(deftype Marker [name]
  Object
  (toString [_] (str "<" name ">")))

(defn marker?
  [any]
  (instance? Marker any))

(def END (->Marker "END"))

(defn end? [any] (identical? END any))

(defprotocol IStream
  (put [this value])
  (poll [this])
  (on-put [this f])
  (finally [this f]))

(deftype Stream
  [^:volatile-mutable queue ^:volatile-mutable on-put ^:volatile-mutable finally]
  Object
  (toString [_] (str "<" (.toString queue) ">"))
  IStream
  (put [_ value]
    (if (end? value)
      (when finally
        (finally))
      (if on-put
        (on-put value)
        (set! queue (conj queue value)))))
  (poll [_] (let [v (peek queue)]
              (set! queue (pop queue))
              v))
  (on-put
    [_ f]
    (set! on-put f)
    (while (not (.isEmpty queue))
      (let [v (peek queue)]
        (set! queue (pop queue))
        (if (end? v)
          (when finally (finally))
          (on-put v)))))
  (finally [_ f]
    (set! finally f)))

(defn ->stream
  []
  (->Stream (clojure.lang.PersistentQueue/EMPTY) nil nil))

(defn pipe
  ([f s0] (pipe f s0 (->stream)))
  ([f s0 s1]
   (on-put s0 (fn [v] (if (marker? v) (put s1 v) (f s1 v))))
   s1))

(defmethod print-method Stream [v ^java.io.Writer w]
  (.write w (.toString v)))

(defn wrap
  [f in]
  (let [out (->stream)]
    (f in out)
    out))

(defn wrapeach
  [f s])

(defn foreach
  [f s & [fnly]]
  (finally s fnly)
  (on-put s f)
  s)

(defn empty
  []
  (let [s (->stream)]
    (put s END)
    s))

(defn single
  [value]
  (put (->stream) value))

(defn sequentially
  [interval vals]
  (let [out (->stream)]
    (go-loop [vals vals]
      (Thread/sleep interval)
      (if-not (empty? vals)
        (do (put out (first vals))
            (recur (rest vals)))
        (put out END)))
    out))

(defn later
  [delay value]
  (sequentially delay [value]))

(defn constantly
  [interval value]
  (sequentially interval (repeat value)))

(defn repeatedly
  [interval generator]
  (sequentially interval (clojure.core/repeatedly generator)))

(defn reduce
  [f base in]
  (let [acc (atom base)]
    (pipe
      (fn [s v]
        (swap! acc #(f % v)))
      in)))

(defn map
  [f in]
  (pipe (fn [s v] (put s (f v))) in))

(defn filter
  [f in]
  (pipe (fn [s v] (when (f v) (put s v))) in))

(defn into
  [s0 s1]
  (pipe put s0 s1))

(defn drop
  [n in]
  (let [dropped (atom 0)]
    (pipe
      (fn [s v]
        (if (>= @dropped n)
          (put s v)
          (swap! dropped inc)))
      in)))

(defn take
  [n in]
  (let [taken (atom 0)]
    (pipe
      (fn [s v]
        (when (< @taken n)
          (swap! taken inc)
          (put s v)))
      in)))

(defn extract
  [s]
  (poll (reduce (fn [acc other] other) nil s)))
