(ns contrib.match
  (:require
    [contrib.do :refer [! *this via*] :as do]
    [clojure.walk :as walk]))

(declare -matches?)

(deftype Environment
  []
  do/Do-via
  (resolver-for
    [H]
    {:Environment.whole
     (fn [_] *this)
     :Environment.init
     (fn [_]
       (set! *this {}))
     :Environment.contains?
     (fn [[_ n]]
       (contains? *this n))
     :Environment.get
     (fn [[_ n]]
       (get *this n))
     :Environment.set
     (fn [[_ n v]]
       (set! *this (assoc *this n v)))
     :Environment.swap!
     (fn [[_ n f & args]]
       (set! *this (assoc *this n (apply f (get *this n) args))))}))

(def ReturnBoolean
  (reify
    do/Do-via
    (resolver-for
     [H]
     {:Return
      (fn [[_ val]]
        val)
      :Return.join
      (fn [[_ v0 v1]]
        (and v0 v1))})))

(def ReturnEnvironment
  (reify
    do/Do-via
    (resolver-for
     [H]
     {:Return
      (fn [[_ val]]
        (when val
          (! :Environment.whole)))
      :Return.join
      (fn [[_ v0 v1]]
        (if (or (nil? v0) (nil? v1))
          nil
          (merge v0 v1)))})))

(deftype Matcher
  []
  do/Do-via
  (resolver-for
    [H]
    {:Match.map
     (fn [[_ pattern target continuation]]
       (if (empty? pattern)
         (! :Return (empty? target))
         (let [[pk pv] (first pattern)]
           (! :Return.join
            (-matches? pv (get target pk) [(dissoc pattern pk) (dissoc target pk)])
            (apply -matches? continuation)))))

     :Match.vector
      (fn [[_ pattern target continuation]]
        (if (empty? pattern)
          (! :Return (empty? target))
          (if (empty? target)
            (! :Return false)
            (! :Return.join
             (-matches? (peek pattern) (peek target) [(pop pattern) (pop target)])
             (apply -matches? continuation)))))

     :Match.symbol
      (fn [[_ pattern target continuation]]
        (cond
          (= '_ pattern)
          (apply -matches? continuation)

          (! :Environment.contains? pattern)
          (-matches? (! :Environment.get pattern) target continuation)

          :else
          (do
            (! :Environment.set pattern target)
            (apply -matches? continuation))))

     :Match.fn
      (fn [[_ pattern target continuation]]
        (if (apply pattern [target])
          (apply -matches? continuation)
          (! :Return false)))

     :Match.val
      (fn [[_ pattern target continuation]]
        (! :Return.join
          (! :Return (= pattern target))
          (apply -matches? continuation)))}))


(defn match-dispatch
  [v]
  (condp apply [v]
    vector? :Match.vector
    map?    :Match.map
    symbol? :Match.symbol
    fn?     :Match.fn
            :Match.val))

(defn -matches?
  ([] (! :Return true))
  ([pattern target & [continuation]]
   (! (match-dispatch pattern) pattern target continuation)))

(defn --matches?
  [pattern target]
  (via* (Environment.)
    (via* (Matcher.)
      (! :Environment.init)
      (-matches? pattern target))))

(defn matches?
  [pattern target]
  (via* ReturnBoolean
   (--matches? pattern target)))

(defn handle-quotes
  [v]
  (walk/postwalk
     (fn [x]
       (if (and (seq? x)
                (= '(quote clojure.core/unquote) (first (seq x))))
         (second (second x))
         (if (symbol? x)
           (list 'quote x)
           x)))
     v))

(defmacro match
  [expr & branches]
  (let [emit (fn emit [& branches]
               (cond
                 (empty? branches)
                 `(throw (IllegalArgumentException. (str "No matching clause: " ~expr)))

                 (= 1 (count branches))
                 (first branches)

                 :else
                 (let [[form then & branches] branches
                       form (handle-quotes form)
                       then (handle-quotes then)]
                   `(if-let [env# (--matches? ~form ~expr)]
                      (walk/postwalk-replace env# ~then)
                      ~(apply emit branches)))))]

    `(via* ReturnEnvironment
       ~(apply emit branches))))


(defn fizzbuzz
  [n]
  (doseq [n (range 1 n)]
    (println
     (match [(mod n 3) (mod n 5)]
            [0 0] "FizzBuzz"
            [0 _] "Fizz"
            [_ 0] "Buzz"
            [_ _] ~n))))
