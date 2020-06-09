(ns contrib.match
  (:require
    [contrib.do :refer [! *state via*] :as do]
    [clojure.walk :as walk]))

(declare -matches?)

(deftype Environment
  []
  do/Do-via
  (resolver-for
    [H]
    {:Environment.whole
     (fn [_] *state)
     :Environment.init
     (fn [_]
       (set! *state {}))
     :Environment.contains?
     (fn [[_ n]]
       (contains? *state n))
     :Environment.get
     (fn [[_ n]]
       (get *state n))
     :Environment.set
     (fn [[_ n v]]
       (set! *state (assoc *state n v)))
     :Environment.swap!
     (fn [[_ n f & args]]
       (set! *state (assoc *state n (apply f (get *state n) args))))}))

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

(def match|map
  (reify
    do/Do-via
    (resolver-for
      [H]
      {:Match.match
       (fn [[_ pattern target continuation]]
         (if (empty? pattern)
           (! :Return (empty? target))
           (let [[pk pv] (first pattern)]
             (! :Return.join
              (-matches? pv (get target pk) [(dissoc pattern pk) (dissoc target pk)])
              (apply -matches? continuation)))))})))

(def match|vector
  (reify
    do/Do-via
    (resolver-for
      [H]
      {:Match.match
       (fn [[_ pattern target continuation]]
         (if (empty? pattern)
           (! :Return (empty? target))
           (! :Return.join
            (-matches? (peek pattern) (peek target) [(pop pattern) (pop target)])
            (apply -matches? continuation))))})))

(def match|symbol
  (reify
    do/Do-via
    (resolver-for
      [H]
      {:Match.match
       (fn [[_ pattern target continuation]]
         (cond
           (= '_ pattern)
           (apply -matches? continuation)

           (! :Environment.contains? pattern)
           (-matches? (! :Environment.get pattern) target continuation)

           :else
           (do
             (! :Environment.set pattern target)
             (apply -matches? continuation))))})))

(def match|fn
  (reify
    do/Do-via
    (resolver-for
      [H]
      {:Match.match
       (fn [[_ pattern target continuation]]
         (if (apply pattern [target])
           (apply -matches? continuation)
           (! :Return false)))})))

(def match|val
  (reify
    do/Do-via
    (resolver-for
      [H]
      {:Match.match
       (fn [[_ pattern target continuation]]
         (! :Return.join
           (! :Return (= pattern target))
           (apply -matches? continuation)))})))

(defn match-dispatch
  [v]
  (condp apply [v]
    vector? match|vector
    map?    match|map
    symbol? match|symbol
    fn?     match|fn
            match|val))

(defn -matches?
  ([] (! :Return true))
  ([pattern target & [continuation]]
   (via* (match-dispatch pattern)
         (! :Match.match pattern target continuation))))

(defn --matches?
  [pattern target]
  (via* (Environment.)
    (! :Environment.init)
    (-matches? pattern target)))

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
  [x & branches]
  (let [emit (fn emit [& branches]
               (cond
                 (empty? branches)
                 `(throw (IllegalArgumentException. (str "No matching clause: " ~x)))

                 (= 1 (count branches))
                 (first branches)

                 :else
                 (let [[pred then & branches] branches
                       pred (handle-quotes pred)
                       then (handle-quotes then)]
                   `(if-let [env# (--matches? ~pred ~x)]
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
