(ns contrib.data
  (:refer-clojure :exclude [pmap])
  (:require
   [clojure.string :as str])
  #?(:cljs (:require-macros [contrib.data])))

(defn for-kv "f :: m k v -> m"
  [kvs init f]
  (reduce-kv f init kvs))

(defn map-values [f m]
  (for-kv m m (fn [acc k v] (assoc acc k (f v)))))

(defn map-keys [f m]
  (for-kv m m (fn [acc k v] (assoc acc (f k) v))))

(defn filter-keys [f? m]
  (for-kv m m (fn [acc k _] (if (f? k) acc (dissoc acc k)))))

(defn filter-vals [f? m]
  (for-kv m m (fn [acc k v] (if (f? v) acc (dissoc acc k)))))

(defn keywordize-keys [m]
  (map-keys keyword m))

(defn index-by
  "Take a function `k`, a seq of maps `xs`, and produces a map of {(k x), x}.
  eg. :id, [{:id 1} {:id 2}] => {1 {:id 1}, 2 {:id 2}}"
  [k xs]
  (into {} (comp (filter k)
              (map (juxt k identity)))
        xs))

(defn group-by-assume-unique [f xs]
  (->> xs
       (map (juxt f identity))
       (into {})))

(defn group-by-unique [kf xs]                                ; i think args are flipped, this is more like update-in than map
  ; https://github.com/hyperfiddle/hyperfiddle/issues/298
  ; nil key is legal for sparse entity that pulls to empty.

  ; Legal duplicates happen for entity without identity.
  ; When indexing this is not a problem, because they have equal value and actually should coalesce.
  (persistent!
   (reduce (fn [acc x]
             #_(assert k (str "group-by-unique, nil key: " k))
             #_(assert (not (contains? acc k)) (str "group-by-unique, duplicate key: " k))
             (assoc! acc (kf x) x))
           (transient {})
           xs)))

(defn group-by-unique-ordered "Take care to never update this value, it will lose the ordered type.
  Unused, has linear lookup, needs clj-commons/ordered which needs to be ported to CLJC."
  [f xs]
  (->> xs
       (map (juxt f identity))
       (reduce (fn [acc [k _ :as kv]]
                 #_(if (contains? acc k)
                     (throw (ex-info "Duplicate key" k)))
                 (conj acc kv))
               [])
       flatten
       ; Use https://github.com/clj-commons/ordered instead, array-map has linear lookups
       (apply array-map)))

(defn merge-by [f as bs]
  (->> (merge
         (group-by f as)
         (group-by f bs))
       vals
       (apply concat)))

(defn collect
  "Pour kvs into a map, collecting key collisions"
  [kvs]
  (reduce (fn [acc [k v]]
            (update acc k (fnil conj []) v))
          {} kvs))

(defn dissoc-nils [m]
  (reduce-kv (fn [m k v]
               (if (nil? v)
                 (dissoc m k)                               ; reuse structure
                 m))
             m
             m))

(defn update-existing [m k f & args]
  (if (get m k)
    (apply update m k f args)
    m))

(defn update-in-existing
  [m ks f & args]
  (if (empty? ks)
    (apply f m args)
    (let [[k & ks] ks]
      (if (contains? m k)
        (update m k update-in-existing f args)
        m))))

(defn deep-merge
  [v0 v1]
  (if (and (map? v0) (map? v1))
    (merge-with deep-merge v0 v1)
    v1))

(defn deep-merge-dissoc-nils
  [v0 v1]
  (if (and (map? v0) (map? v1))
    (merge-with deep-merge (dissoc-nils v0) (dissoc-nils v1))
    v1))

(defn take-to
  "Returns a lazy sequence of successive items from coll while (pred item) returns true.
  Unlike take-while, it includes the last item."
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (let [me (first s)]
        (cons me (when (pred me) (take-to pred (rest s))))))))

(defn parse-query-element [q query-element]
  ; return value is symbols, not strings
  (let [last-kw (atom nil)
        f (fn [x]
            (if (keyword? x) (reset! last-kw x))
            @last-kw)]
    (->> (partition-by f q)
         (filter #(= query-element (first %)))
         first
         (drop 1))))

(defn transpose "Define transpose empty matrix to return the matrix unchanged - this is not math"
  [matrix]
  (if (seq matrix) (apply mapv vector matrix)
                   matrix))

(defn zip [as bs]
  ;(transpose [as bs])
  (map vector as bs))

(defn pad
  ([zero coll] (concat coll (repeat zero)))
  ([n zero coll] (take n (pad zero coll))))

(defn with-pad [reducer zero]
  (fn [f & cols]
    (let [n (apply max (map count cols))
          cols (map #(pad n zero %) cols)]
      (apply reducer f cols))))

(def map-pad (partial with-pad map))

;(defn fallback [p v not-found]
;  (if-not (p v) v not-found))

(defn fvor [f default]
  (fn [v & args]
    (or (if v (apply f v args)) default)))

(defn tee [f! g] (fn [v] (f! v) (g v)))

(defmacro tap [f! & body]
  `((tee ~f! identity) ~@body))

(defn kwargs
  "arg format is kwargs first; trailing non-kw args are nil key
      [:a 1 :b 2 'a 'b 'c] => {nil (a b c), :b 2, :a 1}"
  [as]
  (let [[kwargs args] (split-with (comp keyword? first) (partition-all 2 as))
        args (flatten args)]
    (-> (apply hash-map (flatten kwargs))
        (as-> $ (if (seq args) (assoc $ nil args) $)))))

(defn xorxs [xorxs & [zero]]
  (cond (vector? xorxs) xorxs
        (set? xorxs) xorxs
        (seq? xorxs) xorxs
        (nil? xorxs) zero
        :else-single-value-or-map ((fnil conj []) zero xorxs)))

(defn group-by-pred [f? xs]
  (let [{a true b false} (group-by f? xs)]
    [a b]))

(defn ungroup "this is not quite ungroup but so close!" [m]
  (->> m
       (mapcat (fn [[path links]]
                 (map vector links (repeat path))))))

#?(:clj
   (defmacro orp
     "`clojure.core/or` evaluates arguments one by one, returning the first truthy
  one and so leaving the remaining ones unevaluated. `orp` does the same but
  with a custom predicate."
     ([pred] nil)
     ([pred x]
      `(let [or# ~x]
         (when (~pred or#) or#)))
     ([pred x & next]
      `(let [or# ~x]
         (if (~pred or#) or# (orp ~pred ~@next))))))

#?(:clj
   (defmacro orf [y]
     "Like `#(or %1 y)`. Useful with update. Eg. (update {:x nil} :x (orf 1))"
     `(fn [x#] (or x# ~y))))


; https://crossclj.info/ns/net.mikera/core.matrix/0.62.0/clojure.core.matrix.utils.html#_xor
(defn xor "Returns the logical xor of a set of values, considered as booleans."
  ([] false)
  ([x] (boolean x))
  ([x y] (if x (not y) (boolean y)))
  ([x y & more]
   (loop [p (xor x y) ss (seq more)]
     (if ss
       (recur (if (first ss) (not p) p) (next ss))
       p))))

(defmacro cond-let [& clauses]
  (when clauses
    (list 'if-let (first clauses)
          (second clauses)
          (cons `cond-let (next (next clauses))))))

(defn rtrim-coll [f? xs]
  (->> (reverse xs)
       (drop-while f?)
       (reverse)
       (into (empty xs))))

(defn fix-arity [f n]
  (if f
    (fn [& args]
      (apply f (take n args)))))

(defn compare-by-index [ordering]
  (let [index (into {} (map vector ordering (range)))]
    #(compare (index %1) (index %2))))

(defn ancestry-common [as bs]
  (->> (take-while (partial apply =) (zip as bs))
       (map first)))

(defn ancestry-divergence [as bs]
  ; pad bs only, not as
  (->> (drop-while (partial apply =) (map vector as (pad nil bs)))
       (map first)))

(defn assoc-if [m k v]
  (conj m (if v [k v])))

(defn unqualify
  "Strip namespace from keyword, discarding it and return unqualified keyword. Nil-safe.
  (unqualify :db.type/ref) -> :ref"
  [?qualified-kw]
  (if ?qualified-kw
    (keyword (name ?qualified-kw))))

(defn qualify
  "Qualify a keyword with a namespace. If already qualified, replace the namespace. Nil-safe.
  (qualify :db :isComponent) -> :db/isComponent"
  [ns ?k]
  (assert ns)
  (assert (not (namespace ns)))
  (if ?k
    (keyword (name ns) (str (name ?k)))))

(defn in-ns? [ns x]
  (= (str ns) (namespace (keyword x))))

(defn keywordize [s]
  (when s
    (if (keyword? s)
      s
      (if (= \: (first s))
        (keyword (subs s 1))
        (keyword s)))))

(defn first-key [k]
  (cond (ident? k) k
        (seqable? k) (first k)
        :or #?(:cljs (js/Error "IllegalArgumentException")
               :clj  (throw (IllegalArgumentException.)))))

(defn to-keys [v]
  (cond (seqable? v) v
        :or [v]))

(defn tag [xs]
  (keyword (first-key xs)))

(defn trim-str [s]
  (if-let [indent (some-> (re-find #"(\n +)\S" s) second)]
    (str/trim (str/replace s indent "\n"))
    (str/trim s)))

(defmacro seq->>
  "When expr is not an empty seq, threads it into the first form (via ->>),
  and when that result is not an empty seq, through the next etc"
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (and (seqable? ~g) (seq ~g)) (->> ~g ~step) nil))
                   forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

#?(:cljs (def pmap map)
   :clj
   (defn pmap
     "Similar to `clojure.core/pmap`. Will execute `f` on a different thread for each
  value (via `future`). Not lazy, not batched. Usefull if `f` is very expensive
  and coll is small. Meant for parallel application of `f` and not for fast
  collection processing."
     [f coll]
     (cond
       (empty? coll)      ()
       (= 1 (count coll)) (list (f (first coll)))
       :else              (->> coll
                               ;; A transducer would make it sequential. We want
                               ;; to fire all futures at once.
                               (map (fn [x] (future (f x))))
                               (map (fn [x] (deref x)))
                               (doall)))))

(defn distinct-by
  ([f]
   (fn [rf]
     (let [seen (volatile! #{})]
       (fn
         ([] (rf))
         ([result] (rf result))
         ([result input]
          (if (contains? @seen (f input))
            result
            (do (vswap! seen conj (f input))
                (rf result input))))))))
  ([f coll]
   (let [step (fn step [xs seen]
                (lazy-seq
                  ((fn [[x :as xs] seen]
                     (when-let [s (seq xs)]
                       (if (contains? seen (f x))
                         (recur (rest s) seen)
                         (cons x (step (rest s) (conj seen (f x)))))))
                   xs seen)))]
     (step coll #{}))))

(defn zipseq
  "Similar to `merge-with`, but for sequences. Merges two lists `as` and `bs`.
  Returned list will contain `(max (count as) (count bs))` elements. Will call
  `pickf` to decide which elements to keep. Defaults to prioritize values from
  `bs` except if it's `nil` and the corresponding value in `as` is not nil (nil
  punning).

  (zipseq '(1 nil nil 4) '(nil 2 3)) => (1 2 3 4)
  (reduce zipseq '[(1 2 3 4) (:a :b :c) (a b)]) => (a b :c 4)"
  ([]      ())
  ([xs]    xs)
  ([as bs] (zipseq as bs (fn [a b] (or b a))))
  ([as bs pickf]
   (let [[longest shortest] (if (> (count as) (count bs)) [as bs] [bs as])
         tail               (drop (count shortest) longest)]
     (concat (map pickf as bs) tail))))
