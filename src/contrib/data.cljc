(ns contrib.data
  #?(:cljs (:require-macros [contrib.data])))


(defn map-values [f m]
  (->> (map (juxt key (comp f val)) m)
       (into {})))

(defn map-keys [f m]
  (->> (map (juxt (comp f key) val) m)
       (into {})))

(defn group-by-assume-unique [f xs]
  (->> xs
       (map (juxt f identity))
       (into {})))

(defn group-by-unique [f xs]
  (->> xs
       (map (juxt f identity))
       (reduce (fn [acc [k _ :as kv]]
                 (if (contains? acc k)
                   (throw (ex-info "Duplicate key" k))
                   (conj acc kv)))
               {})))

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

(defn tee [g f!] (fn [v] (f! v) (g v)))

(defn kwargs
  "arg format is kwargs first; trailing non-kw args are nil key
      [:a 1 :b 2 'a 'b 'c] => {nil (a b c), :b 2, :a 1}"
  [as]
  (let [[kwargs args] (split-with (comp keyword? first) (partition-all 2 as))
        args (flatten args)]
    (-> (apply hash-map (flatten kwargs))
        (as-> $ (if (seq args) (assoc $ nil args) $)))))

(defn xorxs [xorxs]
  (cond (vector? xorxs) xorxs
        (set? xorxs) xorxs
        (seq? xorxs) xorxs
        (nil? xorxs) nil
        :else-single-value [xorxs] #_"can be a map"))

(defn group-by-pred [f? xs]
  (let [{a true b false} (group-by f? xs)]
    [a b]))

(defn ungroup "this is not quite ungroup but so close!" [m]
  (->> m
       (mapcat (fn [[path links]]
                 (map vector links (repeat path))))))

(defn filter-keys [f? m]
  (->> m (filter (fn [[k v]] (f? k))) (into {})))

#?(:clj
   (defmacro orp
     ([pred] nil)
     ([pred x]
      `(let [or# ~x]
         (when (~pred or#) or#)))
     ([pred x & next]
      `(let [or# ~x]
         (if (~pred or#) or# (orp ~pred ~@next))))))

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
