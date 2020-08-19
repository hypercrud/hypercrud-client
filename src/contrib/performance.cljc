(ns contrib.performance
  (:refer-clojure :exclude [time])
  #?(:cljs (:require-macros [contrib.performance]))
  (:require
    [clojure.pprint :as pprint]
    [promesa.core :as p]))


(defn total-time-fn-builder []
  #?(:clj  (let [start (System/currentTimeMillis)]
             #(- (System/currentTimeMillis) start))
     :cljs (let [start (system-time)]
             #(-> (- (system-time) start)
                  (.toFixed 0)))))

(defmacro time-promise [p error-fn success-fn]
  `(let [total-time-fn# (contrib.performance/total-time-fn-builder)]
     (-> ~p
         (p/then (fn [resp#]
                   (~success-fn resp# (total-time-fn#))
                   (p/resolved resp#)))
         (p/catch (fn [err#]
                    (~error-fn err# (total-time-fn#))
                    (throw err#))))))

(defmacro time
  "Evaluates expr and prints the time it took. Returns the value of expr."
  [with-time expr]
  `(let [total-time-fn# (contrib.performance/total-time-fn-builder)
         ret# ~expr]
     (~with-time (total-time-fn#))
     ret#))

(defonce timings (atom {}))

(defn reset-all [] (reset! timings {}))

(defn reset [fvar] (swap! timings dissoc (symbol fvar)))

(defn- condense [{:keys [t-list acc]}]
  {:acc (-> acc
            (update :count #(+ (or % 0) (count t-list)))
            (update :sum #(apply + (or % 0) t-list))
            (update :max #(apply max (or % 0) t-list))
            (update :min #(if %
                            (apply min (or % 0) t-list)
                            (apply min t-list))))})

#?(:clj
   (defn- -stats [v]
     (let [acc (:acc (condense v))]
       (assoc acc :average (format "%.3f" (double (/ (:sum acc) (:count acc))))))))

(defn add-timing [v timing]
  (if (< 10000 (count (:t-list v)))
    (condense v)
    (update v :t-list conj timing)))

#?(:clj
   (defn stats-all
     ([] (stats-all :name))
     ([sort-key] (stats-all sort-key (if (= sort-key :name) :asc :desc)))
     ([sort-key dir]
      {:pre [(contains? #{:asc :desc} dir)]}
      (let [key-fn (if (= :name sort-key)
                     first
                     (comp sort-key second))]
        (->> (cond->> (reduce-kv (fn [acc k v]
                                   (assoc acc k (-stats v)))
                                 {}
                                 @timings)
               sort-key (sort-by key-fn)
               (= :desc dir) reverse)
             (map (fn [[k v]] (assoc v :ns (namespace k) :name (name k)))))))))

#?(:clj
   (defn stats-for [fvar] (-stats (get @timings (symbol fvar)))))

(defn print-table [data] (pprint/print-table [:ns :name :count :sum :average :max :min] data))

#?(:clj
   (defn print-stats [& args] (print-table (apply stats-all args))))

#?(:clj
   (defn track [fvar]
     {:pre [(var? fvar)]}
     (let [k (symbol fvar)
           f (var-get fvar)]
       (fn [& args]
         (time (fn [total-time] (swap! timings update k add-timing total-time))
               (apply f args))))))

#?(:clj
   (defn track-ns [sym]
     (doseq [fvar (vals (ns-interns (find-ns sym)))
             :when (fn? (var-get fvar))]
       (alter-var-root fvar (constantly (track fvar))))))

(comment
  (perf/track-ns 'contrib.datomic)
  (perf/track-ns 'hypercrud.browser.base)
  (perf/track-ns 'hypercrud.browser.context)
  (perf/track-ns 'hyperfiddle.data)
  (perf/track-ns 'hypercrud.browser.browser-request)
  ; ... run code ...
  (->> (perf/stats-all :count)
       #_(filter #(and (< 1 (:count %))
                       (< 250 (:sum %))))
       perf/print-table))
