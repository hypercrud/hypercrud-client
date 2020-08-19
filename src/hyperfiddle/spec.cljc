(ns hyperfiddle.spec
  (:require [clojure.spec.alpha :as s]
            [contrib.data :as data]
            [hyperfiddle.spec.parser :as parser]
            [hyperfiddle.spec.serializer :as serializer]))

(def parse parser/parse)
(def defs (comp reverse first serializer/serialize))
(def form (comp second serializer/serialize))

(defn leaf? [node]
  (empty? (:children node)))

(def branch? (complement leaf?))

(defn index
  "Index a spec tree by spec name. Useful for direct access."
  [tree]
  (data/index-by :name (tree-seq branch? :children tree)))

(defn fiddle-spec
  "Extract important info from a ::fn spec so they can be stored in a fiddle."
  [{:keys [type args ret] :as fspec}]
  (if (not= ::fn type)
    (throw (ex-info "A fiddle spec must be built from a function spec." {:type type}))
    {:args args
     :ret  ret}))

(defn spec
  "Get fiddle spec in context, if any"
  [ctx]
  (:fiddle/spec @(:hypercrud.browser/fiddle ctx)))

(defn names [spec]
  (case (:type spec)
    ::keys (->> spec :children (map :name))
    ::cat  (:names spec)
    ::alt  (reduce (fn [acc names] (assoc acc (count names) names))
                   {}
                  (map names (:children spec)))))

(defn spec-keys [spec]
  (case (:type spec)
    ::keys (:keys spec)
    ::cat  (:names spec)
    ::alt  (->> (names spec)
                (mapcat val)
                (distinct))))

(defn arg?
  "State if `attribute` belongs to `spec` :args"
  [spec attr]
  (when-let [args (:args spec)]
    (contains? (set (spec-keys args)) attr)))

(defn args-spec [fspec]
  (if (qualified-symbol? fspec)
    (when (s/get-spec fspec)
      (args-spec (parse fspec)))
    (if (= ::fn (:type fspec))
      (:args fspec)
      (throw (ex-info "This spec is not a function spec, cannot extract argument spec from it." {:fspec fspec})))))

(defn best-match-for
  [arities args]
  (let [arity (count args)]
    (or (get arities arity)
        (reduce (fn [_ [proposed-arity names]]
                  (when (<= proposed-arity arity) ; works because sorted
                    (reduced names)))
                nil
                (sort-by first > arities)))))

(defn- composite?
  "State if a spec defines a collection"
  [x]
  (or (not (leaf? x))
      (#{`map? `set? `vector? `list? `seq?} (:predicate x))))

(defn shape [fspec]
  (when-let [ret (:ret fspec)]
    (when-let [type (:type ret)]
      (cond
        (#{::coll} type) (if (composite? (first (:children ret)))
                           '[:find [(pull $ ?e [*]) ...] :in $ :where [$ ?e]] ; TODO not support yet as ?e doesn't have a source
                           '[:find [?e ...] :in $ :where [$ ?e]])
        (#{::keys} type) '[:find (pull $ ?e [*]) . :in $ :where [$ ?e]]
        :else            '[:find ?e . :in $ :where [$ ?e]] ; TODO not support yet as ?e doesn't have a source
        ))))

