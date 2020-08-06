(ns hyperfiddle.spec
  (:require [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [contrib.data :as data]
            [contrib.orderedmap :refer [ordered-map]]
            [hyperfiddle.spec.parser :as parser]
            [hyperfiddle.spec.serializer :as serializer]))

(defmacro with-local-semantics
  "Rebinds the clojure.spec global registry to a temporary atom. Specs defined in
  body will therefor only be defined locally.
  The immutable local scope inherits the immutable global scope.
  TODO: make it thread-safe."
  [& body]
  `(with-redefs [s/registry-ref (atom (s/registry))]
     ~@body))

(defmacro with-empty-spec
  "Provide an empty local spec context in which no specs are defined. Useful to
  avoid naming conflict from the global registry in test fixtures."
  [& body]
  `(with-redefs [s/registry-ref (atom {})]
     ~@body))

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

(defn- no-args-error! [data]
  (throw (ex-info "Couldn't find an `:args` spec for this function, unable to infer argument order" data)))

(defn best-match-for [names m]
  (->> (sort-by first > names)
       (filter (fn [[arity names]]
                 (let [keyset    (set (keys m))
                       names-set (set names)]
                   (and (set/subset? names-set keyset)
                        (= arity (count (set/intersection keyset names-set)))))))
       (map second)
       (first)))

(comment
  (-> `user.hello-world/submission-master
      (parse)
      (:args)
      (names)
      (best-match-for {:user.hello-world/needle 0
                       :user.hello-world/ignored 0
                       :bar 9
                       }))
  (-> `user.hello-world/submission-master
      (parse)
      (:args)
      (spec-keys)))

(defn positional
  "Extract values from `m` in s/cat order"
  [fspec m]
  {:pre [(or (qualified-symbol? fspec)
             (= ::fn (:type fspec)))]}
  (if-let [spec (args-spec fspec)]
    (if-let [names (seq (names spec))]
      (let [args (case (:type spec)
                   ::cat names
                   ::alt (best-match-for names m))]
        ((apply juxt args) m))
      ())
    (no-args-error! {:fspec fspec})))

(defn max-arity [alt]
  (case (:type alt)
    ::cat alt
    ::alt (->> (:children alt)
               (sort-by :count >)
               (first))))

(defn nil-args
  "Generate a map where keys come from a fn args and all values are nil."
  [spec]
  (case (:type spec)
    ::fn  (nil-args (:args spec))
    ::cat (zipmap (names spec) (repeat nil))
    ::alt (nil-args (max-arity spec))
    nil))

(comment
  (-> `user.hello-world/submission-master
      (parse)
      (sexp {})))

(defn sexp
  "Take a `spec` and a `route`, return a function call s-expression `(function args…)`"
  [{:keys [name args] :as spec} {:keys [:hyperfiddle.route/fiddle] :as route}]
  (if args
    (if-let [names (seq (names (max-arity args)))]
      (cons name ((apply juxt names) route))
      (list name))
    (no-args-error! {:hyperfiddle.route/fiddle fiddle})))

(defn read-route
  "Take route call sexp and parse it to a map, giving names to args."
  [sexp]
  (cond
    (not (list? sexp)) sexp
    (seq sexp)         (let [[sym & argv] sexp]
                         (if-let [args (args-spec (symbol sym))]
                           (let [arg-names (case (:type args)
                                             ::cat (names args)
                                             ::alt (names (max-arity args)))]
                             (->> (zipmap arg-names argv)
                                  (data/filter-vals some?)
                                  (into {:hyperfiddle.route/fiddle (keyword sym)})))
                           (no-args-error! {:sym sym})))
    :else              nil))

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

(defn ordered
  "Order a `map` by a positional `spec`, return an OrderedMap. Drop keys missing
  from `spec`."
  [spec map]
  (let [names    (case (:type spec)
                   ::cat (names spec)
                   ::alt (best-match-for (names spec) map))
        position (-> (map-indexed vector names)
                     (set/map-invert))]
    (->> map
         (data/filter-keys position)
         (sort-by (comp position key))
         (mapcat identity)
         (apply ordered-map))))

(comment
  (sexp (parse `user.demo.route-state/sub-request)
        {:hyperfiddle.route/fiddle     :user.demo.route-state/sub-requests
         :user.demo.route-state/since :inst
         :user.demo.route-state/school :school})
  (read-route `(user.demo.route-state/school-picklist "foo")))
