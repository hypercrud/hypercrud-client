(ns hyperfiddle.spec
  (:require [hyperfiddle.spec.parser :as parser]
            [hyperfiddle.spec.serializer :as serializer]
            [clojure.spec.alpha :as s]
            [contrib.data :as data]))

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

(defn spec-keys [spec]
  (set
   (case (:type spec)
     ::keys (:keys spec)
     ::cat  (:names spec))))

(defn arg?
  "State if `attribute` belongs to `spec` :args"
  [spec attr]
  (when-let [args (:args spec)]
    (contains? (spec-keys args) attr)))

(defn names [spec]
  (case (:type spec)
    ::keys (->> spec :children (map :name))
    ::cat  (:names spec)))

(defn- args-spec [fspec]
  (if (qualified-symbol? fspec)
    (when (s/get-spec fspec)
      (args-spec (parse fspec)))
    (if (= ::fn (:type fspec))
      (:args fspec)
      (throw (ex-info "This spec is not a function spec, cannot extract argument spec from it." {:fspec fspec})))))

(defn- no-args-error! [data]
  (throw (ex-info "Couldn't find an `:args` spec for this function, unable to infer argument order" data)))

(defn apply-map
  "Pass args from `m` to `f` in the order `f` expects them, based on `fspec` :args."
  ([f m]
   (apply-map f f m))
  ([f fspec m]
   {:pre [(or (qualified-symbol? fspec)
              (= ::fn (:type fspec)))]}
   (if-let [spec (args-spec fspec)]
     (case (:type spec)
       ::keys (f m)
       ::cat  (if-let [args (seq (names spec))]
                (let [extract-args (apply juxt args)]
                  (apply f (extract-args m)))
                (f)))
     (no-args-error! {:fn f}))))

(defn nil-args
  "Generate a map where keys come from a fn args and all values are nil."
  [fspec]
  (when-let [args (args-spec fspec)]
    (zipmap (names args) (repeat nil))))

(defn sexp
  "Take a `spec` and a `route`, return a function call s-expression `(function args…)`"
  [{:keys [name] :as spec} {:keys [:hyperfiddle.route/fiddle] :as route}]
  (if-let [args (:args spec)]
    (if-let [args (seq (names args))]
      (->> ((apply juxt args) route)
           (cons name))
      (list name))
    (no-args-error! {:hyperfiddle.route/fiddle fiddle})))

(defn read-route
  "Take route call sexp and parse it to a map, giving names to args."
  [sexp]
  (if (seq sexp)
    (let [[sym & argv] sexp]
      (if-let [args (args-spec (symbol sym))]
        (->> (zipmap (names args) argv)
             (data/filter-vals some?)
             (into {:hyperfiddle.route/fiddle (keyword sym)}))
        (no-args-error! {:sym sym})))
    nil))

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

(comment
  (sexp (parse `user.demo.route-state/sub-request)
        {:hyperfiddle.route/fiddle     :user.demo.route-state/sub-requests
         :user.demo.route-state/since :inst
         :user.demo.route-state/school :school})
  (read-route `(user.demo.route-state/school-picklist "foo")))
