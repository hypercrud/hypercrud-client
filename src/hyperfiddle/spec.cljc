(ns hyperfiddle.spec
  (:require [hyperfiddle.spec.parser :as parser]
            [hyperfiddle.spec.serializer :as serializer]
            [clojure.spec.alpha :as s]
            [contrib.data :as data]))

;; Implemented as a record temporarily during the UI transition from datomic
;; schema to spec.
(defrecord Spec [args ret attributes])

;; -----------------------------------------------------------------------------

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
  (not-empty (:children node)))

(def branch? (complement leaf?))

(defn index
  "Index a spec tree by spec name. Useful for direct access."
  [tree]
  (data/index-by :name (tree-seq branch? :children tree)))

(defn fiddle-spec
  "Index a spec `tree` and wrap it as a `Spec` type for further use a schema
  replacement."
  [{:keys [type args ret] :as tree}]
  (if (not= :fn type)
    (throw (ex-info "A fiddle spec must be built from a function spec." {:type type}))
    (map->Spec
     {:args       args
      :ret        ret
      :attributes (index ret)})))

(defn spec
  "Get fiddle spec in context, if any"
  [ctx]
  (:fiddle/spec @(:hypercrud.browser/fiddle ctx)))

(defn spec-keys [spec]
  (set
   (case (:type spec)
     :keys (:keys spec)
     :cat  (:names spec))))

(defn arg?
  "State if `attribute` belongs to `spec` :args"
  [spec attr]
  (when-let [args (:args spec)]
    (contains? (spec-keys args) attr)))

(defn names [spec]
  (case (:type spec)
    :keys (->> spec :children (map :name))
    :cat  (:names spec)))

(defn apply-map
  "Pass args from `m` to `f` in the order `f` expects them, based on `fspec` :args."
  ([f m]
   (apply-map f (parse f) m))
  ([f fspec m]
   {:pre [(or (qualified-symbol? fspec)
              (= :fn (:type fspec)))]}
   (if (qualified-symbol? fspec)
     (apply-map f (parse fspec) m)
     (if-let [spec (:args fspec)]
       (case (:type spec)
         :keys (f m)
         :cat  (let [extract-args (apply juxt (names spec))]
                 (apply f (extract-args m))))
       (throw (ex-info "Couldn't find a spec for this function, unable to infer argument order" {:fn f}))))))


;; {:name user.demo.route-state/sub-requests, :type :fn, :args-seq (:args (clojure.spec.alpha/keys :opt [:user.demo.route-state/since :user.demo.route-state/school]) :ret clojure.core/any? :fn nil), :args {:type :keys, :keys #{:user.demo.route-state/since :user.demo.route-state/school}, :args {:opt [:user.demo.route-state/since :user.demo.route-state/school]}, :children [{:name :user.demo.route-state/since, :type :predicate, :predicate clojure.core/inst?} {:name :user.demo.route-state/school, :type :predicate, :predicate clojure.spec.alpha/nilable}]}, :ret {:type :predicate, :predicate clojure.core/any?}}
