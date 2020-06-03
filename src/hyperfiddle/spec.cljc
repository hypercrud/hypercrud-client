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
