(ns hyperfiddle.project
  (:require
    [hyperfiddle.runtime :as runtime]
    [hypercrud.types.EntityRequest :refer [->EntityRequest]]
    [hypercrud.types.QueryRequest :refer [->QueryRequest]]
    [hyperfiddle.api :as hf]
    [hyperfiddle.io.core :as io]
    [promesa.core :as p]))


(defn attrs-request [rt pid]
  (->QueryRequest '[:find ?i ?r :where
                    [?attr :attribute/ident ?i]
                    [?attr :attribute/renderer ?r]]
                  [(runtime/db rt pid (hf/fiddle-dbname (hf/domain rt)))]
                  {:limit -1}))

(defn hydrate-attr-renderers [rt pid local-basis partitions]
  (-> (io/hydrate-one! (hf/io rt) local-basis partitions (attrs-request rt pid))
      (p/then #(into {} %))))

