(ns hyperfiddle.project
  (:require
    #?(:cljs [contrib.eval-cljs :as eval-cljs])
    [contrib.try$ :refer [try-either]]
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

(defn project-request [rt pid]
  (->EntityRequest
    :hyperfiddle/project
    (runtime/db rt pid (hf/fiddle-dbname (hf/domain rt)))
    [:project/code
     :project/css]))

(defn hydrate-project-record [rt pid local-basis partitions]
  (io/hydrate-one! (hf/io rt) local-basis partitions (project-request rt pid)))

#?(:cljs
   (defn eval-domain-code!+ [code-str]
     (try-either (some->> code-str (eval-cljs/eval-statement-str! 'user.domain)))))
