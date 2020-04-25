(ns hyperfiddle.io.datomic.hydrate-route
  (:require
    [cats.core :refer [alet]]
    [cats.monad.either :as either]
    [cats.labs.promise]
    [contrib.performance :as perf]
    [contrib.reactive :as r]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.browser-request :as browser-request]
    [hypercrud.browser.context :refer [map->Context]]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.datomic.hydrate-requests :as hydrate-requests]
    [hyperfiddle.project :as project]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.schema :as schema]
    [hyperfiddle.state :as state]
    [promesa.core :as p]
    [hyperfiddle.scope :refer [scope]]
    [taoensso.timbre :as timbre]
    [hyperfiddle.domain :as domain]))


(deftype RT [domain db-with-lookup get-secure-db-with+ state-atom ?subject]
  state/State
  (state [rt] state-atom)

  runtime/HF-Runtime
  (domain [rt] domain)
  (request [rt pid request]
    (let [ptm @(r/cursor state-atom [::runtime/partitions pid :ptm])]
      (-> (if (contains? ptm request)
            (get ptm request)
            (let [result (or (if (some-> request :e vector?)
                               (some-> (domain/resolve-fiddle (runtime/domain rt) (-> request :e second))
                                       (assoc :db/id [:def (-> request :e second)])
                                       either/right))
                             (hydrate-requests/hydrate-request domain get-secure-db-with+ request ?subject))
                  ptm (assoc ptm request result)
                  tempid-lookups (hydrate-requests/extract-tempid-lookups db-with-lookup pid)]
              (state/dispatch! rt [:hydrate!-success pid ptm tempid-lookups])
              result))
          (r/atom))))
  (set-route [rt pid route] (state/dispatch! rt [:partition-route pid route])))

(defn hydrate-route [domain local-basis route pid partitions ?subject]
  (let [aux-io (reify io/IO
                 (hydrate-requests [io local-basis partitions requests]
                   (p/do* (hydrate-requests/hydrate-requests domain local-basis requests partitions ?subject))))
        aux-rt (reify runtime/HF-Runtime
                 (io [rt] aux-io)
                 (domain [rt] domain))]
    (alet [schemas (schema/hydrate-schemas aux-rt pid local-basis partitions)
           ; schemas can NEVER short the whole request
           ; if the fiddle-db is broken (duplicate datoms), then attr-renderers and project WILL short it
           attr-renderers (project/hydrate-attr-renderers aux-rt pid local-basis partitions)
           project (project/hydrate-project-record aux-rt pid local-basis partitions)]

      (scope [`hydrate-route (:hyperfiddle.route/fiddle route)]
        (let [db-with-lookup (atom {})
              initial-state {::runtime/user-id    ?subject
                             ; should this be constructed with reducers?
                             ; why dont we need to preheat the tempid lookups here for parent branches?
                             ::runtime/partitions (update partitions pid assoc
                                                    :attr-renderers attr-renderers
                                                    :local-basis local-basis
                                                    :project project ; todo this is needed once total, not once per partition
                                                    :route route
                                                    :schemas schemas)}
              state-atom (r/atom (state/initialize initial-state))
              partitions-f (fn []
                             (->> (::runtime/partitions @state-atom)
                                  (map (fn [[k v]]
                                         [k (select-keys v [:is-branched :partition-children :parent-pid :stage])]))
                                  (into {})))
              get-secure-db-with+ (hydrate-requests/build-get-secure-db-with+ domain partitions-f db-with-lookup local-basis)
              rt (->RT domain db-with-lookup get-secure-db-with+ state-atom ?subject)]

          (perf/time (fn [t] (when (> t 500) (timbre/debugf "hydrate-route hydrate-requests/extract-tempid-lookups %sms" t)))
            ; must d/with at the beginning otherwise tempid reversal breaks
            (do
              (doseq [[pid partition] partitions
                      :when (boolean (:is-branched partition))
                      [dbname _] (:stage partition)]
                (get-secure-db-with+ dbname pid))
              (doseq [pid (keys @db-with-lookup)]
                (swap! state-atom assoc-in [::runtime/partitions pid :tempid-lookups]
                  (hydrate-requests/extract-tempid-lookups db-with-lookup pid)))))

          (perf/time (fn [t] (when (> t 500) (timbre/warnf "browser-request/requests %sms route: %s" t route)))
            (-> (base/browse-partition+ (map->Context {:ident nil :partition-id pid :runtime rt}))
                (either/branch
                  (fn [e] (timbre/warn e))
                  browser-request/requests)))

          (-> @(state/state rt)
              ::runtime/partitions
              (select-keys (runtime/descendant-pids rt pid))
              (->> (filter (fn [[pid p]] (some? (:route p))))
                   (map (fn [[pid partition]]
                          [pid (select-keys partition [:is-branched
                                                       :partition-children
                                                       :parent-pid
                                                       :route :local-basis
                                                       :attr-renderers :error :project :ptm :schemas :tempid-lookups])]))
                   (into {}))))))))
