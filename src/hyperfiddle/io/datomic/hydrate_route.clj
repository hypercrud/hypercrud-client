(ns hyperfiddle.io.datomic.hydrate-route
  (:require
    [cats.core :refer [alet]]
    [cats.monad.either :as either]
    [cats.labs.promise]
    [contrib.performance :as perf]
    [contrib.reactive :as r]
    [contrib.do :refer :all]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.browser-request :as browser-request]
    [hypercrud.browser.context :refer [map->Context]]
    [hyperfiddle.api :as hf]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.datomic.hydrate-requests :as hydrate-requests]
    [hyperfiddle.project :as project]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.schema :as schema]
    [hyperfiddle.state :as state]
    [hyperfiddle.def :as hf-def]
    [hyperfiddle.domain :as domain]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))

(deftype RT [domain db-with-lookup get-secure-db-with+ state-atom ?subject]
  hf/State
  (state [rt] state-atom)

  hf/HF-Runtime
  (domain [rt] domain)

  (request [rt pid request]
    (let [ptm @(r/cursor state-atom [::runtime/partitions pid :ptm])]

      (-> (if (contains? ptm request)

            (get ptm request)                               ; cache

            (let [result (hydrate-requests/hydrate-request domain get-secure-db-with+ request ?subject)

                  ptm (assoc ptm request result)

                  tempid-lookups (hydrate-requests/extract-tempid-lookups db-with-lookup pid)]

              (state/dispatch! rt [:hydrate!-success pid ptm tempid-lookups])

              result))
          (r/atom))))

  (set-route [rt pid route] (state/dispatch! rt [:partition-route pid route])))

(defn hydrate-route [domain local-basis route pid partitions ?subject]

  (do-async
    (scope [`hydrate-route (:hyperfiddle.route/fiddle route)]

      (let [aux-io (reify io/IO
                     (hydrate-requests [io local-basis partitions requests]
                       (p/do! (hydrate-requests/hydrate-requests domain local-basis requests partitions ?subject))))
            aux-rt (reify hf/HF-Runtime
                     (io [rt] aux-io)
                     (domain [rt] domain))

            schemas @(schema/hydrate-schemas aux-rt pid local-basis partitions)

            attr-renderers
            (into {} (map (fn [[k v]] {k (:attribute/renderer v)}) (hyperfiddle.def/get-def :attribute)))

            db-with-lookup (atom {})

            initial-state {::runtime/user-id    ?subject
                           ; should this be constructed with reducers?
                           ; why dont we need to preheat the tempid lookups here for parent branches?
                           ::runtime/partitions (update partitions pid assoc
                                                  :attr-renderers attr-renderers
                                                  :local-basis local-basis
                                                  :route route
                                                  :schemas schemas)}
            state-atom (r/atom (state/initialize initial-state))
            partitions-f (fn []
                           (->> (::runtime/partitions @state-atom)
                                (map (fn [[k v]]
                                       [k (select-keys v [:is-branched :partition-children :parent-pid :stage])]))
                                (into {})))
            get-secure-db-with+ (hydrate-requests/build-get-secure-db-with+ domain partitions-f db-with-lookup local-basis ?subject)
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
          (-> (base/browse-partition+ (map->Context {:partition-id pid :runtime rt}))
              (either/branch
                (fn [e] (timbre/warn e))                    ; write to server log, exception has been written to partition map for transmission to client
                browser-request/requests)))

        (-> @(hf/state rt)
            ::runtime/partitions
            (select-keys (runtime/descendant-pids rt pid))
            (->> (filter (fn [[pid p]] (some? (:route p))))
                 (map (fn [[pid partition]]
                        [pid (select-keys partition [:is-branched
                                                     :partition-children
                                                     :parent-pid
                                                     :route :route-defaults :local-basis
                                                     :attr-renderers :error :ptm :schemas :tempid-lookups])]))
                 (into {})))))))
