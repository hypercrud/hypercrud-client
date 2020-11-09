(ns hyperfiddle.io.datomic.hydrate-route
  (:require
    [cats.monad.either :as either]
    [contrib.do :refer :all]
    [contrib.reactive :as r]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.browser-request :as browser-request]
    [hypercrud.browser.context :refer [map->Context]]
    [hyperfiddle.api :as hf]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.datomic.hydrate-requests :as hydrate-requests]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.schema :as schema]
    [hyperfiddle.state :as state]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))

(deftype RT [domain db-with-lookup get-secure-db-with+ state-atom ?subject]
  hf/State
  (state [rt] state-atom)

  hf/HF-Runtime
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

(defn hydrate-route [config local-basis route pid partitions ?subject]
  (let [aux-io              (reify io/IO
                              (hydrate-requests [io local-basis partitions requests]
                                (p/do! (hydrate-requests/hydrate-requests config local-basis requests partitions ?subject))))
        schemas             @(schema/hydrate-schemas aux-io config pid local-basis partitions)
        db-with-lookup      (atom {})
        initial-state       {::runtime/user-id    ?subject
                             ;; should this be constructed with reducers?
                             ;; why dont we need to preheat the tempid lookups here for parent branches?
                             ::runtime/partitions (update partitions pid assoc
                                                          :local-basis local-basis
                                                          :route route
                                                          :schemas schemas)}
        state-atom          (r/atom (state/initialize initial-state))
        partitions-f        (fn []
                              (->> (::runtime/partitions @state-atom)
                                   (map (fn [[k v]]
                                          [k (select-keys v [:is-branched :partition-children :parent-pid :stage])]))
                                   (into {})))
        get-secure-db-with+ (hydrate-requests/build-get-secure-db-with+ config partitions-f db-with-lookup local-basis ?subject)
        rt                  (->RT config db-with-lookup get-secure-db-with+ state-atom ?subject)]

    ;; must d/with at the beginning otherwise tempid reversal breaks
    (doseq [[pid partition] partitions
            :when           (boolean (:is-branched partition))
            [dbname _]      (:stage partition)]
      (get-secure-db-with+ dbname pid))
    (doseq [pid (keys @db-with-lookup)]
      (swap! state-atom assoc-in [::runtime/partitions pid :tempid-lookups]
             (hydrate-requests/extract-tempid-lookups db-with-lookup pid)))

    (-> (base/browse-partition+ (map->Context {:partition-id pid :runtime rt}))
        (either/branch
         (fn [e] (timbre/warn e))                    ; write to server log, exception has been written to partition map for transmission to client
         browser-request/requests))

    (-> @(hf/state rt)
        ::runtime/partitions
        (select-keys (runtime/descendant-pids rt pid))
        (->> (filter (fn [[_pid p]] (some? (:route p))))
             (map (fn [[pid partition]]
                    [pid (select-keys partition [:is-branched
                                                 :partition-children
                                                 :parent-pid
                                                 :route :route-defaults :local-basis
                                                 :error :ptm :schemas :tempid-lookups])]))
             (into {})))))
