(ns hyperfiddle.io.core
  #?(:clj (:refer-clojure :exclude [sync]))
  (:require
    [cats.core :as cats]
    [cats.monad.either :as either]
    [promesa.core :as p]
    [contrib.performance :as perf]
    [hyperfiddle.api :as hf]
    [taoensso.timbre :as timbre]))


(defprotocol IO
  (global-basis [io] "Datomic time basis for a domain (N databases combined).
  Used to guarantee consistency and also for cache-control.")

  (hydrate-requests [io local-basis partitions requests] "A hyperfiddle-route
  resolves into N datomic queries (due to iframes, a system schema query). This
  does the datomic io.
  NOTE: that in the reactive-over-network future, hydrate-request is probably
  the join point of a reaction.")

  (hydrate-route [io local-basis route pid partitions] "Do the queries for a
  hyperfiddle route, respecting iframes")

  (local-basis [io global-basis route] "Datomic time basis for databases visible
  on a single route (e.g. if the page you are looking at does not query the user
  database, we would exclude that basis for tighter caching). An ideal
  local-basis could be computed with advance knowledge of the queries actually
  on the page we could exclude novel datoms that do not impact the visible
  queries. That is is difficult to compute without incremental view maintenance,
  so local-basis has some sort of simple stub for now.")

  (sync [io dbnames] "Get the datomic time-basis for a set of databases.")

  (transact! [io tx-groups] "d/transact for a domain, there are N databases on
  the domain that can be transacted to so the tx is group-by database."))

(defn hydrate-one! [io local-basis partitions request]
  (-> (hydrate-requests io local-basis partitions [request])
      (p/then (fn [{:keys [pulled-trees]}] (either/branch (first pulled-trees) p/rejected p/resolved)))))

; Promise[List[Response]]
(defn hydrate-all-or-nothing! [io local-basis partitions requests]
  (if (empty? requests)
    (p/resolved nil)
    (-> (hydrate-requests io local-basis partitions requests)
        (p/then (fn [{:keys [pulled-trees]}] (either/branch (cats/sequence pulled-trees) p/rejected p/resolved))))))

(defn global-basis-for [io domain]
  (perf/time-promise
    (->> (hf/databases domain) keys set
      (sync io)
      (cats/fmap (fn [user-basis] {:domain {:t (hf/basis domain)
                                            :hash (hash {:fiddle-dbname (hf/fiddle-dbname domain)
                                                         :databases (->> (hf/databases domain)
                                                                      (map (fn [[dbname database]]
                                                                             ; todo switching between peer/client will break this hash
                                                                             [dbname (select-keys database [:database/uri :database/db-name])]))
                                                                      (into {}))
                                                         :environment (hf/environment domain)
                                                         :type-name (str (type domain))})}
                                   :user user-basis})))
    (fn [err total-time]
      (timbre/debugf "global-basis failure; total time: %sms" total-time))
    (fn [success total-time]
      (timbre/debugf "global-basis; total time: %sms" total-time))))

(defn local-basis-for [io global-basis route]
  (:user global-basis))
