(ns hyperfiddle.io.core
  (:refer-clojure :exclude [sync])
  (:require [cats.core :as cats]
            [cats.monad.either :as either]
            [promesa.core :as p]))

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

(defn global-basis-for [{:keys [io config environement] :as _context}]
  (->> (:databases config) keys set
       (sync io)
       (cats/fmap (fn [user-basis] {:domain {:t (:basis config)
                                            :hash (hash {:fiddle-dbname (:fiddle-dbname config)
                                                         :databases (->> (:databases config)
                                                                         (map (fn [[dbname database]]
                                                                                ;; todo switching between peer/client will break this hash
                                                                                [dbname (select-keys database [:database/uri :database/db-name])]))
                                                                         (into {}))
                                                         :environment environement
                                                         :type-name (str (type config))})}
                                   :user user-basis}))))

(defn local-basis-for [io global-basis route]
  (:user global-basis))
