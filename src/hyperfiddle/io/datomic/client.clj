(ns hyperfiddle.io.datomic.client
  (:require
    [contrib.performance :as perf]
    [datomic.client.api]
    [datomic.client.impl.shared]
    [hyperfiddle.api :as hf]
    [taoensso.timbre :refer [error debugf]])
  (:import
    (datomic.client.impl.shared Connection Db)))


(extend-type Connection
  hf/ConnectionFacade
  (basis [conn] (-> conn datomic.client.api/db :t))
  (db [conn] (datomic.client.api/db conn))
  (transact [conn arg-map] (datomic.client.api/transact conn arg-map))
  (with-db [conn] (datomic.client.api/with-db conn)))

(extend-type Db
  hf/DbFacade
  (as-of [db time-point] (datomic.client.api/as-of db time-point))
  (basis-t [db] (:t db))
  (pull [db arg-map] (datomic.client.api/pull db arg-map))
  (with [db arg-map] (datomic.client.api/with db arg-map)))

(defn connect [client db-name & [on-created!]]
  (try (perf/time
         (fn [total-time]
           (debugf "Connecting to %s %sms" db-name total-time))
         (datomic.client.api/connect client {:db-name db-name}))
       (catch Exception e
         (error e)
         (throw e))))

(defn q [arg-map] (datomic.client.api/q arg-map))
