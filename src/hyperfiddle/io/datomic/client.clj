(ns hyperfiddle.io.datomic.client
  (:require
    [contrib.performance :as perf]
    [datomic.client.api :as d-client]
    [datomic.client.impl.shared]
    [hyperfiddle.io.datomic.core :refer [ConnectionFacade DbFacade]]
    [taoensso.timbre :as timbre])
  (:import
    (datomic.client.impl.shared Connection Db)))


(extend-type Connection
  ConnectionFacade
  (basis [conn] (-> conn d-client/db :t))
  (db [conn] (d-client/db conn))
  (transact [conn arg-map] (d-client/transact conn arg-map))
  (with-db [conn] (d-client/with-db conn)))

(extend-type Db
  DbFacade
  (as-of [db time-point] (d-client/as-of db time-point))
  (basis-t [db] (:t db))
  (pull [db arg-map] (d-client/pull db arg-map))
  (with [db arg-map] (d-client/with db arg-map)))

(defn connect [client db-name & [on-created!]]
  (try (perf/time
         (fn [total-time]
           (timbre/debugf "Connecting to %s %sms" db-name total-time))
         (d-client/connect client {:db-name db-name}))
       (catch Exception e
         (timbre/error e)
         (throw e))))

(defn q [arg-map] (d-client/q arg-map))
