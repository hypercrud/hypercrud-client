(ns hyperfiddle.io.datomic.peer
  (:require
    [clojure.string]
    [datomic.api :as d-peer]                                ; Must be provided by app at runtime
    [datascript.parser :as parser]
    [hyperfiddle.io.datomic.core :refer [ConnectionFacade DbFacade]]
    [hyperfiddle.query]                                     ; query helpers
    [taoensso.timbre :as timbre])
  (:import
    (datascript.parser FindColl FindRel)
    (datomic.db Db)
    (datomic.peer Connection LocalConnection)
    (java.net URISyntaxException)
    (java.util.concurrent ExecutionException)))


(extend-protocol ConnectionFacade
  Connection
  (basis [conn] (-> conn d-peer/sync deref d-peer/basis-t))
  (db [conn] (d-peer/db conn))
  (transact [conn arg-map] @(d-peer/transact conn (:tx-data arg-map)))
  (with-db [conn] (d-peer/db conn))

  LocalConnection
  (basis [conn] (-> conn d-peer/sync deref d-peer/basis-t))
  (db [conn] (d-peer/db conn))
  (transact [conn arg-map] @(d-peer/transact conn (:tx-data arg-map)))
  (with-db [conn] (d-peer/db conn)))

(extend-type Connection
  ConnectionFacade
  (db [conn] (datomic.api/db conn))
  #_(transact [conn arg-map] @(datomic.api/transact conn {:tx-data arg-map})))

(extend-type Db
  DbFacade
  (as-of [db time-point] (d-peer/as-of db time-point))
  (basis-t [db] (d-peer/basis-t db))
  (pull [db arg-map] (d-peer/pull db (:selector arg-map) (:eid arg-map)))
  (with [db arg-map] (d-peer/with db (:tx-data arg-map))))

(defn connect
  "if in-mem, create it and call create-fn"
  [uri & [on-created!]]
  (try (let [is-created (and (clojure.string/starts-with? (str uri) "datomic:mem")
                          (datomic.api/create-database (str uri)))
             conn (datomic.api/connect (str uri))]
         (when (and is-created on-created!)
           (on-created! conn))
         conn)

       (catch URISyntaxException e
         ; Illegal character in opaque part at index 30: datomic:free://datomic:4334/as df
         (throw (ex-info (.getMessage e) {:hyperfiddle.io/http-status-code 400})))
       (catch IllegalArgumentException e
         ; :db.error/invalid-db-uri Invalid database URI datomic:free://datomic:4334/?af/
         (throw (ex-info (.getMessage e) {:hyperfiddle.io/http-status-code 400})))
       (catch ExecutionException e
         ; bad host, bad port, or datomic is down
         ; todo validate scheme, host, and port before attempting to connect
         ; - Connection refused  java.net.PlainSocketImpl.socketConnect
         ; - UnknownHostException datomicasdf: unknown error  java.net.Inet6AddressImpl.lookupAllHostAddr (Inet6AddressImpl.java:-2)
         ; - Database is already closed (to disable automatic closing at VM shutdown, add \";DB_CLOSE_ON_EXIT=FALSE\" to the db URL) [90121-171]
         (timbre/error e)
         (throw (ex-info "Service Unavailable" {:hyperfiddle.io/http-status-code 503})))
       (catch RuntimeException e
         (cond
           (re-find #"Could not find [^ ]* in catalog" (.getMessage e))
           (throw (ex-info (.getMessage e) {:hyperfiddle.io/http-status-code 404}))
           :else (do
                   (timbre/error e)
                   (throw e))))
       (catch Exception e
         (timbre/error e)
         (throw e))))

(defn- q-find-type [query]
  (->> (if (map? query) query (parser/query->map query))
       :find datascript.parser/parse-find type))

(defn q [{:keys [query limit offset] :as arg-map}]
  (let [result (d-peer/query (dissoc arg-map :limit :offset))]
    (if (contains? #{FindColl FindRel} (q-find-type query))
      (cond-> result
        ; Datomic queries naturally return vectors (not seqs) and
        ; Hyperfiddle UI relies on Datomic results being associative.
        ; Be careful to preserve the vector type here.
        offset (subvec offset)
        (and limit (not= -1 limit)) (subvec 0 (min limit (count result))))
      result)))
