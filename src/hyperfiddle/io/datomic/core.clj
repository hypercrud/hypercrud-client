(ns hyperfiddle.io.datomic.core
  (:require
    [hypercrud.types.DbRef])
  (:import
    (hypercrud.types.DbRef DbRef)
    (java.io FileNotFoundException)))


; TODO Migrate these fns to hyperfiddle.api multimethods

(def client-supported (delay (try (require 'datomic.client.api) true ; todo this is always true
                                  (catch FileNotFoundException e false))))

(def peer-supported (delay (try (require 'datomic.api) true
                                (catch FileNotFoundException e false))))

(defn dyna-client [arg-map]
  (require 'datomic.client.api)
  ((resolve 'datomic.client.api/client) arg-map))

(defn dyna-connect [{:keys [database/uri database/db-name] :as hf-db} ?client & [on-created!]]
  (cond
    (and uri @peer-supported) (do (require 'hyperfiddle.io.datomic.peer)
                                  ((resolve 'hyperfiddle.io.datomic.peer/connect) uri on-created!))
    (and db-name @client-supported ?client) (do (require 'hyperfiddle.io.datomic.client)
                                                ; Clients don't need to auto-provision memory dbs so ignore on-created!
                                                ((resolve 'hyperfiddle.io.datomic.client/connect) ?client db-name))
    (and (nil? uri) (nil? db-name)) (throw (ex-info "Database not well formed, must specify a db-name or uri to connect to" {}))
    (and uri (not @peer-supported)) (throw (ex-info "Unable to resolve datomic peer library on classpath" {:database/uri uri}))
    (and db-name (nil? ?client)) (throw (ex-info "No datomic client provided, cannot connect" {:database/db-name db-name}))
    (and db-name (not @client-supported)) (throw (ex-info "Unable to resolve datomic client library on classpath" {:database/db-name db-name}))
    ))

(defn qf2 "private internal version, TODO unify"
  [{:keys [database/uri database/db-name]}]
  (cond
    (and uri @peer-supported) (do (require 'hyperfiddle.io.datomic.peer)
                                  (resolve 'hyperfiddle.io.datomic.peer/q))
    (and db-name @client-supported) (do (require 'hyperfiddle.io.datomic.client)
                                        (resolve 'hyperfiddle.io.datomic.client/q))
    (and (nil? uri) (nil? db-name)) (throw (ex-info "Database not well formed, must specify a db-name or uri to connect to" {:dbname
                                                                                                                             ; This param was dbname per qf, is that the same?
                                                                                                                             db-name}))
    (and uri (not @peer-supported)) (throw (ex-info "Unable to resolve datomic peer library on classpath" {:database/uri uri}))
    (and db-name (not @client-supported)) (throw (ex-info "Unable to resolve datomic client library on classpath" {:database/db-name db-name}))))

(defn qf
  "Resolve a datomic query function from a Datomic product line (adapted to a consistent interface)
  to query a :domain/database indicated as a hyperfiddle parameter. You can have both client and peer
  databases available, thus you have to look at the :domain/database config to know for sure which Datomic
  product line is needed. This interface is really confusing and needs work"
  [dbs params]
  (let [{:keys [dbname branch]} (some #(when (instance? DbRef %) %) params)] ; TODO lift this sentinel type out?
    (qf2 (get dbs dbname))))
