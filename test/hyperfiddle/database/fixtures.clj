(ns hyperfiddle.database.fixtures
  (:require
    [clojure.test :refer [join-fixtures]]
    [datomic.api :as d]                                     ; no longer necessary, use hf/connect
    [hyperfiddle.api :as hf]
    [hyperfiddle.io.datomic.transact :as transact]))


(defn init-domain [domain & {:keys [subject schemas init-txs]}]
  (->> (hf/databases domain)
       (map (fn [[dbname {:keys [:database/uri]}]]
              (fn [f]
                (when-not (d/create-database (str uri))
                  (throw (ex-info "Database already exists" {:dbname dbname :uri uri})))
                (try
                  (when-let [schema (get schemas dbname)]
                    (transact/transact! domain subject {dbname schema}))
                  (when-let [init-tx (get init-txs dbname)]
                    (transact/transact! domain subject {dbname init-tx}))
                  (f)
                  (finally
                    (when-not (d/delete-database (str uri))
                      (throw (ex-info "Database already deleted" {:dbname dbname :uri uri}))))))))
       (join-fixtures)))
