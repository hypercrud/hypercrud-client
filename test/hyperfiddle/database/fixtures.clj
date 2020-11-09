(ns hyperfiddle.database.fixtures
  (:require
    [clojure.test :refer [join-fixtures]]
    [datomic.api :as d]                                     ; no longer necessary, use hf/connect
    [hyperfiddle.io.datomic.transact :as transact]))

(defn init-domain [config & {:keys [subject schemas init-txs]}]
  (->> (:databases config)
       (map (fn [[dbname {:keys [:database/uri]}]]
              (fn [f]
                (when-not (d/create-database (str uri))
                  (throw (ex-info "Database already exists" {:dbname dbname :uri uri})))
                (try
                  (when-let [schema (get schemas dbname)]
                    (transact/transact! config subject {dbname schema}))
                  (when-let [init-tx (get init-txs dbname)]
                    (transact/transact! config subject {dbname init-tx}))
                  (f)
                  (finally
                    (when-not (d/delete-database (str uri))
                      (throw (ex-info "Database already deleted" {:dbname dbname :uri uri}))))))))
       (join-fixtures)))
