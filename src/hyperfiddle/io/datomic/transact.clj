(ns hyperfiddle.io.datomic.transact
  (:require
    [hyperfiddle.api :as hf]
    [hyperfiddle.transaction :refer [expand-hf-tx]]))


(defn transact! [domain subject tx-groups]                  ; this is hf/transact
  (let [tempid-lookups (->> tx-groups
                            (map (fn [[dbname tx]]
                                   (let [$ (->> (hf/connect domain dbname)
                                                (hf/with-db))]
                                     (binding [hf/*subject* subject
                                               hf/*$* $]
                                       ; Security can query the database e.g. for attribute whitelist
                                       ; no basis on transacts nor staging areas
                                       [dbname (expand-hf-tx (hf/process-tx $ domain dbname subject tx))]))))
                            (doall)                         ; allow any exceptions to fire before transacting anythng
                            (map (fn [[dbname dtx]]
                                   (let [conn (hf/connect domain dbname)
                                         {:keys [tempids]} (hf/transact conn {:tx-data dtx})]
                                     [dbname tempids])))
                            (into {}))]
    {:tempid->id tempid-lookups}))
