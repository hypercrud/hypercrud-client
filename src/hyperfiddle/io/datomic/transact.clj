(ns hyperfiddle.io.datomic.transact
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.service.db :as db]
            [hyperfiddle.transaction :refer [expand-hf-tx]]))

(defn transact! [config subject tx-groups]                  ; this is hf/transact
  (let [tempid-lookups (->> tx-groups
                            (map (fn [[dbname tx]]
                                   (let [$ (->> (db/connect config dbname)
                                                (hf/with-db))]
                                     (binding [hf/*subject* subject
                                               hf/*$* $]
                                       (with-bindings (hf/bindings config)
                                         ; Security can query the database e.g. for attribute whitelist
                                         ; no basis on transacts nor staging areas
                                         [dbname (expand-hf-tx (hf/process-tx $ config dbname subject tx))])))))
                            (doall)                         ; allow any exceptions to fire before transacting anythng
                            (map (fn [[dbname dtx]]
                                   (let [conn (db/connect config dbname)
                                         {:keys [tempids]} (hf/transact conn {:tx-data dtx})]
                                     [dbname tempids])))
                            (into {}))]
    {:tempid->id tempid-lookups}))
