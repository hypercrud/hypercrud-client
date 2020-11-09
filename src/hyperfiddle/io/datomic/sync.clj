(ns hyperfiddle.io.datomic.sync
  (:refer-clojure :exclude [sync])
  (:require [hyperfiddle.api :as hf]
            [hyperfiddle.service.db :as db]
            [taoensso.timbre :as timbre]))

(defn sync [config dbnames]
  (timbre/debug "syncing" (pr-str dbnames))
  (into {}
        ;; hf/basis is for ConnectionFacade
        (map (juxt identity #(hf/basis (db/connect config %))))
        dbnames))
