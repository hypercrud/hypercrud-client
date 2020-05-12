(ns hyperfiddle.io.datomic.sync
  (:refer-clojure :exclude [sync])
  (:require
    [hyperfiddle.api :as hf]
    [taoensso.timbre :refer [debug]]))


(defn sync [domain dbnames]
  (debug "syncing" (pr-str dbnames))
  (->> dbnames
       (map (juxt identity #(-> (hf/connect domain %) hf/basis)))
       (into {})))
