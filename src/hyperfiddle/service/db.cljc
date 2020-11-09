(ns hyperfiddle.service.db
  "A simple abstraction over dyna-connect"
   (:require
    #?(:clj [hyperfiddle.io.datomic.core :as dclient])
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [contrib.uri :refer [is-uri?]]
    [hyperfiddle.config :as config]
    [hyperfiddle.ui.db-color :as color]))

(s/def ::dbname (s/and string? #(str/starts-with? % "$")))
(s/def :database/db-name string?)
(s/def :database/uri is-uri?)
(s/def :database/write-security (s/keys :req [:db/ident]))  ;  {:db/ident :hyperfiddle.security/tx-operation-whitelist}
(s/def ::database (s/keys :opt [:database/db-name
                                :database/uri
                                :database/write-security
                                :hf/transaction-operation-whitelist]))

(s/def ::databases (s/map-of ::dbname ::database))
(s/def ::config (s/keys :req-un [::databases]))


(defn valid-name? [config dbname] (some? (config/db config dbname)))
(defn valid-names? [config dbnames] (set/subset? (set dbnames) (set (keys (:databases config)))))

(defn dbname-label [dbname]
  (if (= "$" dbname)
    "$ (default)"
    dbname))

(defn color [config dbname]
  (color/color-for-name "$")                                ; hack for Rosie, so gray relations don't stand out. Todo rethink color
  #_(or (:database/color (hf/database domain dbname)) (color/color-for-name dbname)))

#?(:clj
   (defn connect
     ([db]
      (s/assert ::database db)
      (dclient/dyna-connect db nil))
     ([config dbname]
      (s/assert ::config config)
      (s/assert ::dbname dbname)
      (connect (config/db config dbname)))))

