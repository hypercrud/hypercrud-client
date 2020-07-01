(ns hyperfiddle.domain
  (:refer-clojure :exclude [memoize])
  (:require
    [hyperfiddle.api :as hf]
    [hyperfiddle.service.resolve :as R]
    [hyperfiddle.route :as route]
    ;[hyperfiddle.system-fiddle :as system-fiddle]
    [hyperfiddle.ui.db-color :as color]
    [cognitect.transit :as t]
    [hypercrud.transit :as hc-t]
    #?(:clj [hyperfiddle.io.datomic.core :as d])
    [hypercrud.browser.router-bidi :as router-bidi]

    [bidi.bidi :as bidi]
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [clojure.string :as string]
    [contrib.uri :refer [is-uri?]]
    [cats.monad.either :as either]
    [contrib.try$ :refer [try-either]]
    ))


; todo these db specs belong somewhere else
(def dbname-spec (s/and string? #(string/starts-with? % "$")))
(s/def :database/db-name string?)
(s/def :database/uri is-uri?)
(def database-spec (s/keys :opt [:database/db-name
                                 :database/uri
                                 :database/write-security
                                 :hf/transaction-operation-whitelist]))
(s/def :database/write-security (s/keys :req [:db/ident]))  ;  {:db/ident :hyperfiddle.security/tx-operation-whitelist}
(s/def :hf/transaction-operation-whitelist (s/coll-of (s/or :k keyword? :f symbol?))) ; [:school/street-address :sub/phone-confirmed :sub-req/subject]

(s/def ::config map?)                                       ; can validate config too
(s/def ::basis some?)
(s/def ::fiddle-dbname dbname-spec)
(s/def ::databases (s/map-of dbname-spec database-spec))
(s/def ::environment map?)

(defn database-color [domain dbname]
  (color/color-for-name "$")                                ; hack for Rosie, so gray relations don't stand out. Todo rethink color
  #_(or (:database/color (hf/database domain dbname)) (color/color-for-name dbname)))

(defn api-path-for [domain handler & {:as params}]
  (apply bidi/path-for (hf/api-routes domain) handler (apply concat params)))

(defn api-match-path [domain path & {:as options}]
  (apply bidi/match-route (hf/api-routes domain) path (apply concat options)))

(defn valid-dbname? [domain dbname] (some? (hf/database domain dbname)))

(defn valid-dbnames? [domain dbnames] (set/subset? (set dbnames) (set (keys (hf/databases domain)))))

(defn dbname-label [dbname]
  (if (= "$" dbname)
    "$ (default)"
    dbname))

(s/def ::home-route (s/spec :hyperfiddle/route))

(s/def ::domain-core (s/keys :req-un [:hyperfiddle.config/config
                                      ::basis
                                      ::databases
                                      ::environment
                                      ::home-route]))

(s/def ::domain-hyperfiddle (s/and (s/keys :req-un [::fiddle-dbname])
                              (fn [domain]
                                (if-let [{:keys [fiddle-dbname]} domain]
                                  (contains? (:databases domain) fiddle-dbname)))))

(def spec-ednish-domain (s/and ::domain-core ::domain-hyperfiddle))

(defrecord EdnishDomain [config basis fiddle-dbname databases environment home-route ?datomic-client memoize-cache]
  hf/Domain
  (basis [domain] basis)
  (fiddle-dbname [domain] fiddle-dbname)
  (database [domain dbname] (get databases dbname))
  (databases [domain] databases)
  (environment [domain] environment)
  (url-decode [domain s] (route/url-decode s home-route))
  (url-encode [domain route] (route/url-encode route home-route))
  (api-routes [domain] R/domain-routes)
  #?(:clj (connect [domain dbname] (d/dyna-connect (hf/database domain dbname) ?datomic-client)))
  #?(:clj (connect [domain dbname on-created!] (d/dyna-connect (hf/database domain dbname) ?datomic-client on-created!))))

(defrecord BidiDomain [config basis fiddle-dbname databases environment router ?datomic-client memoize-cache]
  hf/Domain
  (basis [domain] basis)
  (fiddle-dbname [domain] fiddle-dbname)
  (database [domain dbname] (get databases dbname))
  (databases [domain] databases)
  (environment [domain] environment)
  (url-decode [domain s]
    (either/branch
      (try-either (router-bidi/decode router s))
      (fn [e] (route/decoding-error e s))
      identity))
  (url-encode [domain route] (router-bidi/encode router route))
  (api-routes [domain] (R/domain-routes config))
  #?(:clj (connect [domain dbname] (d/dyna-connect (hf/database domain dbname) ?datomic-client))))

(hypercrud.transit/register-handlers
  EdnishDomain
  (str 'hyperfiddle.domain/EdnishDomain)
  #(-> (into {} %) (dissoc :?datomic-client :memoize-cache))
  #(-> (into {} %) (assoc :memoize-cache (atom nil)) map->EdnishDomain))

(hypercrud.transit/register-handlers
  BidiDomain
  (str 'hyperfiddle.domain/BidiDomain)
  #(-> (into {} %) (dissoc :?datomic-client :memoize-cache))
  #(-> (into {} %) (assoc :memoize-cache (atom nil)) map->BidiDomain))
