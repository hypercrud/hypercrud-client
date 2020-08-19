(ns hyperfiddle.config
  (:require
    [clojure.spec.alpha :as s]
    [contrib.io :refer [get-edn get-resource]]
    [hyperfiddle.api :as hf]
    [hyperfiddle.domain :refer [map->EdnishDomain]]
    [hyperfiddle.io.datomic.core]
    [contrib.do :as do]
    [taoensso.timbre :refer [warn info]]))


(declare get-config)                                        ; convert config files to server config and user domain values
(declare get-domain)

(s/def :hyperfiddle.config/scheme string?)
(s/def :hyperfiddle.config/host string?)
(s/def :hyperfiddle.config/port int?)
(s/def :hyperfiddle.config/public-service-http-port int?)
(s/def :hyperfiddle.config/public-service-http-scheme string?)
(s/def :git/describe string?)
(s/def :auth0/domain (s/and string? (complement clojure.string/blank?)))
(s/def :auth0/client-id (s/and string? (complement clojure.string/blank?)))
(s/def :auth0/client-secret (s/and string? (complement clojure.string/blank?)))
(s/def :hyperfiddle.config/$users-database (s/keys :req [:database/uri]))
(s/def :hyperfiddle.config/auth0 (s/keys :req-un [:auth0/domain :auth0/client-id :auth0/client-secret :hyperfiddle.config/$users-database]))
(s/def :hyperfiddle.config/domain map?)                     ; This is the domain-config, not the final domain, which is validated later

(s/def ::config
  (s/keys
    :req-un [::scheme
             ::host
             ::port
             ::domain]
    :opt-un [::auth0
             ::public-service-http-port
             ::public-service-http-scheme]
    :req [:git/describe]))

(defn get-config [config & [more-config spec-selection]]
  (let [config (cond
                 (map? config) config
                 (string? config) (get-edn config))
        config (merge
                 {:scheme "http" :host "localhost" :port 8080
                  :git/describe "dev"}
                 more-config
                 config)]
    (s/assert (or spec-selection :hyperfiddle.config/config) config)
    config))

(defn get-domain [config & [_ spec-selection]]
  ; should this validate the spec before returning it or is that the call-site job?
  (let [m (:domain config)
        domain (-> m
                 (update :basis #(or % (System/currentTimeMillis)))
                 (update :environment #(or % {}))
                 (update :home-route #(or % `(hyperfiddle.api/index)))
                 (update :fiddle-dbname #(or % "$hyperfiddle")) ; but only if hyperfiddle is listed as a database, TODO
                 (cond-> (:client-config m) (assoc :?datomic-client (hyperfiddle.io.datomic.core/dyna-client (:client-config m))))
                 (dissoc :client-config)
                 (assoc :memoize-cache (atom nil))
                 (assoc :config config)
                 map->EdnishDomain)]
    (s/assert (or spec-selection hyperfiddle.domain/spec-ednish-domain) domain)
    domain))

(defonce warned-in-mem? false)
(defonce provisioned-db? false)

(defn provision-in-mem-dbs [domain]
  (when-not provisioned-db?
    (def provisioned-db? true)

    (doseq [[dbname {:keys [database/uri]}] (hf/databases domain)
            :when (and uri (clojure.string/starts-with? (str uri) "datomic:mem"))]
      (let [conn (hf/connect domain dbname
                   (fn on-created! [conn]
                     (info "Created DB" uri)
                     (when-not warned-in-mem?
                       (def warned-in-mem? true)
                       (warn "This is a temporary DB"))))]

        (when (= dbname (hf/fiddle-dbname domain))
          (hf/transact conn {:tx-data (get-edn (get-resource "schema/fiddle.edn"))})
          (info "Added hyperfiddle schema"))

        (when (= dbname "$users")
          (hf/transact conn {:tx-data (get-edn (get-resource "schema/users.edn"))})
          (info "Added users schema"))))))
