(ns hyperfiddle.config
  #?@
   (:clj
    [(:require
      [clojure.spec.alpha :as s]
      [clojure.string :as str]
      [contrib.io :refer [get-edn get-resource]]
      [contrib.uri :refer [->URI]]
      [hyperfiddle.api :as hf]
      [hyperfiddle.io.datomic.core :as dclient]
      [hyperfiddle.route :as route]
      [taoensso.timbre :as log])]
    :cljs
    [(:require
      [clojure.spec.alpha :as s]
      [clojure.string :as str]
      [hyperfiddle.api :as hf]
      [hyperfiddle.route :as route])]))

(def non-empty-string? (every-pred string? (complement str/blank?)))

(s/def ::ident keyword?)
(s/def ::scheme string?)
(s/def ::host string?)
(s/def ::port int?)
(s/def ::public-service-http-port int?)
(s/def ::public-service-http-scheme string?)
(s/def ::home-route (s/spec ::route/route))
(s/def :git/describe string?)
(s/def :auth0/domain non-empty-string?)
(s/def :auth0/client-id non-empty-string?)
(s/def :auth0/client-secret non-empty-string?)
(s/def ::$users-database (s/keys :req [:database/uri]))
(s/def ::auth0 (s/keys :req [:auth0/domain :auth0/client-id :auth0/client-secret]
                       :req-un [::$users-database]))
(s/def ::databases (s/map-of non-empty-string? any?))

(s/def :hf/transaction-operation-whitelist (s/coll-of (s/or :k keyword? :f symbol?))) ; [:school/street-address :sub/phone-confirmed :sub-req/subject]

(s/def ::config
  (s/keys
    :req-un [::ident
             ::scheme
             ::host
             ::port
             ::databases
             ::home-route]
    :opt-un [::auth0
             ::public-service-http-port
             ::public-service-http-scheme]
    :req [:git/describe]))

(def SECURE-CLIENT-KEYS [:scheme :host :port :public-service-http-port :public-service-http-scheme :home-route
                         :git/describe
                         :databases
                         :auth0])

(def SECURE-CLIENT-DATABASES-KEYS [:database/db-name :database/write-security :hf/transaction-operation-whitelist])
(def SECURE-CLIENT-AUTH-KEYS [:auth0/domain :auth0/client-id])

(def default-config {:scheme       "http"
                     :host         "localhost"
                     :port         8080
                     :git/describe "dev"
                     :environment  {}
                     :home-route   `(hyperfiddle.api/index)
                     :databases    {}})

#?(:clj
   (defn build [{:keys [client-config] :as config}]
     (let [config (merge default-config
                         {:basis (System/currentTimeMillis)}
                         (when client-config
                           {:?datomic-client (hyperfiddle.io.datomic.core/dyna-client client-config)})
                         config)]
       (s/assert ::config config)
       config)))

(defn valid-dbname? [config dbname] (contains? (:databases config) dbname))

(defn db [config dbname] (get-in config [:databases dbname]))

#?(:clj
   (defonce ^:private provisioned-db? (atom false)))

#?(:clj
   (defn provision-in-mem-dbs [{:keys [:databases :fiddle-dbname] :as _config}]
     (when-not @provisioned-db?
       (doseq [[dbname {:keys [database/uri] :as db}] databases
               :when                                  (and uri (str/starts-with? (str uri) "datomic:mem"))]
         (let [conn (dclient/dyna-connect db nil (fn on-created! [_conn]
                                                   (log/info "Created DB" uri)
                                                   (log/warn "This is a temporary DB")))]
           (when (= dbname fiddle-dbname)
             (hf/transact conn {:tx-data (get-edn (get-resource "schema/fiddle.edn"))})
             (log/info "Added hyperfiddle schema"))
           (when (= dbname "$users")
             (hf/transact conn {:tx-data (get-edn (get-resource "schema/users.edn"))})
             (log/info "Added users schema"))))
       (reset! provisioned-db? true))))

#?(:clj
   (defn service-uri [{:keys [public-service-http-scheme public-service-domain public-service-http-port]}]
     (-> (str public-service-http-scheme "://" public-service-domain) ; (get-in context [:request :server-name])
         (cond-> (or
                  (and (= public-service-http-scheme "http")  (not= (str public-service-http-port) "80"))
                  (and (= public-service-http-scheme "https") (not= (str public-service-http-port) "443")))
           (str ":" public-service-http-port))
         ->URI)))
