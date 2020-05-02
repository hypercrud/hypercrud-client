(ns hyperfiddle.directory.core
  (:require
    [cats.core :as cats :refer [mlet]]
    [cats.monad.either :as either]
    [contrib.reader :as reader]
    [contrib.uri :refer [is-uri?]]
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hypercrud.types.EntityRequest :refer [->EntityRequest]]
    [hyperfiddle.service.resolve :as R]
    [hyperfiddle.domain :as domain :refer [map->EdnishDomain map->BidiDomain]]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.io.core :as io]
    #?(:clj [hyperfiddle.io.datomic.core :as d])
    [hyperfiddle.route :as route]
    [promesa.core :as p]))


(def database-pull
  [:db/id
   :database/db-name
   :database/uri
   :database.custom-security/client
   :database.custom-security/server
   {:database/write-security [:db/ident]}
   :hyperfiddle/owners])

(def domain-pull
  [:db/id
   :hyperfiddle/owners
   {:domain/databases [:domain.database/name
                       {:domain.database/record database-pull}]
    :domain/fiddle-database database-pull}
   :domain/disable-javascript
   :domain/environment
   :domain/ident
   :domain/router
   :domain/home-route])

(defn fiddle-dbname+ [datomic-record]
  (if-let [fiddledb-dbid (get-in datomic-record [:domain/fiddle-database :db/id])]
    (or (some #(when (= fiddledb-dbid (get-in % [:domain.database/record :db/id]))
                 (either/right (:domain.database/name %)))
              (:domain/databases datomic-record))
        (either/left (ex-info "Invalid :domain/fiddle-database. Must use a :domain/database"
                              (select-keys datomic-record [:domain/ident]))))
    (either/left (ex-info "Invalid :domain/fiddle-database. Missing :db/id"
                          (select-keys datomic-record [:domain/ident :domain/fiddle-database])))))

(defn hydrate-app-domain [?datomic-client io local-basis domain-eid]
  (-> (io/hydrate-one! io local-basis {foundation/root-pid {:is-branched true}}
                       (->EntityRequest domain-eid (->DbRef "$domains" foundation/root-pid) domain-pull))
      (p/then (fn [datomic-record]
                (if (nil? (:db/id datomic-record))
                  (p/rejected (ex-info "Domain not found" {:hyperfiddle.io/http-status-code 404}))
                  (-> (mlet [environment (reader/read-edn-string+ (:domain/environment datomic-record))
                             fiddle-dbname (fiddle-dbname+ datomic-record)
                             :let [partial-domain {:basis (get local-basis "$domains")
                                                   :fiddle-dbname fiddle-dbname
                                                   :databases (->> (:domain/databases datomic-record)
                                                                   (map (juxt :domain.database/name :domain.database/record))
                                                                   (into {}))
                                                   :environment (assoc environment :domain/disable-javascript (:domain/disable-javascript datomic-record))
                                                   :?datomic-client ?datomic-client
                                                   :memoize-cache (atom nil)}]]
                        (if (:domain/router datomic-record)
                          (->> (reader/read-edn-string+ (:domain/router datomic-record))
                               (cats/fmap (fn [router] (map->BidiDomain (assoc partial-domain :router router)))))
                          (->> (reader/read-edn-string+ (:domain/home-route datomic-record))
                               (cats/=<< route/validate-route+)
                               (cats/fmap (fn [home-route] (map->EdnishDomain (assoc partial-domain :home-route home-route)))))))
                      (either/branch p/rejected p/resolved)))))))

; app-domains = #{"bar.com"}
; fqdn = "foo.bar.com" or "myfancyfoo.com"
(defn domain-for-fqdn [?datomic-client io app-domains fqdn]
  (-> (io/sync io #{"$domains"})
      (p/then (fn [local-basis]
                (let [domain-eid (if-let [domain-ident (some #(second (re-find (re-pattern (str "^(.*)\\." % "$")) fqdn)) app-domains)]
                                   [:domain/ident domain-ident]
                                   [:domain/aliases fqdn])]
                  (hydrate-app-domain ?datomic-client io local-basis domain-eid))))))

; Used by hfnet
;(defn domain [config directory-uri-or-db-name ?datomic-client]
;  (reify domain/Domain
;    #?(:clj (connect [domain dbname] (d/dyna-connect (domain/database domain dbname) ?datomic-client)))
;    (databases [domain] {"$domains" (if (is-uri? directory-uri-or-db-name)
;                                      {:database/uri directory-uri-or-db-name}
;                                      {:database/db-name directory-uri-or-db-name})})
;    (environment [domain] {})
;    (api-routes [domain] (R/domain-routes config))))
