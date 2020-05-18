(ns hyperfiddle.ide.directory
  (:require
    [cats.core :as cats :refer [mlet return]]
    [cats.monad.either :as either]
    [contrib.reader :as reader]
    [hyperfiddle.directory.core :as directory]
    [hyperfiddle.ide.domain :as ide-domain]
    [hyperfiddle.route :as route]))


(defn- build-user+ [config basis user-domain-record ?datomic-client]
  {:pre [config]}
  ; shitty code duplication because we cant pass our api-routes data structure as props (no regex equality)
  (mlet [environment (reader/read-edn-string+ (:domain/environment user-domain-record))
         fiddle-dbname (directory/fiddle-dbname+ user-domain-record)
         :let [partial-domain {:config config
                               :basis basis
                               :fiddle-dbname fiddle-dbname
                               :databases (->> (:domain/databases user-domain-record)
                                               (map (juxt :domain.database/name :domain.database/record))
                                               (into {}))
                               :environment (assoc environment :domain/disable-javascript (:domain/disable-javascript user-domain-record))
                               :?datomic-client ?datomic-client
                               :memoize-cache (atom nil)}]]
    (->> (reader/read-edn-string+ (:domain/home-route user-domain-record))
         (cats/=<< route/validate-route+)
         (cats/fmap (fn [home-route] (ide-domain/map->IdeEdnishDomain (assoc partial-domain :home-route home-route)))))))

(defn- build+ [config ?datomic-client domains-basis ide-datomic-record user-datomic-record]
  (mlet [environment (reader/read-edn-string+ (:domain/environment ide-datomic-record))
         :let [environment (assoc environment :domain/disable-javascript (:domain/disable-javascript ide-datomic-record))]
         home-route (reader/read-edn-string+ (:domain/home-route ide-datomic-record))
         home-route (route/validate-route+ home-route)
         fiddle-dbname (directory/fiddle-dbname+ ide-datomic-record)
         :let [user-fiddle-dbname (either/branch
                                    (directory/fiddle-dbname+ user-datomic-record)
                                    (constantly nil)        ; user domains can be misconfigured, can just leave ide-$ unbound
                                    identity)]]
    (return
      (ide-domain/build
        :config config
        :?datomic-client ?datomic-client
        :basis domains-basis
        :user-databases (->> (:domain/databases user-datomic-record)
                          (map (juxt :domain.database/name :domain.database/record))
                          (into {}))
        :user-fiddle-dbname user-fiddle-dbname
        :user-domain+ (build-user+ config domains-basis user-datomic-record ?datomic-client)
        :ide-databases (->> (:domain/databases ide-datomic-record)
                         (map (juxt :domain.database/name :domain.database/record))
                         (into {}))
        :ide-fiddle-dbname fiddle-dbname

        :ide-environment environment
        :ide-home-route home-route
        ))))
