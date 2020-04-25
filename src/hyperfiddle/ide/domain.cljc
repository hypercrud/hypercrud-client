(ns hyperfiddle.ide.domain
  (:require
    [hyperfiddle.service.resolve :as R]
    #?(:clj
       [hyperfiddle.def :as hf-def])
    [contrib.data :refer [in-ns?]]
    [hyperfiddle.domain :as domain :refer [#?(:cljs EdnishDomain)]]
    [hyperfiddle.ide.routing :as ide-routing]
    [hyperfiddle.route :as route]
    [hyperfiddle.ui.db-color :as color]

    [cats.monad.either :as either]
    [clojure.spec.alpha :as s]
    [contrib.uri :refer [is-uri?]]
    #?(:clj [hyperfiddle.io.datomic.core :as d]))
  #?(:clj
     (:import
       (hyperfiddle.domain EdnishDomain))))


(def user-dbname-prefix "$user.")

(defn resolve-fiddle [fiddle-ident]
  #?(:cljs (js/console.trace :?!)
     :clj (hf-def/get-fiddle fiddle-ident)))

(defrecord IdeDomain [basis fiddle-dbname databases environment home-route build ?datomic-client
                      html-root-id memoize-cache]
  domain/Domain
  (basis [domain] basis)
  (fiddle-dbname [domain] fiddle-dbname)
  (databases [domain] databases)
  (environment [domain] environment)

  (url-decode [domain s]
    (let [{:keys [::route/fiddle] :as route} (route/url-decode s home-route)]
      (cond (in-ns? 'hyperfiddle.ide fiddle) route
            (in-ns? 'hyperfiddle.ide.schema fiddle) route
            :or (ide-routing/preview-route->ide-route route))))

  (url-encode [domain route]
    (route/url-encode
      (case (::route/fiddle route)
        :hyperfiddle.ide/edit (ide-routing/ide-route->preview-route route)
        route)
      home-route))

  (api-routes [domain] R/ide-routes)

  (resolve-fiddle [domain fiddle-ident]
    (resolve-fiddle fiddle-ident))

  ;(system-fiddle? [domain fiddle-ident]
  ;  (or (in-ns? 'hyperfiddle.ide.schema fiddle-ident)
  ;    (system-fiddle/system-fiddle? fiddle-ident)))

  ;(hydrate-system-fiddle [domain fiddle-ident]
  ;  (cond
  ;    (in-ns? 'hyperfiddle.ide.schema fiddle-ident) (ide-system-fiddle/hydrate fiddle-ident (::user-dbname->ide domain))
  ;    :or (system-fiddle/hydrate fiddle-ident)))

  #?(:clj (connect [domain dbname] (d/dyna-connect (domain/database domain dbname) ?datomic-client)))

  (memoize [domain f]
    (if-let [f (get @memoize-cache f)]
      f
      (let [ret (clojure.core/memoize f)]
        (swap! memoize-cache assoc f ret)
        ret))))

; overlaps with EdnishDomain
(defrecord IdeEdnishDomain [basis fiddle-dbname databases environment home-route ?datomic-client memoize-cache]
  domain/Domain
  (basis [domain] basis)
  (fiddle-dbname [domain] fiddle-dbname)
  (databases [domain] databases)
  (environment [domain] environment)
  (url-decode [domain s] (route/url-decode s home-route))
  (url-encode [domain route] (route/url-encode route home-route))
  (api-routes [domain] R/ide-user-routes)
  (resolve-fiddle [domain fiddle-ident] (resolve-fiddle fiddle-ident))
  ;(system-fiddle? [domain fiddle-ident] (system-fiddle/system-fiddle? fiddle-ident))
  ;(hydrate-system-fiddle [domain fiddle-ident] (system-fiddle/hydrate fiddle-ident))
  #?(:clj (connect [domain dbname] (d/dyna-connect (domain/database domain dbname) ?datomic-client)))
  (memoize [domain f]
    (if-let [f (get @memoize-cache f)]
      f
      (let [ret (clojure.core/memoize f)]
        (swap! memoize-cache assoc f ret)
        ret))))

(defn build
  [& {:keys [?datomic-client basis
             user-databases user-fiddle-dbname user-domain+
             ide-databases ide-fiddle-dbname
             ide-environment ide-home-route]}]
  (map->IdeDomain
    {:basis             basis
     :fiddle-dbname     ide-fiddle-dbname
     :databases         (let [user-dbs (->> (dissoc user-databases user-fiddle-dbname)
                                         (map (fn [[dbname db]]
                                                [(str user-dbname-prefix dbname)
                                                 (assoc db
                                                   :database/auto-transact false
                                                   :database/color (color/color-for-name dbname))]))
                                         (into {}))
                              ide-dbs (->> ide-databases
                                        (map (fn [[dbname db]]
                                               [dbname
                                                (assoc db
                                                  :database/auto-transact true
                                                  :database/color "#777")]))
                                        (into {}))]
                          (cond-> (into user-dbs ide-dbs)
                            user-fiddle-dbname (assoc "$" (assoc (get user-databases user-fiddle-dbname)
                                                            :database/auto-transact false
                                                            :database/color (color/color-for-name user-fiddle-dbname)))))
     :environment       (or ide-environment {})
     :home-route        (or ide-home-route {::route/fiddle :hyperfiddle.ide/home})
     ::user-dbname->ide (->> user-databases
                          (map (fn [[dbname db]]
                                 [dbname
                                  (if (= user-fiddle-dbname dbname)
                                    "$"
                                    (str user-dbname-prefix dbname))]))
                          (into {}))
     ::user-domain+     user-domain+
     :?datomic-client   ?datomic-client
     :html-root-id      "ide-root"
     :memoize-cache     (atom nil)}))

(defn build-from-user-domain                                ; todo delete
  ([user-domain {:keys [config src-uri ide-environment ide-databases] :as opts}]
   {:pre [(s/valid? domain/spec-ednish user-domain)
          (s/valid? (s/nilable :hyperfiddle.domain/databases) ide-databases)]}
   (build
     :?datomic-client    (:?datomic-client user-domain)
     :basis              (domain/basis user-domain)
     :user-databases     (domain/databases user-domain)
     :user-fiddle-dbname (domain/fiddle-dbname user-domain)
     :user-domain+       (-> user-domain map->IdeEdnishDomain either/right)
     :ide-databases      (into {"$src" (if (is-uri? src-uri)
                                         {:database/uri src-uri}
                                         {:database/db-name src-uri})}
                           ide-databases)
     :ide-fiddle-dbname  "$src"
     :ide-environment    ide-environment
     :ide-home-route     (case (:default-route config)
                           :fiddle-index {::route/fiddle :hyperfiddle.ide/home}
                           nil (:home-route user-domain)))))

(domain/register-handlers
  IdeDomain
  (str 'hyperfiddle.ide.domain/IdeDomain)
  #(-> (into {} %) (dissoc :?datomic-client :memoize-cache))
  #(-> (into {} %) (assoc :memoize-cache (atom nil)) map->IdeDomain))

(domain/register-handlers
  IdeEdnishDomain
  (str 'hyperfiddle.ide.domain/IdeEdnishDomain)
  #(-> (into {} %) (dissoc :?datomic-client :memoize-cache))
  #(-> (into {} %) (assoc :memoize-cache (atom nil)) map->IdeEdnishDomain))