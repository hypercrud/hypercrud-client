(ns hyperfiddle.ide.domain
  (:require
    [hyperfiddle.service.resolve :as R]
    [contrib.etc :refer [in-ns?]]
    [contrib.ct :refer [unwrap]]
    [hypercrud.transit]
    [hyperfiddle.api :as hf]
    [hyperfiddle.domain :as domain :refer [#?(:cljs EdnishDomain)]]
    [hyperfiddle.ide.routing :as ide-routing]
    [hyperfiddle.ide.system-fiddle :as ide-system-fiddle]
    [hyperfiddle.route :as route]
    [hyperfiddle.system-fiddle :as system-fiddle]
    [hyperfiddle.database.color :as color]

    [cats.monad.either :as either :refer [either?]]
    [clojure.spec.alpha :as s]
    [contrib.uri :refer [is-uri?]]
    #?(:clj [hyperfiddle.io.datomic.core :as d]))
  #?(:clj
     (:import
       (hyperfiddle.domain EdnishDomain))))


(def user-dbname-prefix "$user.")

(defn either-of-domain? [v]
  (and (either? v)
    (s/valid? domain/spec-ednish-domain (unwrap (constantly nil) v))))

(s/def ::user-domain+ either-of-domain?)
(s/def ::?datomic-client any?)
(s/def ::html-root-id string?)
(s/def ::memoize-cache any?)
(def spec-ide-domain
  ; not finished, partial spec
  (s/keys
    :req-un [::domain/config
             ::domain/basis
             ::domain/fiddle-dbname
             ::domain/databases
             ::domain/environment
             ::domain/home-route]
    :req [::user-domain+]
    :opt-un [::?datomic-client
             ::html-root-id
             ::memoize-cache]))

(defrecord IdeDomain [config basis fiddle-dbname databases environment home-route build ?datomic-client
                      html-root-id memoize-cache]
  hf/Domain
  (basis [domain] basis)
  (type-name [domain] (str *ns* "/" "IdeDomain"))
  (fiddle-dbname [domain] fiddle-dbname)
  (database [domain dbname] (get databases dbname))
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

  (system-fiddle? [domain fiddle-ident]
    (or (in-ns? 'hyperfiddle.ide.schema fiddle-ident)
      (system-fiddle/system-fiddle? fiddle-ident)))

  (hydrate-system-fiddle [domain fiddle-ident]
    (cond
      (in-ns? 'hyperfiddle.ide.schema fiddle-ident) (ide-system-fiddle/hydrate fiddle-ident (::user-dbname->ide domain))
      :or (system-fiddle/hydrate fiddle-ident)))
  #?(:clj (connect [domain dbname] (d/dyna-connect (hf/database domain dbname) ?datomic-client)))

  (memoize [domain f]
    (if-let [f (get @memoize-cache f)]
      f
      (let [ret (clojure.core/memoize f)]
        (swap! memoize-cache assoc f ret)
        ret))))

; overlaps with EdnishDomain
(defrecord IdeEdnishDomain [config basis fiddle-dbname databases environment home-route ?datomic-client memoize-cache]
  hf/Domain
  (basis [domain] basis)
  (type-name [domain] (str *ns* "/" "IdeEdnishDomain"))
  (fiddle-dbname [domain] fiddle-dbname)
  (database [domain dbname] (get databases dbname))
  (databases [domain] databases)
  (environment [domain] environment)
  (url-decode [domain s] (route/url-decode s home-route))
  (url-encode [domain route] (route/url-encode route home-route))
  (api-routes [domain] R/ide-user-routes)
  (system-fiddle? [domain fiddle-ident] (system-fiddle/system-fiddle? fiddle-ident))
  (hydrate-system-fiddle [domain fiddle-ident] (system-fiddle/hydrate fiddle-ident))
  #?(:clj (connect [domain dbname] (d/dyna-connect (hf/database domain dbname) ?datomic-client)))
  (memoize [domain f]
    (if-let [f (get @memoize-cache f)]
      f
      (let [ret (clojure.core/memoize f)]
        (swap! memoize-cache assoc f ret)
        ret))))

(defn build
  [& {:keys [config ?datomic-client basis
             user-databases user-fiddle-dbname user-domain+
             ide-databases ide-fiddle-dbname
             ide-environment ide-home-route]}]
  (map->IdeDomain
    {:config            config
     :basis             basis
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
   {:pre [(instance? EdnishDomain user-domain)
          (s/valid? domain/spec-ednish-domain user-domain)
          (s/valid? (s/nilable :hyperfiddle.domain/databases) ide-databases)]}
   (build
     :config config
     :?datomic-client    (:?datomic-client user-domain)
     :basis              (hf/basis user-domain)
     :user-databases     (hf/databases user-domain)
     :user-fiddle-dbname (hf/fiddle-dbname user-domain)
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

(hypercrud.transit/register-handlers
  IdeDomain
  (str 'hyperfiddle.ide.domain/IdeDomain)
  #(-> (into {} %) (dissoc :?datomic-client :memoize-cache))
  #(-> (into {} %) (assoc :memoize-cache (atom nil)) map->IdeDomain))

(hypercrud.transit/register-handlers
  IdeEdnishDomain
  (str 'hyperfiddle.ide.domain/IdeEdnishDomain)
  #(-> (into {} %) (dissoc :?datomic-client :memoize-cache))
  #(-> (into {} %) (assoc :memoize-cache (atom nil)) map->IdeEdnishDomain))
