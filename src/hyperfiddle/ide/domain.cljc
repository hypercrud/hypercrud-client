(ns hyperfiddle.ide.domain
  (:require
    [hyperfiddle.service.resolve :as R]
    #?(:clj
       [hyperfiddle.def :as hf-def])
    [contrib.data :refer [in-ns?]]
    [contrib.ct :refer [unwrap]]
    [hypercrud.transit]
    [hyperfiddle.api :as hf]
    [hyperfiddle.domain :as domain :refer [#?(:cljs EdnishDomain)]]
    [hyperfiddle.ide.routing :as ide-routing]
    [hyperfiddle.route :as route]
    [hyperfiddle.ui.db-color :as color]

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

(defn resolve-fiddle [fiddle-ident]
  (when (find-ns 'hyperfiddle.def)
    (@(resolve 'hyperfiddle.def/get-fiddle) fiddle-ident)))


;; TODO: GG: This domain is going to allow routing to :hyperfiddle.ide/edit
(defrecord IdeDomain [config basis fiddle-dbname databases environment home-route build ?datomic-client
                      html-root-id memoize-cache]
  hf/Domain
  (basis [domain] basis)
  (fiddle-dbname [domain] fiddle-dbname)
  (database [domain dbname] (get databases dbname))
  (databases [domain] databases)
  (environment [domain] environment)

  (url-decode [domain s]
    (let [{:keys [::route/fiddle] :as route} (route/url-decode s home-route)] ;; get route
      (cond (in-ns? 'hyperfiddle.ide fiddle)        route ;; check for corner case
            (in-ns? 'hyperfiddle.ide.schema fiddle) route ;; check for other corner case
            :or                                     route))) ;; wrap route in ide route

  (url-encode [domain route]
    (route/url-encode route home-route))

  (api-routes [domain] R/ide-routes)

  (resolve-fiddle [domain fiddle-ident]
    (resolve-fiddle fiddle-ident))
  
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
  (fiddle-dbname [domain] fiddle-dbname)
  (database [domain dbname] (get databases dbname))
  (databases [domain] databases)
  (environment [domain] environment)
  (url-decode [domain s] (route/url-decode s home-route))
  (url-encode [domain route] (route/url-encode route home-route))
  (api-routes [domain] R/ide-user-routes)
  (resolve-fiddle [domain fiddle-ident] (resolve-fiddle fiddle-ident))
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

;; TODO: TECH DEBT: GG: this should be an identity
(defn build-from-user-domain [user-domain & _]
  (assoc user-domain ::user-domain+ (either/right user-domain)))

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
