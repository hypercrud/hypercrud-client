(ns hyperfiddle.ide.service.express-js
  (:require
    [cats.monad.either :as either]
    [clojure.string :as string]
    [contrib.base-64-url-safe :as base64-url-safe]
    [goog.object :as object]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.ide.authenticate]
    [hyperfiddle.ide.directory :as ide-directory]           ; immoral
    [hyperfiddle.ide.domain :as ide-domain :refer [IdeDomain]]
    [hyperfiddle.ide.service.core :as ide-service]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.http-client :as http-client]
    [hyperfiddle.route :as route]
    [hyperfiddle.service.cookie :as cookie]
    [hyperfiddle.service.express-js.middleware :as middleware]
    [hyperfiddle.service.http :refer [handle-route]]
    [hyperfiddle.service.domain :as service-domain]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(defmethod handle-route :hyperfiddle.ide/auth0-redirect [handler config req res]
  (middleware/platform->express-req-handler
    config
    (fn [& {:keys [domain service-uri]}]
      ; todo configure on domain
      (let [io (reify io/IO
                 (hydrate-requests [io local-basis partitions requests]
                   ; todo who is this executed on behalf of? system/root or the end user?
                   (http-client/hydrate-requests! domain service-uri local-basis partitions requests))

                 (sync [io dbnames]
                   ; todo who is this executed on behalf of? system/root or the end user?
                   (http-client/sync! domain service-uri dbnames))

                 (transact! [io tx-groups]
                   ; todo who is this executed on behalf of? system/root or the end user?
                   (http-client/transact! domain service-uri tx-groups)))]
        (-> (hyperfiddle.ide.authenticate/login config domain service-uri io (-> req .-query .-code))
            (p/then (fn [jwt]
                      (doto res
                        (.cookie ide-service/cookie-name jwt (-> (::ide-directory/ide-domain domain)
                                                                 (cookie/jwt-options-express)
                                                                 #_(assoc "expires" expiration)
                                                                 clj->js))
                        (.redirect (-> req .-query .-state base64-url-safe/decode)))))
            (p/catch (fn [e] (doto res (.status 500) (.send (pr-str e))))))))
    req res))

(defn ide-routing [domain config req res]
  (let [path (.-path req)
        request-method (keyword (.toLowerCase (.-method req)))
        {:keys [handler route-params]} (domain/api-match-path domain path :request-method request-method)]
    (timbre/info "ide-router:" (pr-str handler) (pr-str request-method) (pr-str path))
    (cond
      (= (some-> handler namespace) "user")
      (either/branch
        (::ide-domain/user-domain+ (object/get req "domain"))
        (fn [e]
          (timbre/error e)
          (throw e))
        (fn [user-domain]
          (doto req
            (object/remove "jwt")                           ; this is brittle
            (object/set "domain" user-domain)
            (object/set "route-params" route-params))
          (handle-route (keyword (name handler)) config req res)))

      (and (= :ssr handler)
           (not (nil? (-> config :auth0 :domain)))             ; if there is an auth0 config, require logins
           (nil? (object/get req "user-id"))
           (not (string/starts-with? path "/:hyperfiddle.ide!please-login/")))
      ; todo this logic should be injected into demo domain record
      (let [inner-route (domain/url-decode domain path)
            url (domain/url-encode domain {::route/fiddle :hyperfiddle.ide/please-login
                                           ::route/datomic-args [inner-route]})]
        (.redirect res url))
      :else (do
              (object/set req "route-params" route-params)
              (handle-route handler config req res)))))

(defmethod service-domain/route IdeDomain [domain config req res]
  (let [#_#_{:keys [cookie-name jwt-secret jwt-issuer]} (-> (get-in context [:request :domain])
                                                            domain/environment-secure :jwt)
        cookie-domain (::ide-directory/ide-domain domain)
        mw (middleware/with-user-id ide-service/cookie-name
                                    cookie-domain
                                    (-> config :auth0 :client-secret)
                                    (-> config :auth0 :domain))
        next (fn [] (ide-routing domain config req res))]
    (mw req res next)))
