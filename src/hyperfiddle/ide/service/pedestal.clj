(ns hyperfiddle.ide.service.pedestal
  (:refer-clojure :exclude [sync])
  (:require
    [clojure.string :as str]
    [contrib.base-64-url-safe :as base64-url-safe]
    [contrib.data :refer [unqualify]]
    [contrib.do :refer :all]
    [hyperfiddle.api :as hf]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.ide.authenticate]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.datomic.hydrate-requests :refer [hydrate-requests]]
    [hyperfiddle.io.datomic.sync :refer [sync]]
    [hyperfiddle.io.datomic.transact :refer [transact!]]
    [hyperfiddle.route :as route]
    [hyperfiddle.service.cookie :as cookie]
    [hyperfiddle.service.dispatch :as dispatch]
    [hyperfiddle.service.pedestal :as hf-http]
    [hyperfiddle.service.resolve :as R]
    [promesa.core :as p]
    [taoensso.timbre :as timbre])
  (:import
    [hyperfiddle.domain EdnishDomain]))

(defn public? [handler]
  (or (= "public" (namespace handler))
      (#{:hyperfiddle.ide/auth0-redirect :hyperfiddle.ide/logout} handler)))


(defmethod dispatch/via-domain EdnishDomain [context]
  (let [domain (:domain (R/from context))
        cookie-domain (:hyperfiddle.ide.directory/ide-domain domain)
        {:keys [auth0]} (:config (R/from context))
        #_#_{:keys [cookie-name jwt-secret jwt-issuer]} (-> (get-in context [:request :domain])
                                                          domain/environment-secure :jwt)]
    (cond-> context

      auth0
      (hf-http/via
        (hf-http/with-user-id "jwt"
          cookie-domain
          (:client-secret auth0)
          (:domain auth0)))

      true
      (hf-http/via
       (fn [context]
         (let [domain                     (get-in context [:request :domain])
               [method path query-string headers] (-> context :request (select-keys [:request-method :path-info :query-string :headers]) vals)
               route                      (domain/api-match-path domain path :request-method method)
               is-auth-configured         (-> (R/from context) :config :auth0 :domain nil? not)
               is-private                 (not (public? (:handler route)))
               is-ssr                     (= :ssr (unqualify (:handler route)))
               is-no-subject              (nil? (get-in context [:request :user-id]))
               is-unauthenticated         (and is-auth-configured is-no-subject)
               prevent-infinite-redirect  (and (not (str/starts-with? path "/hyperfiddle.foundation!please-login"))
                                                       (not (str/includes? (get headers "referer" "") "/hyperfiddle.foundation!please-login")))
               redirect                   (hf/url-decode domain (cond-> path
                                                                  (seq query-string) (str "?" query-string)))
               url                        (hf/url-encode domain `(hyperfiddle.foundation/please-login ~redirect))]

           (when-not (= (:handler route) :public/static-resource)
             (timbre/info "ide-router:" (pr-str (:handler route)) (pr-str method) (pr-str path)))

           (if (and is-private
                    is-unauthenticated
                    prevent-infinite-redirect)
             (-> context
                 ;; If ssr, we want to browser to redirect to the login page ->
                 ;; 302 If ajax, we don't want the browser to follow the
                 ;; redirect, we want to fail with a meaningful code for the
                 ;; http client to act on it (trigger a refresh)
                 (assoc-in [:response :status] (if is-ssr 302 401))
                 (assoc-in [:response :headers "Location"] url))
             (-> context
                 (assoc-in [:request :handler] (:handler route))
                 (assoc-in [:request :route-params] (:route-params route))
                 dispatch/endpoint))))))))

(defmethod dispatch/endpoint :hyperfiddle.ide/auth0-redirect [context]
  (R/via context R/run-IO
    (fn [context]
      (let [{{:keys [domain] :as request} :request} context
            io (reify io/IO
                 (hydrate-requests [io local-basis partitions requests]
                   ; todo who is this executed on behalf of? system/root or the end user?
                   (p/do! (hydrate-requests domain local-basis requests partitions nil)))

                 (sync [io dbnames]
                   ; todo who is this executed on behalf of? system/root or the end user?
                   (p/do! (sync domain dbnames)))

                 (transact! [io tx-groups]
                   ; todo who is this executed on behalf of? system/root or the end user?
                   (p/do! (transact! domain nil tx-groups))))

            [jwt max-age] (from-async (hyperfiddle.ide.authenticate/login
                               (:config (R/from context))
                               domain
                               (R/via context R/uri-for :/)
                               io
                               (-> request :query-params :code)))]

        (hf-http/response
          context
          {:status 302
           :headers {"Location" (-> (get-in request [:query-params :state]) base64-url-safe/decode)}
           :cookies {"jwt" (-> domain :hyperfiddle.ide.directory/ide-domain
                                                 (cookie/jwt-options-pedestal)
                                                 (assoc :value jwt
                                                        :max-age max-age))}
           :body ""}))
      )))

(defmethod dispatch/endpoint :hyperfiddle.ide/logout [context]
  (hf-http/response
    context
    {:status 302
     :headers {"Location" "/"}
     :cookies {"jwt" (-> context :request :domain :hyperfiddle.ide.directory/ide-domain
                                           (cookie/jwt-options-pedestal)
                                           (assoc :value (get-in (:request context) [:cookies "jwt" :value])
                                                  :expires "Thu, 01 Jan 1970 00:00:00 GMT"))}
     :body ""}
    ))
