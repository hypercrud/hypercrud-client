(ns hyperfiddle.service.handlers
  (:require [bidi.bidi :as bidi]
            [clojure.string :as string]
            [hyperfiddle.api :as hf]
            [hyperfiddle.io.core :as io]
            [hyperfiddle.io.datomic.hydrate-requests :refer [hydrate-requests]]
            [hyperfiddle.io.datomic.hydrate-route :refer [hydrate-route]]
            [hyperfiddle.io.datomic.sync :as ds]
            [hyperfiddle.io.datomic.transact :refer [transact!]]
            [hyperfiddle.route :as route]
            [hyperfiddle.service.auth :as auth]
            [hyperfiddle.service.endpoints :as endpoints]
            [hyperfiddle.service.routes :as routes]
            [promesa.core :as p]))

(deftype IOImpl [context]
  io/IO
  (global-basis [io]
    (io/global-basis-for (assoc context :io io)))
  (local-basis [io global-basis route]
    (p/resolved (io/local-basis-for io global-basis route)))
  (hydrate-requests [io local-basis partitions requests]
    (p/do! (hydrate-requests (:config context) local-basis requests partitions (:user-id (auth/subject context)))))
  (hydrate-route [io local-basis route pid partitions]
    (p/do! (hydrate-route (:config context) local-basis route pid partitions (:user-id (auth/subject context)))))
  (sync [io dbnames]
    (p/do! (ds/sync (:config context) dbnames)))
  (transact! [io tx-groups]
    (p/do! (transact! (:config context) (auth/subject context) tx-groups))))

;;;;;;;;;;
;; HTTP ;;
;;;;;;;;;;

(defn- build-http-context [config {:keys [path-info] :as request}]
  (let [context {:config  config
                 :request request
                 :auth    (auth/build-auth-context config request)
                 :route   (route/url-decode path-info (:home-route config))}]
    ;; FIXME convoluted
    (assoc context :io (->IOImpl context))))

(defn- api-match-path [path & {:as options}]
  (apply bidi/match-route routes/routes path (apply concat options)))

(defn dispatch-http [config {:keys [request-method path-info query-string] :as request}]
  (let [context                        (build-http-context config request)
        {:keys [handler route-params]} (api-match-path path-info :request-method request-method)]
    (cond
      (and (= ::hf/ssr handler)
           (auth/configured? context)                        ;; if there is an auth0 config, require logins
           (not (auth/authenticated? context))
           (not (string/starts-with? path-info "/hyperfiddle.foundation!please-login")))

      ,, (let [redirect (route/url-decode (cond-> path-info (seq query-string) (str "?" query-string)) (:home-route config))
               url      (route/url-encode `(hyperfiddle.foundation/please-login ~redirect) (:home-route config))]
           (-> context
               (assoc-in [:response :status] 302)
               (assoc-in [:response :headers "Location"] url)))

      :else (-> context
                (update :request merge {:handler      handler
                                        :route-params route-params})
                endpoints/endpoint))))

;;;;;;;;
;; WS ;;
;;;;;;;;

(defn build-ws-context [{:keys [config request]}]
  (let [request (bean request)
        context {:config  config
                 :request request
                 :auth    (auth/build-auth-context config request)
                 :ws      {:session nil
                           :chan    nil}}]
    ;; FIXME convoluted
    (assoc context :io (->IOImpl context))))

(defn dispatch-ws [context {:keys [route path-params body-params]}]
  (-> context
      (update :request merge {:handler      route
                              :route-params path-params
                              :body-params  body-params})
      (endpoints/endpoint)))
