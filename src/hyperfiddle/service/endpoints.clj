(ns hyperfiddle.service.endpoints
  (:require [contrib.base-64-url-safe :as base-64-url-safe]
            [contrib.ednish :as ednish]
            [contrib.reader :as reader]
            [hiccup.core :as hiccup]
            [hyperfiddle.api :as hf]
            [hyperfiddle.config :as config]
            [hyperfiddle.io.core :as io]
            [hyperfiddle.io.datomic.hydrate-requests :refer [hydrate-requests]]
            [hyperfiddle.io.datomic.sync :as ds]
            [hyperfiddle.io.datomic.transact :refer [transact!]]
            [hyperfiddle.service.auth :as auth]
            [hyperfiddle.service.render :as render]
            [hyperfiddle.service.websockets.auth :as wauth]
            [promesa.core :as p]
            [taoensso.timbre :as log]))

(defmulti endpoint (fn [context] (get-in context [:request :handler])))

;;;;;;;;;;
;; Auth ;;
;;;;;;;;;;

(defmethod endpoint ::hf/auth0-redirect [{:keys [config io request] :as context}]
  (-> (auth/login! config (config/service-uri config) io (get-in request [:query-params :code]))
      (p/then (fn [[jwt max-age]]
                (assoc context :response
                       {:status  302
                        :headers {"Location" (-> (get-in request [:query-params :state]) base-64-url-safe/decode)}
                        :cookies {"jwt" (auth/cookie {:domain  (:server-name request)
                                                      :value   jwt
                                                      :max-age max-age})}
                        :body    ""})))))

(defmethod endpoint ::hf/ws-exchange-token [context]
  (assoc context :response {:status 200
                            :body   (if (and (auth/configured? context)
                                             (auth/authenticated? context))
                                      (str (wauth/generate-token! (:jwt (auth/subject context))))
                                      "")}))

(defmethod endpoint ::hf/logout [context]
  (assoc context :response
         {:status  302
          :headers {"Location" "/"}
          :cookies {"jwt" (auth/cookie {:domain  (:server-name (:request context))
                                        :value   ""
                                        :max-age 0})}
          :body    ""}))

;;;;;;;;;;;;;;;;;;
;; SSR / static ;;
;;;;;;;;;;;;;;;;;;

(defmethod endpoint :favicon [context]
  (assoc-in context [:response :status] 204))

(defmethod endpoint ::hf/ssr [context]
  (-> (render/render context)
      (p/then (fn [{:keys [http-status-code component]}]
                (assoc context :response
                       {:status  http-status-code
                        :headers {"Content-Type" "text/html"}
                        :body    (str "<!DOCTYPE html>\n" (hiccup/html component))})))
      (p/catch (fn [err]
                 (log/error err)
                 (assoc context :response
                        {:status  (or (:hyperfiddle.io/http-status-code (ex-data err)) 500)
                         :headers {"Content-Type" "text/html"}
                         :body    (str "<h2>Fatal error:</h2><h4>" (ex-message err) "</h4>")})))))

;;;;;;;;
;; IO ;;
;;;;;;;;

(defmethod endpoint ::hf/global-basis [{:keys [io] :as context}]
  (-> (io/global-basis io)
      (p/then (fn [basis]
                (assoc context :response
                       {:status  200
                        :headers {"Cache-Control" "max-age=0"}
                        :body    basis})))))

(defmethod endpoint ::hf/local-basis [{:keys [io] :as context}]
  (let [{:keys [route-params]} context
        global-basis           (ednish/decode-uri (:global-basis route-params)) ; todo this can throw
        route                  (-> (:encoded-route route-params) base-64-url-safe/decode reader/read-edn-string!)]
    (-> (io/local-basis io global-basis route)
        (p/then (fn [local-basis]
                  (assoc context :response
                         {:status  200
                          :headers {"Cache-Control" "max-age=31536000"}
                          :body    local-basis}))))))

(defmethod endpoint ::hf/hydrate-route [{:keys [config io request] :as context}]
  (let [{:keys [route-params body-params]} request
        partitions                         body-params
        local-basis                        (ednish/decode-uri (:local-basis route-params)) ; todo this decoding can throw
        route                              (-> (:encoded-route route-params) base-64-url-safe/decode reader/read-edn-string!)
        ;; TODO this needs work, decoding should happen after the domain is hydrated
        ;; route (-> (str "/" (:encoded-route route-params)) (foundation/route-decode rt))
        ;; TODO this should be an interceptor
        ;; _ (try (first (from-result (s/valid? ::route/route route)))
        ;;        (catch Exception e
        ;;          (throw (ex-info "Invalid encoded-route" {:route route :hyperfiddle.io/http-status-code 400} e))))
        pid                                (-> (:partition-id route-params) base-64-url-safe/decode reader/read-edn-string!)
        local-basis                        (reduce-kv assoc
                                                      (into {} local-basis)
                                                      @(io/sync io
                                                                (->> (vals partitions)
                                                                     (mapcat :stage)
                                                                     (remove (comp empty? second))
                                                                     (map first)
                                                                     (filter #(config/db config %)) ; only sync declared dbnames
                                                                     (distinct))))]
    (-> (io/hydrate-route io local-basis route pid partitions)
        (p/then (fn [body]
                  (assoc context :response {:status  200
                                            :headers {"Cache-Control" (if true #_(some seq (vals stage)) ; TODO
                                                                          "max-age=0"
                                                                          "max-age=31536000")}
                                            :body    body}))))))

(defmethod endpoint ::hf/hydrate-requests [{:keys [request config] :as context}]
  (let [{:keys [user-id]}                  (auth/subject context)
        {:keys [body-params route-params]} request
        local-basis                        (ednish/decode-uri (:local-basis route-params))
        {:keys [partitions requests]}      body-params]
    (-> (hydrate-requests config local-basis requests partitions user-id)
        (p/then (fn [body]
                  (assoc context :response {:status  200
                                            :headers {}
                                            :body    body}))))))

(defmethod endpoint ::hf/sync [{:keys [config request] :as context}]
  (assoc context :response {:status  200
                            :headers {}
                            :body    (ds/sync config (:body-params request))}))

(defmethod endpoint ::hf/transact [{:keys [config request] :as context}]
  (let [{:keys [user-id]} (auth/subject context)]
    (assoc context :response {:status  200
                              :body    (transact! config user-id (:body-params request))})))
