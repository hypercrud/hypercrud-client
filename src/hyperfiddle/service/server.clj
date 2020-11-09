(ns hyperfiddle.service.server
  (:require [clojure.core.async :as a]
            [contrib.data :as data]
            [hyperfiddle.service.handlers :as handlers]
            [hyperfiddle.service.interceptors :as i]
            [hyperfiddle.service.websockets :as ws]
            [io.pedestal.http :as http]
            [io.pedestal.http.ring-middlewares :as middlewares]
            [io.pedestal.http.route :as route]
            [io.pedestal.http.secure-headers :as secure-headers]
            [io.pedestal.interceptor.helpers :refer [defbefore before]]
            [promesa.core :as p])
  (:import org.eclipse.jetty.server.handler.gzip.GzipHandler))

(def base-config {::http/type            :jetty
                  ::http/allowed-origins {:allowed-origins (constantly true)}
                  ::http/secure-headers  {:content-security-policy-settings (secure-headers/content-security-policy-header {:object-src "'none'"})
                                          :content-type-settings            (secure-headers/content-type-header)}
                  ::http/join?           false})

(def async-dispatch
  (before (fn async-dispatch
            [{:keys [config request] :as context}]
            (let [channel (a/chan)]
              (-> (p/future (handlers/dispatch-http config request))
                  (p/then (fn [{:keys [response]}]
                            (a/go
                              ;; Don’t perform long running tasks here
                              ;; https://webcache.googleusercontent.com/search?q=cache:0WL7T8AKSwIJ:https://martintrojer.github.io/clojure/2013/07/07/coreasync-and-blocking-io+&cd=1&hl=fr&ct=clnk&gl=fr
                              (a/>! channel (assoc context :response response)))))
                  (p/catch (fn [err]
                             (a/go
                               (a/>! channel (update context :response assoc
                                                     :status 500
                                                     :body (ex-message err)))))))
              channel))))

(def interceptors [i/trace i/cookies i/negociate i/auto-content-type i/body-params])

(def rewrite-resources-path
  (before (fn [{:keys [request] :as context}]
            (assoc-in context [:request :path-info] (str "/" (get-in request [:path-params :file-path]))))))

(defn build [{:keys [host port] :as config}]
  (let [base-chain (conj interceptors (i/with-config config))]
    (merge base-config
           {::http/routes
            (->> (route/expand-routes
                  #{["/*route" :any (conj base-chain async-dispatch) :route-name :dispatch]
                    ["/static/:build/*file-path" :get [rewrite-resources-path (middlewares/file-info) (middlewares/file ".serve")] :route-name :serve]
                    })
                 (data/sort-by-position :route-name [:serve :index]))
            ::http/router            :linear-search ;; it’s fine we’ve got 2 routes
            ::http/host              host
            ::http/port              port
            ::http/request-logger    nil ;; handled by i/trace
            ::http/container-options {:context-configurator
                                      (fn [^org.eclipse.jetty.servlet.ServletContextHandler c]
                                        (let [gzip-handler (GzipHandler.)]
                                          (.setGzipHandler c gzip-handler)
                                          (.addIncludedMethods gzip-handler (into-array ["GET" "POST"]))
                                          (ws/add-ws-endpoints config c ws/paths)))}})))

