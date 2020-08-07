(ns hyperfiddle.service.dispatch
  (:require
    [taoensso.timbre :as timbre]
    [hiccup.core :as hiccup]
    [promesa.core :as p]
    [contrib.performance :as perf]
    [contrib.ednish :as ednish]
    [contrib.base-64-url-safe :as base-64-url-safe]
    [contrib.reader :refer [read-edn-string!]]

    [contrib.do :refer :all]
    [hyperfiddle.service.resolve :as R]
    [hyperfiddle.service.render :as render]
    [hyperfiddle.service.pedestal :as hf-http]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.route :as route]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.datomic.sync :as ds]
    [hyperfiddle.io.datomic.transact :refer [transact!]]
    [hyperfiddle.io.datomic.hydrate-requests :refer [hydrate-requests]]
    [hyperfiddle.io.datomic.hydrate-route :refer [hydrate-route]]
    [hyperfiddle.api :as hf]))


; these are renamed from h.s.service-domain/route and h.s.http/handle-route
; handlers for these methods are currently distributed between here
; and hyperfiddle.ide.service.pedestal

(defmulti via-domain
  "resolves a request handler via the domain type associated with this request"
  (fn [context]
    (let [domain (:domain (R/from context))]
      (assert (not (nil? domain)))
      (push-scope ['domain (type domain)])
      (type domain))))

(defmulti endpoint
  "resolves response method"
  (fn [context]
    (let [handler (-> context :request :handler)]
      (assert (not (nil? handler)))
      (push-scope ['endpoint handler])
      handler)))

(defmethod via-domain :default [context]
  (let [domain (:domain (R/from context))
        [method path] (-> context :request (select-keys [:request-method :path-info]) vals)
        route (domain/api-match-path domain path :request-method method)]

    (when-not (= (:handler route) :static-resource)
      (timbre/info "router:" (pr-str (:handler route)) (pr-str method) (pr-str path)))

    (-> context
        (assoc-in [:request :handler] (:handler route))
        (assoc-in [:request :route-params] (:route-params route))
        endpoint)))

(defmethod endpoint :default [context]
  (let [tag (-> context :request :handler)]
    (hf-http/response context {:status 501 :body (str (pr-str tag) " not implemented")})))

(defmethod endpoint :404 [context]
  (hf-http/response context {:status 404 :headers {} :body "Not found"}))

(defmethod endpoint :405 [context]
  (hf-http/response context {:status 405 :headers {} :body "Method Not Allowed"}))

(defmethod endpoint :force-refresh [context]
  (hf-http/response context {:status 404 #_410 :body "Please refresh your browser"}))

(defmethod endpoint :favicon [context]
  (hf-http/response context {:status 204}))

(defmethod endpoint :static-resource [context]
  (R/via context R/serve))

(defmethod endpoint :hydrate-requests [context]
  (R/via context R/run-IO
    (fn [context]
      (let [{:keys [domain user-id body-params route-params]} (:request context)
            local-basis (ednish/decode-uri (:local-basis route-params))
            {:keys [partitions requests]} body-params]
        (hf-http/response context {:status  200
                                   :headers {}
                                   :body    (->> (hydrate-requests domain local-basis requests partitions user-id)
                                                 (perf/time
                                                   (fn [total-time]
                                                     (timbre/debugf "hydrate-requests: count %s, total time: %sms"
                                                       (count requests) total-time))))})))))

(defmethod endpoint :sync [context]
  (R/via context R/run-IO
    (fn [context]
      (let [{{:keys [domain user-id body-params]} :request} context]
        (hf-http/response context {:status  200
                                   :headers {}
                                   :body    (ds/sync domain body-params)})))))

(defmethod endpoint :transact [context]
  (R/via context R/run-IO
         (fn [context]
           (let [{{:keys [domain user-id body-params]} :request} context]
             (hf-http/response context {:status  200
                                        :headers {}
                                        :body    (transact! domain user-id body-params)})))))

(deftype IOImpl [domain ?subject]
  io/IO

  (global-basis [io]
    (io/global-basis-for io domain))

  (local-basis [io global-basis route]
    (p/resolved (io/local-basis-for io global-basis route)))

  (hydrate-requests [io local-basis partitions requests]
    (p/do! (hydrate-requests domain local-basis requests partitions ?subject)))

  (hydrate-route [io local-basis route pid partitions]
    (p/do! (hydrate-route domain local-basis route pid partitions ?subject)))

  (sync [io dbnames]
    (p/do! (ds/sync domain dbnames)))

  (transact! [io tx-groups]
    (p/do! (transact! domain ?subject tx-groups))))

(defmethod endpoint :ssr [context]
  (R/via context R/render))

(defn render [context]
  (do-async-as-chan
    (hf-http/response context
      (try
        (let [{:keys [domain route-params body-params user-id]} (:request context)
              io (R/via context R/IO)
              route (hf/url-decode domain
                      (str (get-in context [:request :path-info]) "?" (get-in context [:request :query-string])))
              {:keys [http-status-code component]}
              (from-async (render/render (-> (R/from context) :config) domain io route user-id))]
          {:status  http-status-code
           :headers {"Content-Type" "text/html"}
           :body    (str "<!DOCTYPE html>\n" (hiccup/html (apply (first component) (rest component))))})
        (catch Exception e
          (timbre/error e)                                  ; this can crash the pretty printer e.g. with eithers
          {:status  (or (:hyperfiddle.io/http-status-code (ex-data e)) 500)
           :headers {"Content-Type" "text/html"}
           :body    (str "<h2>Fatal error:</h2><h4>" (ex-message e) "</h4>")}
          )))))

(defmethod endpoint :global-basis [context]
  (R/via context R/run-IO
    (fn [context]
      (let [{{:keys [domain route-params body-params user-id]} :request} context]
        (hf-http/response context
          {:status  200
           :headers {"Cache-Control" "max-age=0"}
           :body    (from-async (io/global-basis (R/via context R/IO)))})))))

(defmethod endpoint :local-basis [context]
  (R/via context R/run-IO
    (fn [context]
      (let [{:keys [domain route-params body-params user-id]} context]
        (hf-http/response context
          (let [global-basis (ednish/decode-uri (:global-basis route-params)) ; todo this can throw
                route (-> (:encoded-route route-params) base-64-url-safe/decode read-edn-string!)
                ; todo this needs work, decoding should happen after the domain is hydrated
                #_#_route (-> (str "/" (:encoded-route route-params)) (foundation/route-decode rt))]

            (try (first (from-result (route/validate-route+ route)))
                 (catch Exception e
                   (throw (ex-info "Invalid encoded-route" {:route route :hyperfiddle.io/http-status-code 400} e))))

            {:status  200
             :headers {"Cache-Control" "max-age=31536000"}
             :body    (from-async (io/local-basis (R/via context R/IO) global-basis route))}))))))

(defmethod endpoint :hydrate-route [context]
  (R/via context R/run-IO
    (fn [context]
      (let [{{:keys [domain route-params body-params user-id]} :request} context]
        (hf-http/response context
          (let [partitions body-params
                io (R/via context R/IO)
                local-basis (ednish/decode-uri (:local-basis route-params)) ; todo this decoding can throw
                route (-> (:encoded-route route-params) base-64-url-safe/decode read-edn-string!)

                ; todo this needs work, decoding should happen after the domain is hydrated
                #_#_route (-> (str "/" (:encoded-route route-params)) (foundation/route-decode rt))

                _ (try (first (from-result (route/validate-route+ route)))
                       (catch Exception e
                         (throw (ex-info "Invalid encoded-route" {:route route :hyperfiddle.io/http-status-code 400} e))))

                pid (-> (:partition-id route-params) base-64-url-safe/decode read-edn-string!)

                local-basis (reduce-kv assoc (into {} local-basis)
                              (from-async
                                (io/sync io
                                  (->> (vals partitions)
                                       (mapcat :stage)
                                       (remove (comp empty? second))
                                       (map first)
                                       (filter #(hf/database domain %)) ; only sync declared dbnames
                                       (distinct)))))]

            {:status  200
             :headers {"Cache-Control" (if true #_(some seq (vals stage)) ; todo
                                         "max-age=0"
                                         "max-age=31536000")}
             :body    (from-async (io/hydrate-route io local-basis route pid partitions))}))))))


