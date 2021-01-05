(ns hyperfiddle.io.client
  (:require
   [bidi.bidi :as bidi]
   [clojure.set :as set]
   [clojure.core.async :as a :include-macros true]
   [contrib.base-64-url-safe :as base-64-url-safe]
   [contrib.data :as data]
   [contrib.ednish :as ednish]
   [cuerdas.core :as str]
   [hyperfiddle.api :as hf]
   [hyperfiddle.io.client.http :as http]
   [hyperfiddle.io.client.ws :as ws]
   [hyperfiddle.service.routes :as routes]
   [promesa.core :as p]
   [taoensso.timbre :as timbre]))

(def ^:dynamic *force-refresh*)

(defn- handle-error [err]
  (let [{:keys [body status] :as data} (ex-data err)
        response-body                  body]
    (cond
      (data/ex-info? response-body) (throw (ex-info (:msg response-body)
                                                    (assoc data :hyperfiddle.io/http-status-code status)
                                                    (ex-cause err)))
      (and (= 502 status)
           (not (ex-message err)))  (throw (ex-info "Service Unavailable"
                                                    (assoc data :hyperfiddle.io/http-status-code 502)
                                                    (ex-cause err)))
      (and (= 503 status))          (timbre/error "Service Unavailable")
      (and (= 504 status)
           (not (ex-message err)))  (throw (ex-info "Service timed out"
                                                    (assoc data :hyperfiddle.io/http-status-code 504)
                                                    (ex-cause err)))
      (= status 0)                  (throw (ex-info (ex-message err) data (ex-cause err)))
      (and (= status 404)
           (some? *force-refresh*)
           (= "Please refresh your browser"
              response-body))       (*force-refresh* (ex-info response-body
                                                              (assoc data :hyperfiddle.io/http-status-code status)
                                                              (ex-cause err)))
      status                        (throw (ex-info (ex-message err)
                                                    (assoc data :hyperfiddle.io/http-status-code status)
                                                    (ex-cause err)))
      :else                         (throw err))))

(defn ws-request! [req]
  (-> (ws/send! {:type :hyperfiddle-action
                 :data (-> (select-keys req [:ws/route :ws/path-params :params])
                           (set/rename-keys {:ws/route       :route
                                             :ws/path-params :path-params
                                             :params         :body-params}))})
      (p/then :body)
      (p/catch (fn [{:keys [status body]}]
                 ;; Reshape error
                 (throw (ex-info (ex-message body) {:status status, :body (ex-data body)} body))))))

(defn request! [req jwt]
  (->
   ;; (http/request! req jwt)
   (ws-request! req)
   (p/catch handle-error)))

(defn global-basis!
  "See `hyperfiddle.io.core/global-basis` method."
  [config ?service-uri & [jwt]]
  (-> {:method   :get
       :url      (str ?service-uri (bidi/path-for routes/routes ::hf/global-basis))
       :ws/route ::hf/global-basis}
      (request! jwt)))

(defn hydrate-requests!
  "See `hyperfiddle.io.core/hydrate-requests` method."
  [config ?service-uri local-basis partitions requests & [jwt]]
  (let [form {:partitions partitions
              :request    requests}]
    (timbre/debugf "hydrate-requests! request count= %s basis= %s form= %s" (count requests) (pr-str local-basis) (str/prune (pr-str form) 100))
    (-> {;; hydrate-requests always has a POST body, though it has a basis and is cachable
         :method         :post
         :url            (str ?service-uri (bidi/path-for routes/routes ::hf/hydrate-requests :local-basis (ednish/encode-uri local-basis))) ; serialize kvseq
         :params         form
         :ws/route       ::hf/hydrate-requests
         :ws/path-params {:local-basis (ednish/encode-uri local-basis)}}
        (request! jwt)
        (p/then (fn [body]
                  (assert (= (count requests) (count (:pulled-trees body))) "Server contract violation; mismatched counts")
                  body)))))

(defn hydrate-route!
  "See `hyperfiddle.io.core/hydrate-route` method."
  [config ?service-uri local-basis route pid partitions & [jwt]]
  {:pre [config local-basis route]}
  (-> {:method         :post
       :url            (str ?service-uri
                            (bidi/path-for routes/routes ::hf/hydrate-route
                                           :local-basis       (ednish/encode-uri local-basis)
                                           :encoded-route     (base-64-url-safe/encode (pr-str route))
                                           :partition-id      (base-64-url-safe/encode (pr-str pid))
                                           ;; todo this needs work
                                           #_#_:encoded-route (subs (foundation/route-encode rt route) 1) ; includes "/"
                                           ))
       :params         partitions
       :ws/route       ::hf/hydrate-route
       :ws/path-params {:local-basis   (ednish/encode-uri local-basis)
                        :encoded-route (base-64-url-safe/encode (pr-str route))
                        :partition-id  (base-64-url-safe/encode (pr-str pid))}}
      (request! jwt)))

(defn local-basis!
  "See `hyperfiddle.io.core/local-basis` method."
  [config ?service-uri global-basis route & [jwt]]
  (-> {:method         :get
       :url            (str ?service-uri
                            (bidi/path-for routes/routes ::hf/local-basis
                                           :global-basis  (ednish/encode-uri global-basis)
                                           :encoded-route (base-64-url-safe/encode (pr-str route))
                                           ;; todo this needs work
                                           #_#_:encoded-route (subs (foundation/route-encode rt route) 1) ; includes "/"
                                           ))
       :ws/route       ::hf/local-basis
       :ws/path-params {:global-basis  (ednish/encode-uri global-basis)
                        :encoded-route (base-64-url-safe/encode (pr-str route))}}
      (request! jwt)))

(defn sync!
  "See `hyperfiddle.io.core/sync` method."
  [config ?service-uri dbnames & [jwt]]
  (-> {:method   :post
       :url      (str ?service-uri (bidi/path-for routes/routes ::hf/sync))
       :params   dbnames
       :ws/route ::hf/sync}
      (request! jwt)))

(defn transact!
  "See `hyperfiddle.io.core/transact!` method."
  [config ?service-uri tx-groups & [jwt]]
  (-> {:method   :post
       :url      (str ?service-uri (bidi/path-for routes/routes ::hf/transact))
       :params   tx-groups
       :ws/route ::hf/transact}
      (request! jwt)))

(defn subscribe!*
  [config ?service-uri & [jwt]]
  (-> {:type :subscription
       :data {:route ::hf/subscribe}}
      (ws/subscribe!)))

(defn subscribe! [cont]
  (let [[id chan] (subscribe!* nil nil)]
    (a/go-loop [data (a/<! chan)]
      (when (some? data)
        (cont data)
        (recur (a/<! chan))))
    id))

(defn put! [id data]
  (ws/send! {:type :put!
             :id   id
             :data data}))
