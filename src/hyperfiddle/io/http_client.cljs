(ns hyperfiddle.io.http-client
  (:require
    [contrib.base-64-url-safe :as base-64-url-safe]
    [contrib.ednish :as ednish]
    [cuerdas.core :as str]
    [hypercrud.types.Err :as Err]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.http-client.impl :as impl]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))

(def ^:dynamic *force-refresh*)

(defn- handle-error [err]
  (let [{:keys [body status] :as data} (ex-data err)
        response-body                  body]
    (cond
      (Err/Err? response-body)     (throw (ex-info (:msg response-body)
                                                   (assoc data :hyperfiddle.io/http-status-code status)
                                                   (ex-cause err)))
      (and (= 502 status)
           (not (ex-message err))) (throw (ex-info "Service Unavailable"
                                                   (assoc data :hyperfiddle.io/http-status-code 502)
                                                   (ex-cause err)))
      (and (= 504 status)
           (not (ex-message err))) (throw (ex-info "Service timed out"
                                                   (assoc data :hyperfiddle.io/http-status-code 504)
                                                   (ex-cause err)))
      (= status 0)                 (throw (ex-info (ex-message err) data (ex-cause err)))
      (and (= status 404)
           (some? *force-refresh*)
           (= "Please refresh your browser"
              response-body))      (*force-refresh* (ex-info response-body
                                                             (assoc data :hyperfiddle.io/http-status-code status)
                                                             (ex-cause err)))
      status                       (throw (ex-info (ex-message err)
                                                   (assoc data :hyperfiddle.io/http-status-code status)
                                                   (ex-cause err)))
      :else                        (throw err))))

(defn request! [req jwt]
  (-> (impl/request! req jwt)
      (p/catch handle-error)))

(defn global-basis!
  "See `hyperfiddle.io.core/global-basis` method."
  [domain ?service-uri & [jwt]]
  (-> {:method :get
       :url    (str ?service-uri (domain/api-path-for domain :global-basis))}
      (request! jwt)))

(defn hydrate-requests!
  "See `hyperfiddle.io.core/hydrate-requests` method."
  [domain ?service-uri local-basis partitions requests & [jwt]]
  (let [form {:partitions partitions
              :request    requests}]
    (timbre/debugf "hydrate-requests! request count= %s basis= %s form= %s" (count requests) (pr-str local-basis) (str/prune (pr-str form) 100))
    (-> {;; hydrate-requests always has a POST body, though it has a basis and is cachable
         :method :post
         :url    (str ?service-uri (domain/api-path-for domain :hydrate-requests :local-basis (ednish/encode-uri local-basis))) ; serialize kvseq
         :params form}
        (request! jwt)
        (p/then (fn [body]
                  (assert (= (count requests) (count (:pulled-trees body))) "Server contract violation; mismatched counts")
                  body)))))

(defn hydrate-route!
  "See `hyperfiddle.io.core/hydrate-route` method."
  [domain ?service-uri local-basis route pid partitions & [jwt]]
  {:pre [domain local-basis route]}
  (-> {:method :post
       :url    (str ?service-uri
                    (domain/api-path-for domain :hydrate-route
                                         :local-basis       (ednish/encode-uri local-basis)
                                         :encoded-route     (base-64-url-safe/encode (pr-str route))
                                         :partition-id      (base-64-url-safe/encode (pr-str pid))
                                         ;; todo this needs work
                                         #_#_:encoded-route (subs (foundation/route-encode rt route) 1) ; includes "/"
                                         ))
       :params partitions}
      (request! jwt)))

(defn local-basis!
  "See `hyperfiddle.io.core/local-basis` method."
  [domain ?service-uri global-basis route & [jwt]]
  (-> {:method :get
       :url    (str ?service-uri
                    (domain/api-path-for domain :local-basis
                                         :global-basis  (ednish/encode-uri global-basis)
                                         :encoded-route (base-64-url-safe/encode (pr-str route))
                                         ;; todo this needs work
                                         #_#_:encoded-route (subs (foundation/route-encode rt route) 1) ; includes "/"
                                         ))}
      (request! jwt)))

(defn sync!
  "See `hyperfiddle.io.core/sync` method."
  [domain ?service-uri dbnames & [jwt]]
  (-> {:method :post
       :url    (str ?service-uri (domain/api-path-for domain :sync))
       :params dbnames}
      (request! jwt)))

(defn transact!
  "See `hyperfiddle.io.core/transact!` method."
  [domain ?service-uri tx-groups & [jwt]]
  (-> {:method :post
       :url    (str ?service-uri (domain/api-path-for domain :transact))
       :params tx-groups}
      (request! jwt)))
