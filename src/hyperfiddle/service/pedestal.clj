(ns hyperfiddle.service.pedestal
  (:require
    [clojure.string :as string]
    [taoensso.timbre :as timbre]
    [cognitect.transit :as transit]
    [hypercrud.transit :as hc-t]
    [contrib.pprint]
    [contrib.do :refer :all]
    [hyperfiddle.service.cookie :as cookie]
    [hyperfiddle.service.jwt :as jwt]
    [hypercrud.types.Err :refer [->Err]]

    [io.pedestal.http :as http]
    [io.pedestal.http.content-negotiation :as content-negotiation]
    [io.pedestal.interceptor.helpers :as interceptor]
    [io.pedestal.http.body-params :as body-params]
    [io.pedestal.http.ring-middlewares :as ring-middlewares]
    [io.pedestal.http.route]
    [io.pedestal.interceptor.chain]
    [io.pedestal.http.secure-headers :as secure-headers]
    [ring.middleware.file :as file]
    [ring.middleware.resource :as resource])

  (:import
    [com.auth0.jwt.exceptions JWTVerificationException]
    [java.io OutputStreamWriter OutputStream]
    java.util.UUID
    org.apache.commons.lang3.StringEscapeUtils))


; Todo defprotocol HF-Server
;(defprotocol HF-Server
;  ...)


(def interceptor  io.pedestal.interceptor/interceptor)
(def enqueue      io.pedestal.interceptor.chain/enqueue)
(def terminate    io.pedestal.interceptor.chain/terminate)

(defn via
  "Shorthand for enqueue, it lets you compose interceptors easily. This is tricky because we’re doing composition
  with -> which then composes a stack of interceptors. You have to keep both steps in mind."
  ([i] (interceptor
         (if (fn? i) {:name  ::via
                      :enter (with-scope i)}
                     i)))
  ([context i] (enqueue context [(via i)])))

(def base-config
  {::http/type              :jetty
   ::http/allowed-origins   {:allowed-origins (constantly true)}
   ::http/container-options {:context-configurator
                             (fn [^org.eclipse.jetty.servlet.ServletContextHandler c]
                               (let [gzip-handler (org.eclipse.jetty.server.handler.gzip.GzipHandler.)]
                                 (.setGzipHandler c gzip-handler)
                                 (.addIncludedMethods gzip-handler (into-array ["GET" "POST"]))
                                 c))}
   ::http/secure-headers    {:content-security-policy-settings (secure-headers/content-security-policy-header {:object-src "'none'"})
                             :content-type-settings            (secure-headers/content-type-header)}})

(defn init [config]
  (http/create-server
    (into base-config config)))

(defn with-data-params [context]
  (-> context
      (via (body-params/body-params
             (body-params/default-parser-map
               :edn-options {:readers *data-readers*}
               :transit-options [{:handlers @hc-t/read-handlers}])))
      (via (fn [context]
             (let [{:keys [json-params edn-params transit-params]} (:request context)]
               (assoc-in context [:request :body-params] (or json-params edn-params transit-params)))))))

(defn with-cookies [context]
  (enqueue context [ring-middlewares/cookies]))

;; This is copied from the pedestal source code, it is private
(defn- print-fn
  [prn-fn]
  (fn [output-stream]
    (with-open [writer (OutputStreamWriter. output-stream)]
      (binding [*out* writer]
        (prn-fn))
      (.flush writer))))

(def content-transformers
  {"text/plain"
   (fn [body]
     (print-fn #(pr body)))

   "application/json"
   (fn [body]
     (print-fn #(http/json-print body)))

   "application/edn"
   (fn [body]
     ; hyperfiddle/hyperfiddle.net#38
     (print-fn #(pr body)))

   "application/transit+json"
   (fn [body]
     (fn [^OutputStream output-stream]
       (transit/write (transit/writer output-stream :json {:handlers @hc-t/write-handlers}) body)
       (.flush output-stream)))

   "application/transit+msgpack"
   (fn [body]
     (fn [^OutputStream output-stream]
       (transit/write (transit/writer output-stream :msgpack {:handlers @hc-t/write-handlers}) body)
       (.flush output-stream)))

   "text/html"
   (fn [body]
     (binding [clojure.pprint/*print-right-margin* 140]
       (let [body-str (->> (with-out-str (contrib.pprint/pprint body))
                        (StringEscapeUtils/escapeHtml4)
                        (format "<html><body><pre>%s</pre></body></html>"))]
         (print-fn #(.write *out* body-str)))))})

(defn coerce-to [response content-type]
  (-> response
      (update :body (get content-transformers content-type))
      (assoc-in [:headers "Content-Type"] content-type)))

(defn response [context r]
  {:pre [(:status r)]}
  (assoc context :response r))

(defn run-IO [context f]
  (-> context
      (via (content-negotiation/negotiate-content (keys content-transformers)))
      (via (interceptor/after ::auto-content-type
             (fn auto-content-type [context]
               (cond-> context
                 (nil? (get-in context [:response :headers "Content-Type"]))
                 (update :response coerce-to (get-in context [:request :accept :field] "text/plain"))))))
      (via {:name  ::run-io
            :enter (fn [context]
                     (do-async-as-chan
                       (try (let [context (from-async (f context))]
                              (assert (-> context :response :status))
                              context)
                            (catch Throwable e
                              (timbre/error e)
                              (response context
                                {:status  (or (:hyperfiddle.io/http-status-code (ex-data e)) 500)
                                 :headers {} ; todo retry-after on 503
                                 :body    (->Err (.getMessage e))})))))
            })))

(defn with-user-id [cookie-name cookie-domain jwt-secret jwt-issuer]
  {:pre [cookie-name cookie-domain jwt-secret jwt-issuer]}
  {:name ::with-user-id
   :enter (fn [context]
            (let [verify (fn [& args]
                           ; todo this is brittle
                           ; there are configurations where no auth is used
                           ; delay any exception from lack of or misconfiguration of verification until auth is used
                           (apply (jwt/build-verifier jwt-secret jwt-issuer) args))
                  jwt-cookie (get-in context [:request :cookies cookie-name :value])
                  jwt-header (some->> (get-in context [:request :headers "authorization"])
                               (re-find #"^Bearer (.+)$")
                               (second))
                  with-jwt (fn [jwt]
                             (-> context
                                 (assoc-in [:request :user-id] (some-> jwt (verify) :user-id UUID/fromString))
                                 (assoc-in [:request :jwt] jwt)))]
              (cond
                (or (nil? jwt-header) (= jwt-cookie jwt-header))
                (try (with-jwt jwt-cookie)
                     (catch JWTVerificationException e
                       (timbre/error e)
                       (-> (terminate context)
                           (assoc :response {:status 401
                                             :cookies {cookie-name (-> (cookie/jwt-options-pedestal cookie-domain)
                                                                       (assoc :value jwt-cookie
                                                                              :expires "Thu, 01 Jan 1970 00:00:00 GMT"))}
                                             :body (->Err (.getMessage e))}))))

                (nil? jwt-cookie)
                (try (with-jwt jwt-header)
                     (catch JWTVerificationException e
                       (timbre/error e)
                       (-> (terminate context)
                           (assoc :response {:status 401 :body (->Err (.getMessage e))}))))

                :else (-> (terminate context)
                          (assoc :response {:status 400 :body (->Err "Conflicting cookies and auth bearer")})))))})

(def log-request
  (interceptor/on-request
    ::log-request
    (fn [request]
      (if-not (-> request :uri (clojure.string/starts-with? "/static"))
        (timbre/info {:remote-addr (:remote-addr request)
                      :host        (:server-name request)
                      :port        (:server-port request)
                      :method      (string/upper-case (name (:request-method request)))
                      :uri         (:uri request)
                      :protocol    (:protocol request)
                      :referer     (get-in request [:headers "referer"])
                      :user-agent  (get-in request [:headers "user-agent"])}))
      request)))

(defn- static-content [with-request mime-type-by-extension root-path context]
  (let [filename (str "/" (get-in context [:request :route-params :resource-name]))
        mime-type (or (mime-type-by-extension filename) "application/octet-stream")
        accept (get-in context [:request :headers "accept"])]
    (assoc context
      :response
      (if (or (nil? accept)                                 ; nil accept headers are ok
            (content-negotiation/best-match
              (content-negotiation/best-match-fn [mime-type])
              (content-negotiation/parse-accept-* accept)))
        (-> {:request-method (get-in context [:request :request-method])
             :path-info filename}
            (with-request root-path)
            (assoc-in [:headers "Content-Type"] mime-type))
        {:status 406
         :body "Not Acceptable"
         :headers {}}))))

(defn static-resource [mime-type-by-extension root-path context]
  (static-content resource/resource-request mime-type-by-extension root-path context))

(defn static-file [mime-type-by-extension root-path context]
  (static-content file/file-request mime-type-by-extension root-path context))

(defn static-file-resolve [root-path context]
  (static-content file/file-request
                  #(org.eclipse.jetty.http.MimeTypes/getDefaultMimeByExtension %)
                  root-path
                  context))
