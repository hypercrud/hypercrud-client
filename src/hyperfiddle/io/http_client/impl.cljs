(ns hyperfiddle.io.http-client.impl
  (:require
   [cljs-http.client :as http]
   [cljs.core.async :refer [<!]]
   [contrib.performance :as perf]
   [hypercrud.transit :as transit]
   [promesa.core :as p]
   [taoensso.timbre :as timbre])
  (:require-macros [cljs.core.async.macros :refer [go]]))

(def ^:dynamic *content-type* :transit)                     ; for repl e.g. (set! *content-type* :edn)

(def content-types {:edn     "application/edn"
                    :transit "application/transit+json"
                    :json    "application/json"})

(defn- add-auth-info
  "Add oAuth info to the request, if any. Expects a JWT string or nil, will set
  the Authentication: Bearer <jwt> header."
  [req ?jwt]
  (if ?jwt
    (assoc req :oauth-token ?jwt)
    req))

(defn- add-custom-transit
  "Add extra transit readers and writers to the request options. Doesn't have any
  effect if the request content type is not `:transit`."
  [req]
  (assoc req :transit-opts {:decoding-opts {:handlers @transit/read-handlers}
                            :encoding-opts {:handlers @transit/write-handlers}}))

(defn- add-content
  "If a request got some content to send or expect some content in the response,
  this function set:
  - the proper expected content type
  - the proper sent content type, if any."
  [{:keys [params] :as req}]
  (let [content-type (get content-types *content-type*)]
    (cond-> req
      true   (assoc :accept content-type)
      params (-> (assoc :content-type content-type)
                 (assoc (case *content-type*
                          :edn     :edn-params
                          :transit :transit-params
                          :json    :json-params)
                        params)
                 (dissoc :params)))))

(defn- error?
  "State if a request did succeed or not."
  [{:keys [status]}]
  ;; This check is quite naive but good enough for now. You might also want to
  ;; check for other HTTP codes or for the `:status` key.
  (not= 200 status))

(defn- supports-metas?
  "State if an object supports `with-meta`.
  NOTE: Checking for IMeta only tells you if `meta` is supported, not
  `with-meta`.
  NOTE: Only works in cljs, in Clojure this protocol doesn't exist for legacy
  reasons, use IObj interface instead."
  [x]
  (satisfies? IWithMeta x))

(defn- handle-response
  "Handle the HTTP response delivered to the `channel`. Will return a resolved or
  rejected promise based on the respective response success or failure state.
  This function exists because:

  - the http client returns a channel,
  - the domain level expects a promise,
  - the response channel will deliver exactly one value,
  - the delivered value might be in a success or error state.

  You can think of this function as (fmap response-to-promise channel). It's not
  implemented as fmap because it would be overkill."
  [request channel]
  (let [result (p/deferred)]
    (go (let [{:keys [body] :as response} (<! channel)]
          (if (error? response)
            ;; Errors are handled at the domain level. This namespace only cares
            ;; about the IO level.
            (p/reject! result (with-meta response {:http/request request}))
            ;; The domain level do not care about the HTTP response metadata in
            ;; the happy path, so we can abstract the HTTP layer away by
            ;; returning a plain value with HTTP metas as real metadata.
            (p/resolve! result (if (supports-metas? body)
                                 (with-meta body {:http/request  request
                                                  :http/response response})
                                 body)))))
    result))

(defn- prepare
  "Add authentication, content-types, and other various options a request might
  need before getting sent."
  [request jwt]
  (-> request
      (add-auth-info jwt)
      (add-custom-transit)
      (add-content)))

(defn request!
  "Perform a network request and return a promise. The promise will resolve or
  reject based on the response status. If the promise rejects, it will contain
  the full HTTP response, with the original request in metas. If it resolves, it
  will contain the body value, with the full HTTP response and request as
  metadata.

  Expects a map {:method, :url, :params, …}. Full spec at
  https://github.com/r0man/cljs-http.

  Metadatas are in the form {:http/request, :http/response}
  "
  ([req]
   (request! req nil))
  ([{:keys [url] :as req} jwt]
   (let [req      (prepare req jwt)
         req-hash (delay (hash req))]
     (perf/time-promise
      (do
        (timbre/debugf "Request[%s] issued to %s" @req-hash url)
        (handle-response req (http/request req)))
      (fn [_ total-time]
        (timbre/debugf "Request[%s] failed, total time: %sms" @req-hash total-time))
      (fn [_ total-time]
        (timbre/debugf "Request[%s] succeeded, total time: %sms" @req-hash total-time))))))
