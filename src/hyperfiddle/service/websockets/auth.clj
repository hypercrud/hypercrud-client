(ns hyperfiddle.service.websockets.auth
  "
  SSE and WebSockets might not send cookies alongside the upgrade request. The
  simplest way to authenticate SSE or WS is to pass a token as a query
  parameter. This has many drawbacks and is considered bad practice (tokens
  might get logged, seen by reverse proxies, …, end up in links.). To mitigate
  these risks, a temporary unique token can be generated and associated to the
  real one, server-side.

  1. Using HTTP, authenticate -> get a cookie
  2. Using Ajax, exchange your cookie for a short-lived unique token T
  3. Connect via WS or SSE using T as a query parameter.
  4. The server will exchange T for the original cookie value.

  Drawbacks:
  - Beware of load balancers, this is stateful and forces sticky sessions.
  - If you’ve got a cluster and want your api to be stateless, store temporary
  tokens in a distributed store.
  "
  (:require [clojure.core.cache.wrapped :as w])
  (:import java.util.UUID))

(def ^:private tokens (w/ttl-cache-factory {} :ttl 30000)) ;; 30s in case of crazy latency between 2. and 3.

(defn generate-token! [jwt]
  (when jwt
    (let [t (UUID/randomUUID)]
      (w/miss tokens t jwt)
      t)))

(defn exchange-token [request]
  (when-let [token (some->> (get-in request [:parameterMap "token"])
                            (first)
                            (not-empty)
                            (UUID/fromString))]
    (when-let [jwt (w/lookup tokens token)]
      (w/evict tokens token)
      jwt)))
