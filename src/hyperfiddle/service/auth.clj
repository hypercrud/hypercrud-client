(ns hyperfiddle.service.auth
  (:require [bidi.bidi :as bidi]
            [contrib.data :as data]
            [datomic.api :as d]
            [hyperfiddle.api :as hf]
            [hyperfiddle.io.core :as io]
            [hyperfiddle.service.routes :as routes]
            [hyperfiddle.service.websockets.auth :as wsauth]
            [promesa.core :as p]
            [taoensso.timbre :as log])
  (:import com.auth0.client.auth.AuthAPI
           com.auth0.jwt.algorithms.Algorithm
           com.auth0.jwt.exceptions.JWTVerificationException
           com.auth0.jwt.JWT
           [java.util Date UUID]))

;;;;;;;;;;;;;;;;;
;; ACCESS INFO ;;
;;;;;;;;;;;;;;;;;

(defn subject [context] (get-in context [:auth ::subject]))

(defn configured? [context]
  (some-> context :auth ::provider not-empty some?))

(defn authenticated? [context]
  (and (configured? context)
       (some? (:user-id (subject context)))))

;;;;;;;;;;;;;;;;;;;
;; DECODE TOKENS ;;
;;;;;;;;;;;;;;;;;;;

(defn issuer [auth0-domain]
  (str "https://" auth0-domain "/"))

(defn verifier [{:keys [:auth0/client-secret :auth0/domain]}]
  (let [jwt-verifier (-> (Algorithm/HMAC256 client-secret)
                         (JWT/require)
                         (.withIssuer (issuer domain))
                         (.build))]
    (fn [token]
      (-> (.verify jwt-verifier token)
          (.getClaims)
          (->> (into {})
               (data/map-values #(.as % Object))
               (data/keywordize-keys))))))

;;;;;;;;;;;;;;;;;;;
;; CREATE TOKENS ;;
;;;;;;;;;;;;;;;;;;;

(defn sign [claims secret]
  (let [jwt-builder (JWT/create)]
    (doseq [[k v] claims]
      (.withClaim jwt-builder (name k) v))
    (.sign jwt-builder (Algorithm/HMAC256 secret))))

;;;;;;;;;;;;;;;;;;;;
;; AUTHENTICATION ;;
;;;;;;;;;;;;;;;;;;;;

(defn authenticate [provider request]
  (try
    (let [verify                          (verifier provider)
          ;; Authentication Bearer header have precedence over cookies
          jwt                             (or (some->> (get-in request [:headers "authorization"])
                                                       (re-find #"^Bearer (.+)$")
                                                       (second))
                                              (get-in request [:cookies "jwt" :value])
                                              (wsauth/exchange-token request))
          {:keys [:user-id :email :name]} (some-> jwt (verify))
          user-id                         (some-> user-id (UUID/fromString))]
      {:user-id user-id
       :email   email
       :name    name
       :jwt     jwt})
    (catch JWTVerificationException e
      (log/error e)
      nil
      ;; TODO change to an interceptor and terminate the request here.
      #_(assoc :response {:status  401
                          :cookies {"jwt" (-> (cookie/jwt-options-pedestal cookie-domain)
                                              (assoc :value jwt-cookie
                                                     :expires "Thu, 01 Jan 1970 00:00:00 GMT"))}
                          :body    (ex-info (ex-message e) {} e) ; don't leak cause and data
                          }))))

(defn build-auth-context [config request]
  (let [provider (:auth0 config)]
    {::provider provider
     ::subject  (authenticate provider request)}))


;;;;;;;;;;;;;;;;
;; OAuth Flow ;;
;;;;;;;;;;;;;;;;

(defn- query-subject [jwt-sub]
   (d/pull (hf/get-db "$users")
           '[:db/id :user/user-id :user/created-date]
           [:user/sub jwt-sub]))

(defn fetch-id-token [config service-uri oauth-authorization-code]
  (let [{:keys [auth0/domain auth0/client-id auth0/client-secret]} (:auth0 config)
        redirect-path                                              (bidi/path-for routes/routes :hyperfiddle.api/auth0-redirect)
        _                                                          (assert redirect-path)
        redirect-uri                                               (str service-uri redirect-path)
        auth                                                       (AuthAPI. domain client-id client-secret)
        req                                                        (.exchangeCode auth oauth-authorization-code redirect-uri)]
    (p/future (-> req (.execute) (.getIdToken)))))

(defn login! [{:keys [auth0] :as config} service-uri io oauth-authorization-code]
  {:pre [config service-uri io oauth-authorization-code]}
  ;;(assert false "die before login cookie set")
  (-> (fetch-id-token config service-uri oauth-authorization-code)
      (p/then (fn [encoded-id-token]
                (let [id-token    (let [verify (verifier auth0)] (verify encoded-id-token))
                      basis       @(io/sync io #{"$" "$users"})
                      user-record @(->> `((query-subject ~(:sub id-token)) ~hf/root-pid)
                                        (io/hydrate-one! io basis {hf/root-pid {:is-branched true}}))
                      user-id     (:user/user-id user-record (UUID/randomUUID))
                      now         (Date.)]
                  (io/transact! io {"$users" [(cond-> {:user/name      (:nickname id-token)
                                                       :user/sub       (:sub      id-token)
                                                       :user/email     (:email    id-token)
                                                       :user/picture   (:picture  id-token)
                                                       :user/last-seen now
                                                       :user/user-id   user-id}
                                                (nil? (:user/created-date user-record)) (assoc :user/created-date now))]})
                  [(-> (assoc id-token :user-id (str user-id))
                       (sign (:auth0/client-secret auth0)) ; todo maybe use a different secret to sign
                       )
                   (- (:exp id-token)
                      (quot (System/currentTimeMillis) 1000))])))))

(defn cookie [{:keys [domain value max-age]}]
  {:http-only true
   :domain    domain
   :path      "/"
   :value     value
   :max-age   max-age})
