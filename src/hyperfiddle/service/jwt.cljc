(ns hyperfiddle.service.jwt
  (:require
    #?(:clj [clojure.walk :as walk])
    #?(:clj [contrib.data :as data :refer [map-values]])
    #?(:cljs [jsonwebtoken :as jwt])                        ; nodejs only
    [taoensso.timbre :as timbre])
  #?(:clj
     (:import (com.auth0.jwt JWT)
              (com.auth0.jwt.algorithms Algorithm))))

(defn auth0-domain->issuer [auth0-domain]
  (str "https://" auth0-domain "/"))

(defn build-verifier [secret auth0-domain]
  #?(:clj  (let [jwt-verifier (-> (Algorithm/HMAC256 secret)
                                  (JWT/require)
                                  (.withIssuer (auth0-domain->issuer auth0-domain))
                                  (.build))]
             (fn [token]
               (-> (.verify jwt-verifier token)
                   (.getClaims)
                   (->> (into {})
                        (map-values #(.as % Object))
                        (data/keywordize-keys)))))
     :cljs (fn [token]
             (some-> (.verify jwt token secret)
                     (js->clj :keywordize-keys true)))))

(defn sign [claims secret & [options]]
  #?(:clj  (let [jwt-builder (JWT/create)]
             (doseq [[k v] claims]
               (.withClaim jwt-builder (name k) v))
             (.sign jwt-builder (Algorithm/HMAC256 secret)))
     :cljs (.sign jwt (clj->js claims) secret (clj->js options))))
