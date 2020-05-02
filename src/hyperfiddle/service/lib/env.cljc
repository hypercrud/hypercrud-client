(ns hyperfiddle.service.lib.env
  (:require
    #?(:cljs [cljs.nodejs :as node])
    [clojure.string :as string]
    [contrib.data :refer [update-existing]]
    [contrib.uri :refer [->URI]]
    #?(:cljs [goog.object :as object]))
  #?(:clj
     (:import [clojure.lang ILookup])))


; These are constants loaded when the process starts. Restart the process to change them.
(defn get-env
  ([]
   (get-env #?(:clj  (let [raw-env (System/getenv)]
                       (reify
                         ILookup
                         (valAt [o k] (.get raw-env k))
                         (valAt [o k not-found] (or (.get raw-env k) not-found))))
               :cljs (let [raw-env (.-env node/process)]
                       (reify
                         ILookup
                         (-lookup [o k] (object/get raw-env k))
                         (-lookup [o k not-found] (object/get raw-env k not-found)))))))
  ([raw-env]
   (let [required #{:HF_HOSTNAMES
                    :NODE_PORT}
         optional #{:ANALYTICS :HF_ALIAS_HOSTNAMES
                    :DOMAINS_API_URI
                    :DIRECTORY_URI_OR_DB_NAME
                    :DOMAINS_USER_ID

                    :SENTRY_DSN :SENTRY_ENV
                    :AWS_ACCESS_KEY_ID :AWS_SECRET_KEY}
         env (-> (->> (concat required optional)
                      (reduce (fn [acc k]
                                (assoc acc k (get raw-env (name k))))
                              {}))
                 (update :ANALYTICS #(not= % "false"))      ; todo this check is garbage
                 (update :HF_HOSTNAMES #(string/split % #";"))

                 (update :DOMAINS_API_URI ->URI)
                 (update :DIRECTORY_URI_OR_DB_NAME #(if (.startsWith % "datomic:") (->URI %) %))


                 (update-existing :HF_ALIAS_HOSTNAMES #(string/split % #";")))]
     (doseq [v required]
       (assert (not (nil? (get env v))) (str "Environment variable for '" v "' not found")))
     env)))
