(ns hyperfiddle.security.domains
  (:require
    [hyperfiddle.security :as security]
    [hyperfiddle.security.client :as client-sec])
  #?(:clj
     (:import
       (java.io FileNotFoundException))))

#?(:clj (try (require 'hyperfiddle.security.entity-ownership) (catch Exception e)))

#?(:clj
   (def server
     {:process-tx (fn [& args]                              ; $ domain dbname subject tx
                    (let [f (try
                              (require 'hyperfiddle.security.entity-ownership)
                              (resolve 'hyperfiddle.security.entity-ownership/write-domains)
                              (catch Exception e
                                (throw (ex-info "Entity ownership unsupported with datomic client" {} e))))]
                      (apply f args)))}))

(def client client-sec/entity-ownership)
