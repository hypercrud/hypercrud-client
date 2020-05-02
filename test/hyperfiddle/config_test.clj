(ns hyperfiddle.config-test
  (:require
    [clojure.test :refer [deftest is]]
    [hyperfiddle.config]
    [clojure.spec.alpha :as s]))


(def config
  {:hostname "localhost"                                    ; legacy key
   :domain
   {:databases
    {"$" {:database/uri #uri "datomic:mem://localhost:4334/swing-subs"}
     "$hyperfiddle" {:database/uri #uri "datomic:mem://localhost:4334/hyperfiddle"}}}})

(deftest config-backwards-compat
  (let [config (hyperfiddle.config/get-config
                 config
                 {:git/describe "618ffb1-dirty"}
                 (constantly true))                         ; disable validation
        ; Backwards compat for swing_hyperfiddle.edn :hostname key
        config (assoc config :host (:hostname config (:host config)))]
    ; validate fixed config
    (is (s/valid? :hyperfiddle.config/config config)))
  )
