(ns hyperfiddle.config-test
  (:require
   [clojure.spec.alpha :as s]
   [clojure.test :refer [deftest is]]
   [hyperfiddle.config :as config]))

(def config
  {:hostname  "localhost" ; legacy key
   :databases {"$" {:database/uri #uri "datomic:mem://localhost:4334/swing-subs"}}})

(deftest config-backwards-compat
  (let [config (config/build (merge {:ident :test, :git/describe "618ffb1-dirty"} config))
        ;; Backwards compat for swing_hyperfiddle.edn :hostname key
        config (assoc config :host (:hostname config (:host config)))]
    ;; validate fixed config
    (is (s/valid? :hyperfiddle.config/config config))))
