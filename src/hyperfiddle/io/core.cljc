(ns hyperfiddle.io.core
  #?(:clj (:refer-clojure :exclude [sync]))
  (:require
    [cats.core :as cats]
    [cats.monad.either :as either]
    [promesa.core :as p]
    [contrib.performance :as perf]
    [hyperfiddle.api :as hf]
    [taoensso.timbre :as timbre]))


(defprotocol IO
  (global-basis [io])
  (hydrate-requests [io local-basis partitions requests])
  (hydrate-route [io local-basis route pid partitions])
  (local-basis [io global-basis route])
  (sync [io dbnames])
  (transact! [io tx-groups]))

(defn hydrate-one! [io local-basis partitions request]
  (-> (hydrate-requests io local-basis partitions [request])
      (p/then (fn [{:keys [pulled-trees]}] (either/branch (first pulled-trees) p/rejected p/resolved)))))

; Promise[List[Response]]
(defn hydrate-all-or-nothing! [io local-basis partitions requests]
  (if (empty? requests)
    (p/resolved nil)
    (-> (hydrate-requests io local-basis partitions requests)
        (p/then (fn [{:keys [pulled-trees]}] (either/branch (cats/sequence pulled-trees) p/rejected p/resolved))))))

(defn global-basis-for [io domain]
  (perf/time-promise
    (->> (hf/databases domain) keys set
      (sync io)
      (cats/fmap (fn [user-basis] {:domain {:t (hf/basis domain)
                                            :hash (hash {:fiddle-dbname (hf/fiddle-dbname domain)
                                                         :databases (->> (hf/databases domain)
                                                                      (map (fn [[dbname database]]
                                                                             ; todo switching between peer/client will break this hash
                                                                             [dbname (select-keys database [:database/uri :database/db-name])]))
                                                                      (into {}))
                                                         :environment (hf/environment domain)
                                                         :type-name (hf/type-name domain)})}
                                   :user user-basis})))
    (fn [err total-time]
      (timbre/debugf "global-basis failure; total time: %sms" total-time))
    (fn [success total-time]
      (timbre/debugf "global-basis; total time: %sms" total-time))))

(defn local-basis-for [io global-basis route]
  (:user global-basis))
