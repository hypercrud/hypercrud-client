(ns hyperfiddle.runtime-impl
  (:require
    [cats.labs.promise]
    [cats.monad.either :as either]
    [clojure.spec.alpha :as s]
    [contrib.reactive :as r]
    [hyperfiddle.api :as hf]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.state :as state]
    [promesa.core :as p]))


(s/def ::message string?)
(s/def ::data any?)

(defn hydrate-impl [rt pid request]
  (r/fmap-> (r/cursor (hf/state rt) [:hyperfiddle.runtime/partitions pid :ptm])
            (get request (either/right [nil {::message "Loading" ::data {:request request}}]))))


(defn set-route [rt pid route force-hydrate]
  (let [current-route (get-in @(hf/state rt) [:hyperfiddle.runtime/partitions pid :route])]
    (state/dispatch! rt [:stage-route pid (vec route)]) ; vec for positional update
    (if (and (not force-hydrate) (= route current-route))
      ; just update state without re-hydrating
      (p/resolved nil)
      (runtime/bootstrap-data rt pid runtime/LEVEL-GLOBAL-BASIS))))
