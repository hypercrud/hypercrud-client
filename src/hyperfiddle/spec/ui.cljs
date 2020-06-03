(ns hyperfiddle.spec.ui
  (:require [hyperfiddle.ui :as ui]))

(defmulti ui (fn [_ctx _extras node] (:type node)))

(defmethod ui :predicate [ctx extras {:keys [name]}]
  [ui/route-input ctx (get extras name)])

(defmethod ui :keys [ctx extras {:keys [children]}]
  (into [:<>] (map (partial ui ctx extras) children)))

