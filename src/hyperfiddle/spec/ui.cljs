(ns hyperfiddle.spec.ui
  (:require [hyperfiddle.ui :as ui]))

(defmulti ui (fn [_ctx _extras node] (:type node)))

(defmethod ui :hyperfiddle.spec/predicate [ctx extras {:keys [name]}]
  [ui/route-input ctx (get extras name)])

(defmethod ui :hyperfiddle.spec/keys [ctx extras {:keys [children]}]
  (into [:<>] (map (partial ui ctx extras) children)))
