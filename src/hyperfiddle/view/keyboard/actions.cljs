(ns hyperfiddle.view.keyboard.actions
  (:require [hyperfiddle.api :as hf]))

(defn frame-on-click [rt route ^js event]
  (when (and route (.-altKey event)) ;; under what circumstances is route nil?
    (let [native-event      (.-nativeEvent event)
          anchor            (-> (.composedPath native-event) (aget 0) (.matches "a"))
          anchor-descendant (-> (.composedPath native-event) (aget 0) (.matches "a *"))]
      (when-not (or anchor anchor-descendant)
        (.stopPropagation event)
        (js/window.open (hf/url-encode (hf/domain rt) route) "_blank")))))
