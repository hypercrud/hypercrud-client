(ns hyperfiddle.view.controller
  (:require [contrib.reactive :as r]))

(def initial-state {:view-mode :hypercrud.browser.browser-ui/user})

(def state (r/atom initial-state))

(defn set-view-mode [view-mode state]
  (assoc state :view-mode view-mode))

(defn set-view-mode! [view-mode]
  (swap! state (partial set-view-mode view-mode)))

(defn view-mode [state]
  (get state :view-mode))
