(ns hyperfiddle.view.controller
  (:require [contrib.reactive :as r]))

(def initial-state {:mode :hypercrud.browser.browser-ui/user})

(def state (r/atom initial-state))

(defn set-mode [mode state]
  (assoc state :mode mode))

(defn set-mode! [mode]
  (swap! state (partial set-mode mode)))

(defn mode [state]
  (get state :mode))

(defn mode= [m state]
  (= m (mode state)))
