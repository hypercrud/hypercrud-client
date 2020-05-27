(ns hyperfiddle.view.controller
  (:require [contrib.reactive :as r]))

(def initial-state {:mode :hypercrud.browser.browser-ui/user})

(def state (r/atom initial-state))

(defn set-mode [mode state]
  (assoc state :mode mode))

(defn set-mode! [mode]
  (swap! state (partial set-mode mode)))

(defn toggle-mode! []
  (swap! state update :mode #(if (= % :hypercrud.browser.browser-ui/user)
                               :hypercrud.browser.browser-ui/xray
                               :hypercrud.browser.browser-ui/user)))

(defn mode [state]
  (get state :mode))

(defn mode= [m state]
  (= m (mode state)))
