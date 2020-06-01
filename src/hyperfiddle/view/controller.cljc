(ns hyperfiddle.view.controller
  (:require [contrib.reactive :as r]))

(def initial-state {:mode            :hypercrud.browser.browser-ui/user
                    :alt-key-pressed false})

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


(defn set-alt-key-pressed [pressed? state]
  (assoc state :alt-key-pressed pressed?))

(defn set-alt-key-pressed! [pressed?]
  (swap! state (partial set-alt-key-pressed pressed?)))

(defn alt-key [state]
  (get state :alt-key-pressed))
