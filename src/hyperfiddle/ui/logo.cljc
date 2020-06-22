(ns hyperfiddle.ui.logo)

(defn logo
  [props]
  [:div.hyperfiddle-logo props
   [:div.hyperfiddle-logo-square-wrapper
    [:div.hyperfiddle-logo-square]]
   [:div.hyperfiddle-logo-square-wrapper {:delay "true"}
    [:div.hyperfiddle-logo-square]]])
