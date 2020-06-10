(ns hyperfiddle.def.ide
  (:require
   [hyperfiddle.def :as hf-def]))

(hf-def/fiddle :hyperfiddle.ide/please-login
  :renderer
  (let [tunneled-route (first (:hyperfiddle.route/datomic-args @(:hypercrud.browser/route ctx)))
        state          (hyperfiddle.api/url-encode (hyperfiddle.api/domain (:runtime ctx)) tunneled-route)
        href           (hyperfiddle.ide/stateless-login-url ctx state)]
    [:div
     [:br]
     [:center [:h3 "Please " [:a {:href href} "login"]]]]))
