(ns hyperfiddle.ide
  (:require
    [contrib.base-64-url-safe :as base64-url-safe]
    [hyperfiddle.api :as hf]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]))

(defn stateless-login-url
  ([ctx]
   (stateless-login-url ctx (hf/url-encode (hf/domain (:runtime ctx)) (runtime/get-route (:runtime ctx) "root"))))
  ([ctx state]
   (let [{:keys [hyperfiddle.ide.directory/service-uri hyperfiddle.ide.directory/ide-domain] :as domain}
         ,, (hf/domain (:runtime ctx))
         {:keys [domain client-id]} (get-in (hf/environment domain) [:auth0 ide-domain])
         redirect-uri               (str service-uri (domain/api-path-for (hf/domain (:runtime ctx))
                                                                          :hyperfiddle.ide/auth0-redirect))]
     (str "https://" domain "/"
          "login?"
          "client=" client-id
          "&scope=" "openid email profile"
          "&state=" (base64-url-safe/encode state)
          "&redirect_uri=" redirect-uri))))
