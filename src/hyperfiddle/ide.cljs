(ns hyperfiddle.ide
  (:require
    [cats.monad.either :as either]
    [clojure.spec.alpha :as s]
    [contrib.base-64-url-safe :as base64-url-safe]
    [contrib.data :refer [unqualify]]
    [contrib.ednish :as ednish]
    [contrib.reader :as reader]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.fiddle]                                    ; specs
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.runtime]
    [hyperfiddle.ui.staging]

    ; pull in the entire ide app for reference from user-land
    [hyperfiddle.ide.edit]
    [hyperfiddle.ide.fiddles.fiddle-src]
    [hyperfiddle.ide.fiddles.schema-attribute]
    [hyperfiddle.ide.fiddles.schema-editor]
    [hyperfiddle.ide.fiddles.topnav]
    [hyperfiddle.ide.preview.view]
    [hyperfiddle.api :as hf]))


; specs for ui
(s/def :hyperfiddle/ide                                     ; !! fiddle/ident overlap with attributes
  (s/merge
    #_(s/multi-spec fiddle-type :fiddle/type)
    #_(s/or :ident (s/keys :req [:fiddle/ident])
            :uuid (s/keys :req [:fiddle/uuid]))
    (s/keys :opt [:fiddle/ident
                  :fiddle/uuid
                  :fiddle/type
                  :fiddle/links
                  :fiddle/markdown
                  :fiddle/renderer
                  :fiddle/css
                  :fiddle/cljs-ns
                  :fiddle/hydrate-result-as-fiddle
                  :hyperfiddle/owners])))
(s/def :hyperfiddle.ide/new-fiddle (s/keys :req [:fiddle/ident]))
(s/def :hyperfiddle.ide/new-link (s/keys :req [:link/path]))

(defn parse-ide-fragment [s-fragment]
  (let [fragment (some-> s-fragment ednish/decode-ednish reader/read-edn-string+ (either/branch (constantly nil) identity))]
    (when (and (keyword? fragment)
               (#{"hf.src" "hf"} (namespace fragment)))
      (keyword "hf" (unqualify fragment)))))

(defn stateless-login-url
  ([ctx] (stateless-login-url ctx (hf/url-encode (hf/domain (:runtime ctx)) (hyperfiddle.runtime/get-route (:runtime ctx) foundation/root-pid))))
  ([ctx state]
   (let [{:keys [hyperfiddle.ide.directory/service-uri
                 hyperfiddle.ide.directory/ide-domain] :as domain} (hf/domain (:runtime ctx))
         {:keys [domain client-id]} (get-in (hf/environment domain) [:auth0 ide-domain])
         redirect-uri (str service-uri (domain/api-path-for (hf/domain (:runtime ctx)) :hyperfiddle.ide/auth0-redirect))]
     (str "https://" domain "/"
          "login?"
          "client=" client-id
          "&scope=" "openid email profile"
          "&state=" (base64-url-safe/encode state)
          "&redirect_uri=" redirect-uri))))

(defn ide-stage [ctx]
  ; Could also be inlined:
  ; http://hyperfiddle.hyperfiddle.site/:hyperfiddle.ide!edit/(:fiddle!ident,:hyperfiddle.ide!edit)
  [hyperfiddle.ui.staging/inline-stage (:runtime ctx) (:partition-id ctx)
   (->> (hf/domain (:runtime ctx))
        :hyperfiddle.ide.domain/user-dbname->ide
        (map (fn [[user-dbname ide-dbname]] {:id ide-dbname
                                             :label (domain/dbname-label user-dbname)}))
        (sort-by :label))])
