(ns hyperfiddle.foundation
  (:require
    #?(:cljs [cats.monad.either :as either])
    [contrib.base-64-url-safe :as base64-url-safe]
    [contrib.reactive :as r]
    [clojure.spec.alpha :as s]
    ;#?(:clj [datomic.api :as d]) ; illegal
    [hyperfiddle.api :as hf]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]
    #?(:cljs [hyperfiddle.project :as project])
    #?(:cljs [hyperfiddle.ui :as ui])
    #?(:cljs [hyperfiddle.spec.ui])
    #?(:cljs [hyperfiddle.ui.checkbox :refer [Checkbox Radio RadioGroup]])
    #?(:cljs [hyperfiddle.ui.iframe :as iframe])
    #?(:cljs [hyperfiddle.ui.staging :as staging])
    #?(:cljs [hyperfiddle.ui.util :refer [with-entity-change!]])
    #?(:cljs [hyperfiddle.view.keyboard :as k])
    #?(:cljs [hyperfiddle.view.keyboard.combos :as combos])
    [hyperfiddle.view.controller :as view]))

; Old source tree, before removal of IDE:
; https://github.com/hyperfiddle/hyperfiddle/tree/aa11ec7ce636cf5554334974a575821710947cb2

(defn set-view-mode! [mode _]
  (view/set-mode! mode))

#?(:cljs
   (defn ViewModeSelector [{:keys [mode]}]
     [RadioGroup {:name      "view"
                  :options   [{:key :hypercrud.browser.browser-ui/api, :text "edn"}
                              {:key :hypercrud.browser.browser-ui/xray, :text "data"}
                              {:key :hypercrud.browser.browser-ui/user, :text "view"}]
                  :value     mode
                  :on-change (r/partial set-view-mode!)
                  :props     {:style {:display        :inline-grid
                                      :grid-auto-flow :column
                                      :grid-gap       "0.75rem"}}}]))

(defn auth-configured? [ctx]
  ; todo duplicated logic
  (let [{:keys [hyperfiddle.ide.directory/ide-domain] :as domain} (hf/domain (:runtime ctx))
        {:keys [domain]} (get-in (hf/environment domain) [:auth0 ide-domain])]
    (some? domain)))

#?(:cljs
   (defn stateless-login-url
     ([ctx]
      (stateless-login-url ctx (hf/url-encode (hf/domain (:runtime ctx)) (runtime/get-route (:runtime ctx) hf/root-pid))))
     ([ctx state]
      (let [{:keys [hyperfiddle.ide.directory/service-uri hyperfiddle.ide.directory/ide-domain] :as domain} (hf/domain (:runtime ctx))
            {:keys [domain client-id]} (get-in (hf/environment domain) [:auth0 ide-domain])
            redirect-uri (str service-uri (domain/api-path-for (hf/domain (:runtime ctx))
                                            :hyperfiddle.ide/auth0-redirect))]
        (assert domain "auth not configured, see `auth-configured?")
        (str "https://" domain "/"
          "login?"
          "client=" client-id
          "&scope=" "openid email profile"
          "&state=" (base64-url-safe/encode state)
          "&redirect_uri=" redirect-uri)))))

#?(:cljs
   (defn TopNav [{:keys [ctx]}]
     [:nav {:role  :navigation
            :style {:display     :flex
                    :align-items :center
                    :padding     "0.5rem 1rem"}}

      [:div {:style {:margin-right "0.75rem"}} [:a {:href "/"} "Home"]]
      [ViewModeSelector {:mode (:hyperfiddle.ui/display-mode ctx)}]
      [:div {:style {:flex 1}}]
      (if (hf/subject ctx)
        [:a {:href  (hf/url-encode (hf/domain (:runtime ctx)) `(hyperfiddle.foundation/account))
             :style {:text-transform :capitalize}}
         (str "👤Account")]
        (if (auth-configured? ctx)
          [:a {:href (stateless-login-url ctx)} "👤 Sign in"]))]))

#?(:cljs
   (defn frame-on-click [rt route ^js event]
     (when (and route (.-altKey event))                     ; under what circumstances is route nil?
       (let [native-event (.-nativeEvent event)
             anchor (-> (.composedPath native-event) (aget 0) (.matches "a"))
             anchor-descendant (-> (.composedPath native-event) (aget 0) (.matches "a *"))]
         (when-not (or anchor anchor-descendant)
           (.stopPropagation event)
           (js/window.open (hf/url-encode (hf/domain rt) route) "_blank"))))))

#?(:cljs
   (defn augment [ctx view-state]
     (merge ctx {:hyperfiddle.ui/display-mode    (view/mode view-state)
                 ::hf/view-change!               with-entity-change! ; ctx must be provided by the leaf view
                 :hyperfiddle.ui.iframe/on-click (r/partial frame-on-click (:runtime ctx))})))

(defn on-key-down [e]
  #?(:cljs
     (let [key  (k/keyboard-key e)
           this `on-key-down]
       (condp k/combo? key
         combos/alt-backtick (view/toggle-mode!)
         [:alt :escape]      (prn (str this " TODO: set :hyperfiddle.ide.edit/editor-open true|false in view state"))
         [:alt :enter]       (prn (str this " DEPRECATED: should refresh view content"))
         [:alt]              (view/set-alt-key-pressed! true)
         nil))))

(defn on-key-up [e]
  #?(:cljs
     (when (k/combo? [:alt] (k/keyboard-key e))
       (view/set-alt-key-pressed! false))))

#?(:cljs
   (defn Main [ctx]
     (let [view-state @view/state
           ctx (augment ctx view-state)]
       [:main {:style       {:display        :flex
                             :flex-direction :column
                             :height         "100%"
                             :cursor         (if (view/alt-key view-state) :pointer :inherit)}
               :tabIndex    "-1"
               :on-key-down (r/partial on-key-down)
               :on-key-up   (r/partial on-key-up)}
        [TopNav {:ctx ctx}]
        ; compat with ide.css
        [:div {:class "hyperfiddle hyperfiddle-ide -hyperfiddle-ide-edit"}
         [:div {:class "-hyperfiddle-ide-preview"}
          [:div {:id "root"}
           [iframe/iframe-cmp ctx
            {:class (some-> ctx :hyperfiddle.ui/display-mode name (->> (str "display-mode-")))}]]
          [staging/inline-stage ctx]]]])))

#?(:cljs
   (defn view [ctx]
     [:div {:style {:height "100%"}}
      [Main ctx]]))

(defn ^{::hf/fiddle {:shape '[:find (pull $users ?e [*]) . :in $users ?e :where [$users ?e]]}}
  account []
  #?(:clj
     (datomic.api/q                                         ; Not required to avoid naming this in deps. Get lucky for now
       '[:find (pull ?user [:db/id
                            :user/name
                            :user/email
                            :user/last-seen
                            :user/sub
                            :user/picture
                            :user/user-id])
         .
         :in $ ?subject
         :where [?user :user/user-id ?subject]]
       (hf/*get-db* "$users") hf/*subject*)))               ; non-$ schema is specified in :shape

(s/fdef account :args (s/cat))

(defmethod hf/render #{:user/user-id `account}
  [ctx props]
  #?(:cljs
     [:div.hyperfiddle-input-group
      [:div.input (pr-str (hf/data ctx))]]))

(defmethod hf/render-fiddle `account [_ ctx props]
  #?(:cljs
     (let [user (hf/data ctx)]
       [:div.container-fluid props
        [:div.p
         [ui/img (:user/picture user) ctx {:class "avatar"}]]

        [:h3 "Hello, " (:user/name user) "!"]

        [:div.p
         [:form {:action "/logout" :method "post"}
          (str "Last login was " (or (some-> (:user/last-seen user) .toLocaleDateString) "–") ".")

          [:button.btn.btn-link {:type  "submit"
                                 :style {:display        "inline"
                                         :padding        "0"
                                         :margin-left    ".25em"
                                         :font-size      "inherit"
                                         :vertical-align "inherit"}}
           "logout"]]
         ]

        [:div.p [:div {:style {:margin-bottom "1em"}}]]
        #_[ui/field [:user/user-id] ctx]])))                ; should infer $users from :shape but is failing with missing schema

(defn ^::hf/fiddle please-login [_redirect])

(defmethod hf/render-fiddle `please-login [_ ctx props]
  #?(:cljs
     (let [[_ redirect] @(:hypercrud.browser/route ctx)
           _ (println redirect)
           state (hf/url-encode (hf/domain (:runtime ctx)) redirect)
           href (stateless-login-url ctx state)]
       [:div
        [:br]
        [:center [:h1 "Please " [:a {:href href} "login"]]]])))
