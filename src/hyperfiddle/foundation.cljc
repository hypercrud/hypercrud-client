(ns hyperfiddle.foundation
  #?@
   (:clj
    [(:require
      [clojure.spec.alpha :as s]
      [hyperfiddle.api :as hf]
      [hyperfiddle.view.controller :as view])]
    :cljs
    [(:require
      [bidi.bidi :as bidi]
      [clojure.spec.alpha :as s]
      [contrib.base-64-url-safe :as base64-url-safe]
      [contrib.reactive :as r]
      [hyperfiddle.api :as hf]
      [hyperfiddle.route :as route]
      [hyperfiddle.service.routes :as routes]
      [hyperfiddle.runtime :as runtime]
      [hyperfiddle.ui :as ui]
      [hyperfiddle.ui.checkbox :refer [RadioGroup]]
      [hyperfiddle.ui.iframe :as iframe]
      [hyperfiddle.ui.staging :as staging]
      [hyperfiddle.ui.util :refer [with-entity-change!]]
      [hyperfiddle.view.controller :as view]
      [hyperfiddle.view.keyboard :as k]
      [hyperfiddle.view.keyboard.combos :as combos])]))

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
                  :props     {:class "hyperfiddle-view-mode-selector"}}]))

(defn auth-configured? [{:keys [runtime]}]
  ;; todo duplicated logic
  (some? (get-in (hf/config runtime) [:auth0 :domain])))

#?(:cljs
   (defn stateless-login-url
     ([ctx]
      (stateless-login-url ctx (route/url-encode (runtime/get-route (:runtime ctx) hf/root-pid) (:home-route (hf/config (:runtime ctx))))))
     ([ctx state]
      (let [{:keys [:auth0/domain :auth0/client-id]} (:auth0 (hf/config (:runtime ctx)))
            redirect-uri                             (str (.. js/document -location -origin)
                                                          (bidi/path-for routes/routes :hyperfiddle.api/auth0-redirect))]
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
                    :padding     "0.5rem 1rem"}}

      [:a {:href "/" :style {:margin-right "0.75rem"
                             :display :flex
                             :align-items :center}} "Home"]
      [:div {:style {:display :flex
                     :align-items :center}}
       [ViewModeSelector {:mode (:hyperfiddle.ui/display-mode ctx)}]]
      [:div {:style {:flex 1
                     :margin "0 0.75rem"}}
       [iframe/route-editor-topnav ctx]]
      [:div {:style {:display :flex
                     :align-items :center}}
       (if (hf/subject ctx)
         [:a {:href  (route/url-encode (or (:account-route (hf/config (:runtime ctx)))
                                           `(hyperfiddle.foundation/account))
                                       (:home-route (hf/config (:runtime ctx))) )
              :style {:text-transform :capitalize}}
          (str "👤Account")]
         (if (auth-configured? ctx)
           [:a {:href (stateless-login-url ctx)} "👤 Sign in"]))]]))

#?(:cljs
   (defn frame-on-click [rt route ^js event]
     (when (and route (.-altKey event))                     ; under what circumstances is route nil?
       (let [native-event (.-nativeEvent event)
             anchor (-> (.composedPath native-event) (aget 0) (.matches "a"))
             anchor-descendant (-> (.composedPath native-event) (aget 0) (.matches "a *"))]
         (when-not (or anchor anchor-descendant)
           (.stopPropagation event)
           (js/window.open (route/url-encode route nil) "_blank"))))))

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
      (hf/get-db "$users") hf/*subject*)))               ; non-$ schema is specified in :shape

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
         [ui/img (:user/picture user) ctx {:class "hyperfiddle-account-avatar"}]]

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
           _            (println redirect)
           state        (route/url-encode redirect (:home-route (hf/config (:runtime ctx))))
           href         (stateless-login-url ctx state)]
       [:div
        [:br]
        [:center [:h1 "Please " [:a {:href href} "login"]]]])))
