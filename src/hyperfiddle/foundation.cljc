(ns hyperfiddle.foundation
  (:require
    #?(:cljs [cats.monad.either :as either])
    [contrib.base-64-url-safe :as base64-url-safe]
    [contrib.reactive :as r]
    [hyperfiddle.api :as hf]
    [hyperfiddle.def :as hf-def]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]
    #?(:cljs [hyperfiddle.project :as project])
    #?(:cljs [hyperfiddle.ui :as ui])
    #?(:cljs [hyperfiddle.spec.ui])
    #?(:cljs [hyperfiddle.ui.checkbox :refer [Checkbox Radio RadioGroup]])
    #?(:cljs [hyperfiddle.ui.iframe :as iframe])
    #?(:cljs [hyperfiddle.ui.staging :as staging])
    #?(:cljs [hyperfiddle.view.keyboard :as k])
    #?(:cljs [hyperfiddle.view.keyboard.combos :as combos])
    [hyperfiddle.view.controller :as view]
    hyperfiddle.blocks.account
    ))

(def root-pid "root")

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

(defn stateless-login-url
  ([ctx]
   (stateless-login-url ctx (hf/url-encode (hf/domain (:runtime ctx)) (runtime/get-route (:runtime ctx) "root"))))
  ([ctx state]
   (let [{:keys [hyperfiddle.ide.directory/service-uri hyperfiddle.ide.directory/ide-domain] :as domain} (hf/domain (:runtime ctx))
         {:keys [domain client-id]} (get-in (hf/environment domain) [:auth0 ide-domain])
         redirect-uri               (str service-uri (domain/api-path-for (hf/domain (:runtime ctx))
                                                       :hyperfiddle.ide/auth0-redirect))]
     (assert domain "auth not configured, see `auth-configured?")
     (str "https://" domain "/"
       "login?"
       "client=" client-id
       "&scope=" "openid email profile"
       "&state=" (base64-url-safe/encode state)
       "&redirect_uri=" redirect-uri))))

#?(:cljs
   (defn TopNav [{:keys [ctx]}]
     [:nav {:role  :navigation
            :style {:display     :flex
                    :align-items :center
                    :padding     "0.5rem 1rem"}}

      [ViewModeSelector {:mode (:hyperfiddle.ui/display-mode ctx)}]
      [:div {:style {:flex 1}}]
      (if (hf/subject ctx)
        [:a {:href  "/:hyperfiddle.blocks.account!account"
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
        ;; Below nesting and class lists is meant to preserve IDE CSS rules.
        ;; They are complex and valuable enough to justify this patch.
        ;; @see ide.css
        [:div {:class "hyperfiddle hyperfiddle-ide -hyperfiddle-ide-edit"}
         [:div {:class "-hyperfiddle-ide-preview"}
          [:div {:id "root"}
           [iframe/iframe-cmp ctx
            {:class (some-> ctx :hyperfiddle.ui/display-mode name (->> (str "display-mode-")))}]]
          [staging/inline-stage ctx]]]])))

#?(:cljs
   (defn view [ctx]
     [:div {:style {:height "100%"}}
      [:style {:dangerouslySetInnerHTML {:__html (:project/css (hyperfiddle.runtime/get-project (:runtime ctx) (:partition-id ctx)))}}]
      (-> (hyperfiddle.runtime/get-project (:runtime ctx) (:partition-id ctx))
          :project/code
          project/eval-domain-code!+
          (either/branch
           (fn [e] [:div [:h2 {:style {:margin-top "10%" :text-align "center"}} "Misconfigured domain"]])
           (fn [_]
             [Main ctx])))]))

(hf-def/fiddle :hyperfiddle.ide/please-login
  :renderer
  (let [tunneled-route (first (:hyperfiddle.route/datomic-args @(:hypercrud.browser/route ctx)))
        state          (hyperfiddle.api/url-encode (hyperfiddle.api/domain (:runtime ctx)) tunneled-route)
        href           (hyperfiddle.foundation/stateless-login-url ctx state)]
    [:div
     [:br]
     [:center [:h3 "Please " [:a {:href href} "login"]]]]))
