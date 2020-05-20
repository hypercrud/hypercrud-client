(ns hyperfiddle.ide.preview.view
  (:require
    [cats.monad.either :as either]
    [contrib.cljs-platform :refer [code-for-browser]]
    [contrib.component :as component]
    [contrib.data :refer [map-values]]
    [contrib.local-storage :as local-storage]
    [contrib.reactive :as r]
    [contrib.ui]
    [hyperfiddle.api :as hf]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.ide.domain :as ide-domain]
    [hyperfiddle.ide.preview.runtime :as preview-rt]
    [hyperfiddle.ide.routing :as ide-routing]
    [hyperfiddle.io.basis :as basis]
    [hyperfiddle.local-storage-sync :refer [map->LocalStorageSync]]
    [hyperfiddle.project :as project]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.state :as state]
    [hyperfiddle.ui.error :as error]
    [hyperfiddle.ui.iframe :as iframe]
    [hyperfiddle.ui.loading :as loading]
    [re-com.core :as re-com]
    [reagent.core :as reagent]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(def keypress (code-for-browser (js/keypress.Listener. js/document.body)))

(defn frame-on-click [rt route event]
  (when (and route (.-altKey event))                        ; under what circumstances is route nil?
    (let [native-event (.-nativeEvent event)
          anchor (-> (.composedPath native-event) (aget 0) (.matches "a"))
          anchor-descendant (-> (.composedPath native-event) (aget 0) (.matches "a *"))]
      (when-not (or anchor anchor-descendant)
        (.stopPropagation event)
        (js/window.open (hf/url-encode (hf/domain rt) route) "_blank")))))

(defn ide-partition-reference [rt ide-pid]
  @(r/fmap-> (r/cursor (state/state rt) [::runtime/partitions ide-pid])
             (select-keys [:stage :local-basis])))

(defn stale-global-basis? [rt]
  (= -1 (basis/compare-uri-maps (:user (runtime/get-global-basis rt))
                                @(r/cursor (state/state rt) [::ide-user-global-basis]))))

(defn stale-local-basis? [rt preview-pid ide-pid]
  (= -1 (basis/compare-uri-maps
          (runtime/get-local-basis rt preview-pid)
          (runtime/get-local-basis rt ide-pid))))

(defn preview-stale? [preview-rt ide-pid preview-pid preview-state]
  #_(println (pr-str {:initial-render @(r/cursor preview-state [:initial-render])
                      :staleness-not= (not= @(r/cursor preview-state [:staleness]) (ide-partition-reference rt ide-pid))
                      :stale-gb? (stale-global-basis?)
                      :stale-lb (stale-local-basis?)}))
  (and (not @(r/cursor preview-state [:initial-render]))
       (or (not= @(r/cursor preview-state [:staleness]) (ide-partition-reference preview-rt ide-pid))
           (stale-global-basis? preview-rt)
           (stale-local-basis? preview-rt preview-pid ide-pid))))

(defn refresh! [preview-rt ide-pid preview-pid preview-state]
  (when-not (or @(r/cursor preview-state [:is-refreshing])
                @(r/cursor preview-state [:initial-render]))
    (let [init-level (cond
                       (stale-global-basis? preview-rt) runtime/LEVEL-NONE
                       (stale-local-basis? preview-rt preview-pid ide-pid) runtime/LEVEL-GLOBAL-BASIS
                       :else runtime/LEVEL-LOCAL-BASIS)]
      (swap! preview-state assoc
             :staleness (ide-partition-reference preview-rt ide-pid)
             :is-refreshing true)
      (-> (runtime/bootstrap-data preview-rt preview-pid init-level)
          (p/finally (fn [] (swap! preview-state assoc :is-refreshing false)))))))

(defn preview-toolbar [preview-rt ide-pid preview-pid preview-state]
  (let [stale @(r/track preview-stale? preview-rt ide-pid preview-pid preview-state)]
    [:span.preview-toolbar (when stale {:class "hyperfiddle-preview-stale"})
     ; todo animation
     (let [is-hovering-refresh-button (r/cursor preview-state [:is-hovering-refresh-button])]
       [:button {:class "refresh" :on-click #(refresh! preview-rt ide-pid preview-pid preview-state)
                 :disabled @(r/cursor preview-state [:is-refreshing])
                 :on-mouse-over #(do (reset! is-hovering-refresh-button true) nil)
                 :on-mouse-out #(do (reset! is-hovering-refresh-button false) nil)}
        [re-com/popover-tooltip
         :showing? (r/atom (and @is-hovering-refresh-button (not @(r/cursor preview-state [:is-refreshing]))))
         :anchor "↻"
         :label "Re-compute stale preview (alt+enter)"]])
     [:span.url]
     (into [:span]
           (->> [{:label "edn" :tooltip "What the API client sees" :value :hypercrud.browser.browser-ui/api}
                 {:label "data" :tooltip "Ignore :fiddle/renderer" :value :hypercrud.browser.browser-ui/xray}
                 {:label "view" :tooltip "Use :fiddle/renderer" :value :hypercrud.browser.browser-ui/user}]
                (map (fn [props]
                       [contrib.ui/radio-with-label
                        (assoc props
                          :checked (= (:value props) @(r/cursor preview-state [:display-mode]))
                          :on-change #(swap! preview-state assoc :display-mode %))]))))]))

(defn preview-content [{rt :runtime pid :partition-id :as ctx} ide-pid preview-state]
  (if @(r/cursor preview-state [:initial-render])
    [loading/page (hf/domain rt)]
    (let [code+ (project/eval-domain-code!+ (:project/code (runtime/get-project (:runtime ctx) (:partition-id ctx))))
          is-stale @(r/track preview-stale? rt ide-pid pid preview-state)]
      [:<>
       (when (either/left? code+)
         (let [e @code+]
           (timbre/error e)
           (let [href (hf/url-encode (hf/domain rt) {::route/fiddle :hyperfiddle.ide/env})
                 message (or (some-> (ex-cause e) ex-message) (ex-message e))]
             [:h6 {:style {:text-align "center" :background-color "lightpink" :margin 0 :padding "0.5em 0"}}
              "Exception evaluating " [:a {:href href} [:code ":domain/code"]] ": " message])))
       (if-let [e (some-> (runtime/get-error rt pid))]
         [error/error-block e]
         [:div#root (merge {}
                           (when is-stale {:class "hyperfiddle-preview-stale"})
                           (when @(r/cursor preview-state [:alt-key-pressed]) {:style {:cursor "pointer"}}))
          [:style {:dangerouslySetInnerHTML {:__html (:project/css (runtime/get-project (:runtime ctx) (:partition-id ctx)))}}]
          ^{:key "user-iframe"}
          [iframe/iframe-cmp ctx
           {:class (some-> @(r/cursor preview-state [:display-mode]) name (->> (str "display-mode-")))}]])])))

(defn- ctx->ls [ctx]
  (atom (map->LocalStorageSync {:rt (:runtime ctx) :pid (:partition-id ctx) :ls-key :USER-STATE})))

;; GG: From the perspective of the user domain this is the entry point
;; emited HTTP requests can compose.
;; There is an initialization call called sync
;; called effect because it's not a pure view
;; preview is the name of the user domain
;; preview is not a verb
(def preview-effects
  (reagent/create-class
    {:reagent-render
     (fn [ctx ide-pid preview-state]
       [preview-content ctx ide-pid preview-state])

     :component-did-mount
     (fn [this]
       (let [[_ {rt :runtime preview-pid :partition-id :as ctx} ide-pid preview-state] (reagent/argv this)]
         (.register-many keypress (clj->js [{"keys" "alt"
                                             "prevent_repeat" true
                                             "on_keydown" #(swap! preview-state assoc :alt-key-pressed true)
                                             "on_keyup" #(swap! preview-state assoc :alt-key-pressed false)}
                                            {"keys" "alt escape"
                                             "on_keydown" #(swap! preview-state update :hyperfiddle.ide.edit/editor-open not)}
                                            {"keys" "alt enter"
                                             "on_keydown" #(refresh! rt ide-pid preview-pid preview-state)}
                                            {"keys" "alt `"
                                             "on_keydown" (fn []
                                                            (swap! preview-state update :display-mode
                                                                   #(if (not= :hypercrud.browser.browser-ui/user %)
                                                                      :hypercrud.browser.browser-ui/user
                                                                      :hypercrud.browser.browser-ui/xray)))}]))
         (when local-storage/is-supported
           (swap! (ctx->ls ctx) component/start))
         (-> (runtime/bootstrap-data rt preview-pid runtime/LEVEL-NONE)
             (p/finally (fn []
                          (swap! preview-state assoc
                                 :initial-render false
                                 :is-refreshing false))))
         (add-watch (state/state rt) this
                    (fn [k r o n]
                      (let [old-route (get-in o [::runtime/partitions preview-pid :pending-route])
                            new-route (get-in n [::runtime/partitions preview-pid :pending-route])]
                        (when-not (route/equal-without-frag? old-route new-route)
                          (swap! preview-state assoc :is-refreshing true)
                          (-> (runtime/bootstrap-data rt preview-pid runtime/LEVEL-GLOBAL-BASIS)
                              (p/finally (fn [] (swap! preview-state assoc
                                                       :staleness (ide-partition-reference rt ide-pid)
                                                       :is-refreshing false))))))))))

     :component-will-unmount
     (fn [this]
       (let [[_ ctx ide-pid preview-state] (reagent/argv this)]
         (when local-storage/is-supported
           (component/stop @(ctx->ls ctx)))
         (remove-watch (state/state (:runtime ctx)) this))
       (.reset keypress))
     }))

(defn- to [parent-state user-state]
  (let [user-state (-> (update user-state ::runtime/partitions dissoc foundation/root-pid) ; root branch is readonly to users
                       (dissoc ::ide-user-global-basis ::runtime/user-id)
                       state/initialize)]
    (assoc parent-state ::runtime/user-state user-state)))

(defn- from [ide-domain ide-pid parent-state]
  (let [root-partition (-> (get-in parent-state [::runtime/partitions ide-pid])
                           (select-keys [:is-branched :route :pending-route :stage :local-basis])
                           (update :local-basis (fn [local-basis] (map-values #(get local-basis %) (::ide-domain/user-dbname->ide ide-domain))))
                           (update :stage (fn [stage] (map-values #(get stage %) (::ide-domain/user-dbname->ide ide-domain)))))]
    (-> (::runtime/user-state parent-state)
        (assoc ::ide-user-global-basis (map-values #(get-in parent-state [::runtime/global-basis :user %])
                                                   (::ide-domain/user-dbname->ide ide-domain))
               ::runtime/user-id (::runtime/user-id parent-state))
        (assoc-in [::runtime/partitions foundation/root-pid] root-partition)
        (cond->
          (= :hyperfiddle.ide/edit (get-in root-partition [:route ::route/fiddle]))
          (update-in [::runtime/partitions preview-rt/preview-pid]
                     (fn [v]
                       (let [route (let [route (ide-routing/ide-route->preview-route (:route root-partition))]
                                     (cond-> route
                                       (hyperfiddle.ide/parse-ide-fragment (::route/fragment route)) (dissoc ::route/fragment)))]
                         (-> (assoc v
                               :is-branched true
                               :parent-pid ide-pid)
                             (cond->
                               (and (not (route/equal-without-frag? route (:pending-route v)))
                                    (not (route/equal-without-frag? route (:route v))))
                               (assoc :pending-route route)))))))
        state/initialize)))
