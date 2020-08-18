(ns hyperfiddle.ui.iframe
  (:require
    [clojure.spec.alpha :as s]
    [contrib.css :refer [css css-slugify]]
    [contrib.pprint :refer [pprint-str pprint-async]]
    [contrib.reactive :as r]
    [contrib.reader :as reader]
    [contrib.ui]
    [contrib.ui.safe-render :refer [user-portal]]
    [hypercrud.browser.base :as base]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.error :as ui-error]
    [hyperfiddle.ui.stale :as stale]
    [hyperfiddle.api :as hf]
    [hyperfiddle.spec :as spec]
    [taoensso.timbre :as timbre]
    [clojure.string :as str]))


(defn auto-ui-css-class [?ctx]                              ; semantic css
  (let [ident (some-> (:hypercrud.browser/fiddle ?ctx) (r/cursor [:fiddle/ident]) deref)]
    (->> ["hyperfiddle"
          (css-slugify (some-> ident namespace))
          (css-slugify ident)]
         (apply css))))


(defn- render-fiddle
  "Proxy to the `hf/render-fiddle` multimethod. Since `hf/render-fiddle` is a
  multimethod, you want to call this function instead to ensure reagent will
  react properly on props change. See
  https://stackoverflow.com/questions/33299746/why-are-multi-methods-not-working-as-functions-for-reagent-re-frame"
  [val ctx props]
  [hf/render-fiddle val ctx props])

(defn- fiddle-renderer-cmp [value ctx props & bust-component-did-update]
  (let [props props #_(select-keys props [:class :initial-tab :on-click #_:disabled])]
    (case (:hyperfiddle.ui/display-mode ctx)

      :hypercrud.browser.browser-ui/user
      [:<>
       (if-let [user-renderer (:user-renderer props)] ; validate qfind and stuff?
         [user-renderer value ctx props]
         [render-fiddle value ctx props])]

      :hypercrud.browser.browser-ui/xray
      [:<>
       (if-let [user-renderer (:user-renderer props)]
                                        ; don't use r/partial with user-renderer, r/partial useful for args, not components
                                        ; Select user-renderer is valid in xray mode now
         [user-renderer value ctx props]
         [hyperfiddle.ui/fiddle-xray value ctx props])]

      :hypercrud.browser.browser-ui/api
      [hyperfiddle.ui/fiddle-api value ctx props])))

(defn- ui-comp [ctx & [props]]                              ; user-renderer comes through here
  (let [value @(:hypercrud.browser/result ctx)              ; TODO remove this, make them ask
        props (update props :class css (auto-ui-css-class ctx))
        ; The reactdom component should filter the keys at the last second.
        ; https://github.com/hyperfiddle/hyperfiddle/issues/698
        display-mode (:hyperfiddle.ui/display-mode ctx)
        error-props (-> (select-keys props [:class :on-click])
                        (update :class css "hyperfiddle-error"))]
    ^{:key (str display-mode)}
    [user-portal (ui-error/error-comp ctx) error-props
     ; If userland crashes (fiddle/renderer OR cljs-ns), reactions don't take hold, we need to reset here.
     ; Cheaper to pass fiddle-value as a prop than to hash everything
     [fiddle-renderer-cmp value ctx props @(:hypercrud.browser/fiddle ctx)]]))

(defn- drop-tail-while [f xs]
  (reverse (drop-while f (reverse xs))))

(defn- get-route [rt pid default?]
  (let [route (if default?
                (runtime/get-route-defaults rt pid)
                (runtime/get-route rt pid))]
    (when-let [error (s/explain-data :hyperfiddle/route route)]
      (js/console.error "Invalid route" (::s/problems error)))
    (seq route)))

(defn set-route [rt pid route]
  (hf/set-route rt pid route))

(defn route-editor
  ([{rt  :runtime
     pid :partition-id
     :as ctx}]
   [route-editor (get-route rt pid false) (get-route rt pid true) (r/partial set-route rt pid)])
  ([route default on-change]
   (let [parse-string (fn [s]
                        (let [route (seq (reader/read-edn-string! s))]
                          (try
                            (s/assert :hyperfiddle/route route)
                            (catch :default err
                              (js/console.error "Invalid route" err)
                              (throw err)))
                          route))
         to-string    pprint-str]
     [:<>
      #_[contrib.ui/debounced
       {:value             route
        :debounce/interval 500
        :on-change         (fn [o n] (when-not (= o n) (on-change n)))
        :mode              "clojure"
        :lineNumbers       false}
       contrib.ui/validated-cmp parse-string to-string contrib.ui/code]
      [contrib.ui/async-code {:value     default
                              :read-only true
                              :class     "foo"}]])))

(defn stale-browse [ctx error success & args]
  [stale/loading
   (r/track runtime/loading? (:runtime ctx) (:partition-id ctx))
   (base/browse-partition+ ctx)
   (fn [e] (timbre/error "error in render" (.-stack e))
           (into [error ctx e] args))
   (fn [ctx] (into [success ctx] args))
   (fn [ctx] (into [success ctx] args))])

(defn- browse-error [{rt :runtime pid :partition-id :as ctx} e props]
  (let [error-comp (or (:hyperfiddle.ui/error-render-custom props) ; try props first
                       (ui-error/error-comp ctx))]
    [error-comp e (cond-> {:class (css "hyperfiddle-error" (:class props) "ui")}
                    (::on-click ctx) (assoc :on-click (r/partial (::on-click ctx) (runtime/get-route rt pid))))]))

(defn- browse-success [ctx props]
  [ui-comp ctx (-> props
                   (dissoc :hyperfiddle.ui/error-render-custom)
                   (update :class css "ui")
                   (cond->
                     ; @route here is a broken reaction, see routing/route+ returns a severed reaciton
                     ; use the ctx's route, otherwise alt clicking while loading could take you to the new route, which is jarring
                     (::on-click ctx) (assoc :on-click (r/partial (::on-click ctx) @(:hypercrud.browser/route ctx)))))])

(defn iframe-cmp [ctx & [props]]
  [:<> {:key (:partition-id ctx)}
   (when-not (= :hypercrud.browser.browser-ui/user (:hyperfiddle.ui/display-mode ctx))
     [route-editor ctx])
   ; Route is related from pid lookup in rt internals
   [stale-browse ctx browse-error browse-success props]])
