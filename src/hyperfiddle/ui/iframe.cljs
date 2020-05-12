(ns hyperfiddle.ui.iframe
  (:require
    [cats.monad.either :as either]
    [clojure.spec.alpha :as s]
    [contrib.css :refer [css css-slugify]]
    [contrib.eval-cljs :as eval-cljs]
    [contrib.pprint :refer [pprint-str]]
    [contrib.reactive :as r]
    [contrib.reader :as reader]
    [contrib.string :refer [blank->nil]]
    [contrib.ui]
    [contrib.ui.safe-render :refer [user-portal]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.error :as ui-error]
    [hyperfiddle.ui.stale :as stale]
    [hyperfiddle.api :as hf]))


(defn auto-ui-css-class [?ctx]                              ; semantic css
  (let [ident (some-> (:hypercrud.browser/fiddle ?ctx) (r/cursor [:fiddle/ident]) deref)]
    (->> ["hyperfiddle"
          (css-slugify (some-> ident namespace))
          (css-slugify ident)]
         (apply css))))

(defn- fiddle-css-renderer [s] [:style {:dangerouslySetInnerHTML {:__html @s}}])

(defn- build-wrapped-render-expr-str [user-str]
  (str "(fn [val ctx props]\n" (cond-> user-str (not (string? user-str)) (clojure.string/join "\n")) ")"))

(defn- fiddle-renderer-cmp [value ctx props & bust-component-did-update]
  (let [last-cljs-ns (atom nil)]                            ; don't spam the compiler - memoize buffer of 1
    (fn [value ctx props & bust-component-did-update]
      (let [cljs-ns @(r/fmap-> (:hypercrud.browser/fiddle ctx) :fiddle/cljs-ns blank->nil)] ; always do this
        (when (and (some? cljs-ns)
                   (not= @last-cljs-ns cljs-ns))
          (eval-cljs/eval-statement-str! 'user cljs-ns))    ; Exceptions caught at user-portal error-comp
        (reset! last-cljs-ns cljs-ns)

        (let [display-mode (or (some-> (:hyperfiddle.ui/display-mode ctx) deref)
                               :hypercrud.browser.browser-ui/user)
              props props #_(select-keys props [:class :initial-tab :on-click #_:disabled])]
          (case display-mode

            :hypercrud.browser.browser-ui/user
            [:<>
             (if-let [user-renderer (:user-renderer props)] ; validate qfind and stuff?
               [user-renderer value ctx props]
               (-> @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/renderer])
                   build-wrapped-render-expr-str
                   (->> (context/eval-expr-str!+ ctx))
                   (either/branch
                     (fn [e] (throw e))
                     (fn [f] [f value ctx props]))))
             [fiddle-css-renderer (r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])]]

            :hypercrud.browser.browser-ui/xray
            [:<>
             (if-let [user-renderer (:user-renderer props)]
               ; don't use r/partial with user-renderer, r/partial useful for args, not components
               ; Select user-renderer is valid in xray mode now
               [user-renderer value ctx props]
               [hyperfiddle.ui/fiddle-xray value ctx props])
             [fiddle-css-renderer (r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])]]

            :hypercrud.browser.browser-ui/api
            [hyperfiddle.ui/fiddle-api value ctx props]))))))

(defn- ui-comp [ctx & [props]]                              ; user-renderer comes through here
  (let [value @(:hypercrud.browser/result ctx)              ; TODO remove this, make them ask
        props (update props :class css (auto-ui-css-class ctx))
        ; The reactdom component should filter the keys at the last second.
        ; https://github.com/hyperfiddle/hyperfiddle/issues/698
        display-mode (or (some-> (:hyperfiddle.ui/display-mode ctx) deref) :hypercrud.browser.browser-ui/user)
        error-props (-> (select-keys props [:class :on-click])
                        (update :class css "hyperfiddle-error"))]
    ^{:key (str display-mode)}
    [user-portal (ui-error/error-comp ctx) error-props
     ; If userland crashes (fiddle/renderer OR cljs-ns), reactions don't take hold, we need to reset here.
     ; Cheaper to pass fiddle-value as a prop than to hash everything
     [fiddle-renderer-cmp value ctx props @(:hypercrud.browser/fiddle ctx)]]))

(defn route-editor
  ([{rt :runtime pid :partition-id :as ctx}]
   [route-editor (runtime/get-route rt pid) (r/partial hf/set-route rt pid)])
  ([route on-change]
   (let [parse-string (fn [s]
                        (let [route (reader/read-edn-string! s)]
                          (s/assert :hyperfiddle/route route)
                          route))
         to-string pprint-str]
     [contrib.ui/debounced
      {:value route
       :debounce/interval 500
       :on-change (fn [o n] (when-not (= o n) (on-change n)))
       :mode "clojure"
       :lineNumbers false}
      contrib.ui/validated-cmp parse-string to-string contrib.ui/code])))

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
   (when-not (= :hypercrud.browser.browser-ui/user (or (some-> (:hyperfiddle.ui/display-mode ctx) deref) :hypercrud.browser.browser-ui/user))
     [route-editor ctx])
   [stale-browse ctx browse-error browse-success props]])

(defn ^:deprecated result-as-fiddle [ctx & [props]]
  ^{:key (str (:partition-id ctx) "-result-as-fiddle")}
  [stale/loading
   (r/track runtime/loading? (:runtime ctx) (:partition-id ctx))
   (base/browse-result-as-fiddle+ ctx)
   (fn [e] (timbre/error "error in render" (.-stack e))
           [browse-error ctx e props])
   (fn [ctx] [browse-success ctx props])
   (fn [ctx] [browse-success ctx props])])
