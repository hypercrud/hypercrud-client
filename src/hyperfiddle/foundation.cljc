(ns hyperfiddle.foundation
  (:require
    #?(:cljs [cats.monad.either :as either])
    #?(:cljs [contrib.reactive :as r])
    #?(:cljs [hyperfiddle.api :as hf])
    #?(:cljs [hyperfiddle.project :as project])
    #?(:cljs [hyperfiddle.ui :as ui])
    #?(:cljs [hyperfiddle.ui.checkbox :refer [Checkbox Radio RadioGroup]])
    #?(:cljs [hyperfiddle.ui.iframe :as iframe])
    #?(:cljs [hyperfiddle.ui.staging :as staging])
    #?(:cljs [hyperfiddle.view.keyboard :as k])
    #?(:cljs [hyperfiddle.view.keyboard.combos :as combos])
    [hyperfiddle.blocks.view-mode-selector :refer [ViewModeSelector]]
    [hyperfiddle.runtime]
    [hyperfiddle.view.controller :as view]))

(def root-pid "root")

(defn TopNav [{:keys [ctx]}]
  [:nav {:role  :navigation
         :style {:display     :flex
                 :align-items :center
                 :padding     "0.5rem 1rem"}}
   [ViewModeSelector]
   [:div {:style {:flex 1}}]
   #?(:cljs (if (hf/subject ctx)
              [:a {:href  "/:hyperfiddle.blocks.account!account"
                   :style {:text-transform :capitalize}}
               (str "👤Account")]
              [:a {:href (hyperfiddle.ide/stateless-login-url ctx)} "👤 Sign in"]))])

(defn augment [ctx]
  (assoc ctx :hyperfiddle.ui/display-mode (view/mode @view/state)))

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

(defn IframeRenderer [ctx]
  [:div {:style       {:flex 1}
         :tabIndex    "-1"
         :on-key-down (r/partial on-key-down)
         :on-key-up (r/partial on-key-up)}
   [TopNav {:ctx ctx}]
   #?(:cljs [iframe/iframe-cmp (augment ctx)])])

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
             [:main {:style {:display        :flex
                             :flex-direction :column
                             :height         "100%"}}
              [IframeRenderer ctx]
              [staging/inline-stage ctx]])))]))
