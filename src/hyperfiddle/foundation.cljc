(ns hyperfiddle.foundation
  (:require
    #?(:cljs [cats.monad.either :as either])
    #?(:cljs [hyperfiddle.api :as hf])
    #?(:cljs [hyperfiddle.ide :as ide])
    #?(:cljs [hyperfiddle.project :as project])
    #?(:cljs [hyperfiddle.ui :as ui])
    #?(:cljs [hyperfiddle.ui.checkbox :refer [Checkbox Radio RadioGroup]])
    #?(:cljs [hyperfiddle.ui.iframe :as iframe])
    #?(:cljs [hyperfiddle.ui.staging :as staging])
    #?(:cljs [hyperfiddle.view.keyboard :as k])
    #?(:cljs [hyperfiddle.view.keyboard.actions :as actions])
    #?(:cljs [hyperfiddle.view.keyboard.combos :as combos])
    [contrib.reactive :as r]
    [hyperfiddle.blocks.view-mode-selector :refer [ViewModeSelector]]
    [hyperfiddle.runtime]
    [hyperfiddle.view.controller :as view]))

(def root-pid "root")

(defn TopNav [{:keys [ctx]}]
  [:nav {:role  :navigation
         :style {:display     :flex
                 :align-items :center
                 :padding     "0.5rem 1rem"}}

   [ViewModeSelector {:mode (:hyperfiddle.ui/display-mode ctx)}]
   [:div {:style {:flex 1}}]
   #?(:cljs (if (hf/subject ctx)
              [:a {:href  "/:hyperfiddle.blocks.account!account"
                   :style {:text-transform :capitalize}}
               (str "👤Account")]
              [:a {:href (ide/stateless-login-url ctx)} "👤 Sign in"]))])

(defn augment [ctx view-state]
  (merge ctx {:hyperfiddle.ui/display-mode    (view/mode view-state)
              :hyperfiddle.ui.iframe/on-click #?(:clj nil
                                                 :cljs (r/partial actions/frame-on-click (:runtime ctx)))}))

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
        [iframe/iframe-cmp ctx]
        [staging/inline-stage ctx]])))

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
