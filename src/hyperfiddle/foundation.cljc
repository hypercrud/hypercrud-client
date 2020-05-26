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
    [hyperfiddle.runtime]
    [hyperfiddle.view.controller :as view]))

(def root-pid "root")

(defn augment [ctx]
  (assoc ctx :hyperfiddle.ui/display-mode (view/view-mode (deref view/state))))

(defn IframeRenderer [ctx]
  [:div {:style {:flex 1}}
   [iframe/iframe-cmp (augment ctx)]])

#?(:cljs
   (defn view [ctx]
     [:<>
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
              ;; [preview/preview-effects ctx (:partition-id ctx) preview-state]
              [staging/inline-stage ctx]])))]))
