(ns hyperfiddle.foundation
  (:require
    #?(:cljs [hyperfiddle.ui.iframe :as iframe])
    [cats.monad.either :as either]
    [hyperfiddle.project :as project]
    [hyperfiddle.runtime]
    [hyperfiddle.ui.staging :as staging]))


(def root-pid "root")

#?(:cljs
   (defn view [ctx]
     [:<>
      [:style {:dangerouslySetInnerHTML {:__html (:project/css (hyperfiddle.runtime/get-project (:runtime ctx) (:partition-id ctx)))}}]
      (-> (hyperfiddle.runtime/get-project (:runtime ctx) (:partition-id ctx))
          :project/code
          project/eval-domain-code!+
          (either/branch
           (fn [e] [:div [:h2 {:style {:margin-top "10%" :text-align "center"}} "Misconfigured domain"]])
           (fn [_] [:main {:style {:display         :flex
                                  :flex-direction  :column
                                  :justify-content :space-between}}
                   [iframe/iframe-cmp ctx]
                   ;; [preview/preview-effects ctx (:partition-id ctx) preview-state]
                   [staging/inline-stage ctx]])))]))
