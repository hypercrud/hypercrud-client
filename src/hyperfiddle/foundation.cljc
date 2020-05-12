(ns hyperfiddle.foundation
  (:require
    [cats.monad.either :as either]
    [hyperfiddle.project :as project]
    [hyperfiddle.runtime]
    #?(:cljs [hyperfiddle.ui.iframe :as iframe])))


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
            (fn [_] [iframe/iframe-cmp ctx])))]))
