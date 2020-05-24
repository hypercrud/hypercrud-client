(ns hyperfiddle.foundation
  (:require
    #?(:cljs [cats.monad.either :as either])
    #?(:cljs [hyperfiddle.project :as project])
    #?(:cljs [hyperfiddle.ui.iframe :as iframe])
    #?(:cljs [hyperfiddle.ui :as ui])
    #?(:cljs [hyperfiddle.api :as hf])
    #?(:cljs [hyperfiddle.ui.staging :as staging])
    #?(:cljs [hyperfiddle.ui.checkbox :refer [Checkbox Radio RadioGroup]])
    #?(:cljs [contrib.reactive :as r])
    [hyperfiddle.runtime]))

(def root-pid "root")

#?(:cljs
   (do
     (defn on-browser-ui-selected [key]
       (js/console.log {:selected key}))

     (defn LoginSection [ctx]
       (js/console.log {:ctx ctx})
       (js/console.log (hf/subject ctx))
       (if (hf/subject ctx)
         [ui/link :hyperfiddle.ide/account ctx]
         [:a {:href (hyperfiddle.ide/stateless-login-url ctx)} "login"]))

     (defn Topnav [ctx]
       [:nav {:role  :navigation
              :style {:padding       "0.25rem 1rem"
                      :display       :flex
                      :justify-items :space-between
                      :width         "100%"}}
        [RadioGroup {:name      "view"
                     :options   [{:key :hypercrud.browser.browser-ui/api, :text "edn"}
                                 {:key :hypercrud.browser.browser-ui/xray, :text "data"}
                                 {:key :hypercrud.browser.browser-ui/user, :text "view"}]
                     :value     :hypercrud.browser.browser-ui/user
                     :on-change (r/partial on-browser-ui-selected)
                     :props     {:style {:display        :inline-grid
                                         :grid-auto-flow :column
                                         :grid-gap       "0.75rem"}}}]
        [LoginSection ctx]])))

#?(:cljs
   (defn view [ctx]
     [:<>
      [:style {:dangerouslySetInnerHTML {:__html (:project/css (hyperfiddle.runtime/get-project (:runtime ctx) (:partition-id ctx)))}}]
      (-> (hyperfiddle.runtime/get-project (:runtime ctx) (:partition-id ctx))
          :project/code
          project/eval-domain-code!+
          (either/branch
           (fn [e] [:div [:h2 {:style {:margin-top "10%" :text-align "center"}} "Misconfigured domain"]])
           (fn [_] [:main {:style {:display        :flex
                                  :flex-direction :column
                                  :height         "100%"}}
                   [Topnav ctx]
                   [:div {:style {:flex 1}}
                    [iframe/iframe-cmp ctx]]
                   ;; [preview/preview-effects ctx (:partition-id ctx) preview-state]
                   [staging/inline-stage ctx]])))]))
