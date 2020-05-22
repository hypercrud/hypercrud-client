(ns hyperfiddle.ide.edit
  (:require
    [cats.monad.either :as either]
    [contrib.css :refer [css]]
    [contrib.reactive :as r]
    [hypercrud.browser.context :refer [map->Context]]
    [hyperfiddle.api :as hf]
    [hyperfiddle.ide.domain :as ide-domain]
    [hyperfiddle.ide.fiddles.fiddle-src :as fiddle-src]
    [hyperfiddle.ide.fiddles.topnav :as topnav]
    [hyperfiddle.ide.preview.runtime :as preview-rt :refer [->Runtime]]
    [hyperfiddle.ide.preview.state :refer [->FAtom]]
    [hyperfiddle.ide.preview.view :as preview]
    [hyperfiddle.ide.ui.route-editor :as route-editor]
    [hyperfiddle.io.browser :refer [->IOImpl]]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.state :as state]
    [hyperfiddle.ui :as ui]
    [hyperfiddle.ui.error :as ui-error]
    [hyperfiddle.ui.staging :as staging]
    [re-com.core :as re-com]))


(defn route-editor-button [rt branch preview-state]
  (let [show-route-editor (r/cursor preview-state [:show-route-editor])
        is-hovering-route-button (r/cursor preview-state [:is-hovering-route-button])
        tooltip-content (if @show-route-editor
                          "Hide route editor"
                          "Show route editor")]
    [:button.btn.btn-link
     {:style {:vertical-align "inherit"}
      :on-click #(swap! show-route-editor not)
      :on-mouse-over #(do (reset! is-hovering-route-button true) nil)
      :on-mouse-out #(do (reset! is-hovering-route-button false) nil)}
     [re-com/popover-tooltip
      :showing? is-hovering-route-button
      :anchor (topnav/route->fiddle-label (runtime/get-route rt branch))
      :label tooltip-content]]))

(defn route-editor [rt branch show-route-editor]
  ; too much logic happens in the parent render, localize this deref to prevent thrash
  (when @show-route-editor
    [route-editor/form-editor rt branch]))

(defn view [_ ctx props]
  (let [preview-rt (-> (hf/domain (:runtime ctx)) ::ide-domain/user-domain+
                       (either/branch
                         (constantly nil)
                         (fn [user-domain]
                           (let [preview-state (->FAtom (state/state (:runtime ctx)) preview/to (r/partial preview/from (hf/domain (:runtime ctx)) (:partition-id ctx)))
                                 user-io (->IOImpl user-domain)]
                             (->Runtime (:runtime ctx) preview-rt/preview-pid user-domain user-io preview-state)))))
        preview-state (r/atom {:initial-render true
                               :is-refreshing true
                               :is-hovering-refresh-button false
                               :alt-key-pressed false
                               :display-mode :hypercrud.browser.browser-ui/user
                               :show-route-editor false
                               ::editor-open false
                               ; specifically deref and re-wrap this ref on mount because we are tracking deviation from this value
                               :staleness (when preview-rt
                                            (preview/ide-partition-reference preview-rt (:partition-id ctx)))})
        ; fiddle-src
        initial-tab (-> @(:hypercrud.browser/route ctx) ::route/fragment hyperfiddle.ide/parse-ide-fragment)
        ;:initial-tab @(contrib.reactive/fmap-> (:hypercrud.browser/route ctx) (get 3) hyperfiddle.ide/parse-ide-fragment)
        tab-state (r/atom (if (contains? fiddle-src/tabs initial-tab)
                            initial-tab
                            :hf/query))]
    (fn [_ ctx props]
      [:<>
       #_[ui/browse :hyperfiddle/topnav ctx nil
        {::topnav/left-child
         [:<>
          [:span (topnav/route->fiddle-label (runtime/get-route preview-rt preview-rt/preview-pid))]
          #_[route-editor-button preview-rt preview-branch preview-state]
          (-> (hf/domain (:runtime ctx)) ::ide-domain/user-domain+
              (either/branch
                (fn [e]
                  ; todo why does an error hide the toolbar
                  nil)
                (fn [user-domain]
                  ; todo build the preview-rt here
                  [preview/preview-toolbar preview-rt (:partition-id ctx) preview-rt/preview-pid preview-state])))]

         ::topnav/right-child
         (if @(r/cursor preview-state [::editor-open])
           [fiddle-src/fiddle-src-tabs tab-state])}]

       [:div (select-keys props [:class])
        [:div {:class "-hyperfiddle-ide-preview"}
         (-> (hf/domain (:runtime ctx))
             ;; ::ide-domain/user-domain+
             hf/->either-domain
             (either/branch
               (fn [e]
                 [:<>
                  [:h2 "Domain misconfigured"]              ; todo improve me
                  [ui-error/error-block e]])
               (fn [user-domain]
                 ; todo build the preview-rt here
                 (let [preview-ctx (map->Context
                                     {:runtime preview-rt
                                      :partition-id preview-rt/preview-pid
                                      :hyperfiddle.ui/debug-tooltips true
                                      :hyperfiddle.ui/display-mode (r/cursor preview-state [:display-mode])
                                      :hyperfiddle.ui.iframe/on-click (r/partial preview/frame-on-click preview-rt)})]
                   [:<>
                    #_[route-editor preview-rt preview-branch (r/cursor preview-state [:show-route-editor])]
                    [preview/preview-effects preview-ctx (:partition-id ctx) preview-state]
                    [staging/inline-stage preview-ctx]]))))]

        (if @(r/cursor preview-state [::editor-open])
          [:div.fiddle-editor-col
           [hyperfiddle.ui/link
            :hyperfiddle/ide ctx nil
            {::fiddle-src/tab-state tab-state
             :class (css "fiddle-editor devsrc" #_(hyperfiddle.ui.iframe/auto-ui-css-class ctx))
             :user-renderer hyperfiddle.ide.fiddles.fiddle-src/fiddle-src-renderer}]
           ; In case of datoms-conflict, render outside the :hyperfiddle/ide iframe
           [hyperfiddle.ide/ide-stage ctx]])
        ]])))
