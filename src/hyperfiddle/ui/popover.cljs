(ns hyperfiddle.ui.popover
  (:require
    [cats.monad.either :as either]
    [cats.core :as cats :refer [mlet return]]
    [contrib.css :refer [css]]
    [contrib.ct :refer [unwrap]]
    [contrib.do :refer [do-async from-result]]
    [contrib.keypress :refer [with-keychord]]
    [contrib.reactive :as r]
    [contrib.pprint :refer [pprint-str]]
    [contrib.string :refer [blank->nil]]
    [contrib.ui.tooltip :refer [tooltip tooltip-props]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hyperfiddle.api :as hf]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.iframe :as iframe]
    [promesa.core :as p]
    [re-com.core :as re-com]
    [taoensso.timbre :as timbre]))

(defn run-txfn! [parent-ctx ?popover-ctx props]
  (do-async
    (hf/tx                                                  ; Userland implements this multimethod, can userland throw? Do we need to catch?
      parent-ctx                                            ; parent-ctx has the link pointing to the popover
      (hf/eav parent-ctx)                                   ; Userland nearly always wants this, which is the EAV that reflects the parent-child relationship
      props                                                 ; Userland view props, do not use this for hyperfiddle platform state
      ?popover-ctx                                          ; Randomly add child-ctx for access to route which is used to accumulate popover-local state. effect-buttons don't have a popover
      )))

(defn- stage! [{rt :runtime parent-pid :partition-id :as ctx} popover-ctx props]
  (-> (run-txfn! ctx popover-ctx props)
      (p/then (fn [tx]
                (let [tx-groups {(or (hf/dbname ctx) "$") ; https://github.com/hyperfiddle/hyperfiddle/issues/816
                                 tx}
                      popover-data @(:hypercrud.browser/result popover-ctx)] ; Can this be (hf/data ctx)?
                  (runtime/close-popover (:runtime ctx) (:partition-id ctx) (:partition-id popover-ctx))
                  #_(runtime/delete-partition (:runtime ctx) (:partition-id popover-ctx)) ; don't kill partition before committed, constraint violation
                  (cond-> (runtime/commit-branch (:runtime ctx) (:partition-id popover-ctx) tx-groups) ; if branched
                    (::redirect props) (p/then (fn [_]
                                                 (hf/set-route rt
                                                   (runtime/get-branch-pid rt parent-pid)
                                                   ((::redirect props) popover-data))))))))
      (p/catch (fn [e]
                 ; todo something better with these exceptions (could be user error)
                 (timbre/error e)
                 (js/alert (cond-> (ex-message e)
                             (ex-data e) (str "\n" (pprint-str (ex-data e)))))))))

(defn- cancel! [ctx child-ctx]
  (runtime/close-popover (:runtime ctx) (:partition-id ctx) (:partition-id child-ctx))
  (runtime/delete-partition (:runtime ctx) (:partition-id child-ctx)))

(defn- wrap-with-tooltip [ctx child-pid props child]
  ; omit the formula tooltip when popover is open
  (if (runtime/popover-is-open? (:runtime ctx) (:partition-id ctx) child-pid)
    child
    [tooltip (tooltip-props (:tooltip props)) child]))

(defn- disabled? [link-ref ctx]
  (condp some @(r/fmap :link/class link-ref)
    #{:hf/new} nil #_(not @(r/track hf/subject-may-create? ctx)) ; flag
    #{:hf/remove} (if (let [[_ a _] @(:hypercrud.browser/eav ctx)] a)
                    (if-let [ctx (:hypercrud.browser/parent ctx)]
                      (not @(r/track hf/subject-may-edit-entity? ctx))) ; check logic
                    (not @(r/track hf/subject-may-edit-entity? ctx)))
    ; else we don't know the semantics, just nil out
    nil))

(defn run-effect! [ctx props]
  (-> (run-txfn! ctx nil props)
      (p/then
        (fn [tx]
          (cond-> (runtime/with-tx (:runtime ctx) (:partition-id ctx) (hf/dbname ctx) tx)
            (::redirect props) (p/then (fn [_] (hf/set-route (:runtime ctx) (:partition-id ctx) ((::redirect props) nil)))))))
      (p/catch (fn [e]
                 ; todo something better with these exceptions (could be user error)
                 (timbre/error e)
                 (js/alert (cond-> (ex-message e)
                             (ex-data e) (str "\n" (pprint-str (ex-data e)))))))))

(defn ^:export effect-cmp [ctx link-ref props label]
  (let [link-ctx (try
                   (let [ctx (from-result (context/refocus-to-link+ ctx link-ref))
                         args (context/build-args ctx @link-ref)] ; not sure what args would be in this case
                     (context/occlude-eav ctx args))        ; guessing we are occluding v to nil?
                   (catch js/Error e nil))                  ; wtf how does anything work
        props (-> props
                  (assoc :on-click (r/partial run-effect! link-ctx props))
                  (update :class css "hyperfiddle"
                          ; use twbs btn coloring but not "btn" itself
                          (if-not (contains? (methods hyperfiddle.api/tx)
                                             (hf/link-tx link-ctx))
                            "btn-outline-danger"
                            "btn-warning"))
                  (update :disabled #(or % (disabled? link-ref link-ctx))))]
    [:button (select-keys props [:class :style :disabled :on-click])
     [:span (str label "!")]]))

(defn- popover-anchor-impl [ctx child-ctx props & body-children]
  [wrap-with-tooltip ctx (:partition-id child-ctx) (select-keys props [:class :on-click :style :disabled :tooltip])
   [with-keychord
    "esc" #(do (js/console.warn "esc") ((::close-popover props) child-ctx))
    [re-com/popover-anchor-wrapper
     :showing? (r/track runtime/popover-is-open? (:runtime ctx) (:partition-id ctx) (:partition-id child-ctx))
     :position :below-center
     :anchor [:button (-> props
                          ;(dissoc :route :tooltip ::redirect)
                          (select-keys [:class :style :disabled])
                          ; use twbs btn coloring but not "btn" itself
                          (update :class css "btn-default")
                          (assoc :on-click (r/partial (::open-popover props) child-ctx)))
              [:span (str (::label props) "▾")]]
     :popover [re-com/popover-content-wrapper
               :no-clip? true
               ; wrapper helps with popover max-width, hard to layout without this
               :body (into [:div.hyperfiddle-popover-body] body-children)]]]])

(defn- branched-popover-body-cmp [child-ctx parent-ctx props]
  [:<>
   [iframe/iframe-cmp (assoc child-ctx :hyperfiddle.ui/error-with-stage? true)]
   [:div.hyperfiddle-popover-actions
    (let [child-ctx (try (from-result (base/browse-partition+ child-ctx)) ; todo browse once
                         (catch js/Error e nil))]
      [:button {:on-click #(stage! parent-ctx child-ctx props)
                :disabled (context/tree-invalid? child-ctx)}
       "stage"])
    [:button {:on-click #(cancel! parent-ctx child-ctx)} "cancel"]]])

(let [open-branched-popover! (fn [rt ctx route child-ctx]
                               (runtime/create-partition (:runtime ctx) (:partition-id ctx) (:partition-id child-ctx) true)
                               (-> (hf/set-route rt (:partition-id child-ctx) route)
                                   (p/finally (fn [] (runtime/open-popover (:runtime ctx) (:partition-id ctx) (:partition-id child-ctx))))))]
  (defn- branched-popover-anchor [child-ctx ctx props label]
    [popover-anchor-impl ctx child-ctx
     (assoc props
       ::label label
       ::open-popover (r/partial open-branched-popover! (:runtime ctx) ctx (:route props))
       ::close-popover (r/partial cancel! ctx))
     ; body-cmp NOT inlined for perf
     [branched-popover-body-cmp child-ctx ctx props]]))

(defn- popover-anchor-body [child-ctx ctx]
  [:<>
   [iframe/iframe-cmp child-ctx]
   [:button {:on-click #(runtime/close-popover (:runtime ctx) (:partition-id ctx) (:partition-id child-ctx))}
    "close"]])

(defn- unbranched-popover-anchor [child-ctx ctx props label]
  [popover-anchor-impl ctx child-ctx
   (assoc props
     ::label label
     ::open-popover (r/partial runtime/open-popover ctx)
     ::close-popover (r/partial runtime/close-popover ctx))
   ; body-cmp NOT inlined for perf
   [popover-anchor-body child-ctx ctx]])

(defn ^:export popover-cmp [ctx link-ref props label]
  (let [+route-and-ctx (context/refocus-build-route-and-occlude+ ctx link-ref) ; Can fail if formula dependency isn't satisfied
        link-ctx (either/branch
                   +route-and-ctx
                   (constantly nil)                         ; how can this safely be nil
                   first)
        props (-> (cats/fmap second +route-and-ctx)
                  (hyperfiddle.ui/validated-route-tooltip-props link-ref link-ctx props)
                  (update :class css "hyperfiddle")
                  (update :disabled #(or % (disabled? link-ref link-ctx))))
        child-pid (context/build-pid-from-link ctx link-ctx (:route props)) ; todo remove route from props
        child-ctx (context/set-partition ctx child-pid)]
    (if (context/branched-link? link-ctx)
      [branched-popover-anchor child-ctx link-ctx props label]
      [unbranched-popover-anchor child-ctx link-ctx props label])))
