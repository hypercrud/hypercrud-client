(ns hyperfiddle.ui.popover
  (:require
    [cats.monad.either :as either]
    [cats.core :as cats :refer [mlet return]]
    [contrib.css :refer [css]]
    [contrib.ct :refer [unwrap]]
    [contrib.keypress :refer [with-keychord]]
    [contrib.reactive :as r]
    [contrib.pprint :refer [pprint-str]]
    [contrib.string :refer [blank->nil]]
    [contrib.ui.tooltip :refer [tooltip tooltip-props]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hyperfiddle.api]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.iframe :as iframe]
    [promesa.core :as p]
    [re-com.core :as re-com]
    [taoensso.timbre :as timbre]
    [hyperfiddle.api :as hf]))


(defn- run-txfn! [ctx props]
  (-> (p/resolved (context/link-tx ctx))
      (p/then
        (fn [user-txfn]
          (try
            (let [result (if (contains? (methods hyperfiddle.api/txfn) user-txfn) ; legacy
                           (let [[e a v] (context/eav ctx)]
                             (hyperfiddle.api/txfn user-txfn e a v ctx))
                           (hyperfiddle.api/tx ctx (context/eav ctx) props))]
              ; txfn may be sync or async
              (if (p/promise? result)
                result
                (p/resolved result)))
            (catch js/Error e (p/rejected e)))))))

(defn- stage! [child-pid {rt :runtime parent-pid :partition-id :as ctx} r-popover-data props]
  (-> (run-txfn! ctx props)
      (p/then (fn [tx]
                (let [tx-groups {(or (hypercrud.browser.context/dbname ctx) "$") ; https://github.com/hyperfiddle/hyperfiddle/issues/816
                                 tx}
                      popover-data @r-popover-data]
                  (runtime/close-popover rt parent-pid child-pid)
                  (cond-> (runtime/commit-branch rt child-pid tx-groups)
                    (::redirect props) (p/then (fn [_]
                                                 (hf/set-route rt
                                                   (runtime/get-branch-pid rt parent-pid)
                                                   ((::redirect props) popover-data))))))))
      (p/catch (fn [e]
                 ; todo something better with these exceptions (could be user error)
                 (timbre/error e)
                 (js/alert (cond-> (ex-message e)
                             (ex-data e) (str "\n" (pprint-str (ex-data e)))))))))

(defn- cancel! [rt parent-pid child-pid]
  (runtime/close-popover rt parent-pid child-pid)
  (runtime/delete-partition rt child-pid))

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
  (-> (run-txfn! ctx props)
      (p/then
        (fn [tx]
          (cond-> (runtime/with-tx (:runtime ctx) (:partition-id ctx) (context/dbname ctx) tx)
            (::redirect props) (p/then (fn [_] (hf/set-route (:runtime ctx) (:partition-id ctx) ((::redirect props) nil)))))))
      (p/catch (fn [e]
                 ; todo something better with these exceptions (could be user error)
                 (timbre/error e)
                 (js/alert (cond-> (ex-message e)
                             (ex-data e) (str "\n" (pprint-str (ex-data e)))))))))

(defn ^:export effect-cmp [ctx link-ref props label]
  (let [link-ctx (-> (mlet [ctx (context/refocus-to-link+ ctx link-ref)
                            args (context/build-args+ ctx @link-ref)] ; not sure what args would be in this case
                       (return (context/occlude-eav ctx args))) ; guessing we are occluding v to nil?
                     (either/branch
                       (fn [e] nil)                         ; wtf how does anything work
                       identity))
        props (-> props
                  (assoc :on-click (r/partial run-effect! link-ctx props))
                  (update :class css "hyperfiddle"
                          ; use twbs btn coloring but not "btn" itself
                          (if-not (contains? (methods hyperfiddle.api/tx)
                                             (context/link-tx link-ctx))
                            "btn-outline-danger"
                            "btn-warning"))
                  (update :disabled #(or % (disabled? link-ref link-ctx))))]
    [:button (select-keys props [:class :style :disabled :on-click])
     [:span (str label "!")]]))

(defn- popover-cmp-impl [ctx child-pid props & body-children]
  [wrap-with-tooltip ctx child-pid (select-keys props [:class :on-click :style :disabled :tooltip])
   [with-keychord
    "esc" #(do (js/console.warn "esc") ((::close-popover props) child-pid))
    [re-com/popover-anchor-wrapper
     :showing? (r/track runtime/popover-is-open? (:runtime ctx) (:partition-id ctx) child-pid)
     :position :below-center
     :anchor [:button (-> props
                          ;(dissoc :route :tooltip ::redirect)
                          (select-keys [:class :style :disabled])
                          ; use twbs btn coloring but not "btn" itself
                          (update :class css "btn-default")
                          (assoc :on-click (r/partial (::open-popover props) child-pid)))
              [:span (str (::label props) "▾")]]
     :popover [re-com/popover-content-wrapper
               :no-clip? true
               ; wrapper helps with popover max-width, hard to layout without this
               :body (into [:div.hyperfiddle-popover-body] body-children)]]]])

(defn- branched-popover-body-cmp [child-pid {rt :runtime :as ctx} props]
  (let [branched-ctx (context/set-partition ctx child-pid)]
    [:<>
     [iframe/iframe-cmp (assoc branched-ctx :hyperfiddle.ui/error-with-stage? true)]
     [:div.hyperfiddle-popover-actions
      (let [+popover-ctx-post (base/browse-partition+ branched-ctx) ; todo browse once
            r-popover-data (r/>>= :hypercrud.browser/result +popover-ctx-post) ; focus the fiddle at least then call @(context/data) ?
            popover-invalid (->> +popover-ctx-post (unwrap (constantly nil)) context/tree-invalid?)]
        [:button {:on-click #(stage! child-pid ctx r-popover-data props)
                  :disabled popover-invalid} "stage"])
      [:button {:on-click #(cancel! rt (:partition-id ctx) child-pid)} "cancel"]]]))

(let [open-branched-popover! (fn [rt pid route child-pid]
                               (runtime/create-partition rt pid child-pid true)
                               (-> (hf/set-route rt child-pid route)
                                   (p/finally (fn [] (runtime/open-popover rt pid child-pid)))))]
  (defn- branched-popover-cmp [child-pid ctx props label]
    [popover-cmp-impl ctx child-pid
     (assoc props
       ::label label
       ::open-popover (r/partial open-branched-popover! (:runtime ctx) (:partition-id ctx) (:route props))
       ::close-popover (r/partial cancel! (:runtime ctx) (:partition-id ctx)))
     ; body-cmp NOT inlined for perf
     [branched-popover-body-cmp child-pid ctx props]]))

(defn- unbranched-popover-body-cmp [child-pid ctx]
  [:<>
   [iframe/iframe-cmp (context/set-partition ctx child-pid)]
   [:button {:on-click #(runtime/close-popover (:runtime ctx) (:partition-id ctx) child-pid)} "close"]])

(defn- unbranched-popover-cmp [child-pid ctx props label]
  [popover-cmp-impl ctx child-pid
   (assoc props
     ::label label
     ::open-popover (r/partial runtime/open-popover (:runtime ctx) (:partition-id ctx))
     ::close-popover (r/partial runtime/close-popover (:runtime ctx) (:partition-id ctx)))
   ; body-cmp NOT inlined for perf
   [unbranched-popover-body-cmp child-pid ctx]])

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
        should-branch @(r/fmap (r/comp some? blank->nil :link/tx-fn) link-ref)]
    (if should-branch
      [branched-popover-cmp child-pid link-ctx props label]
      [unbranched-popover-cmp child-pid link-ctx props label])))
