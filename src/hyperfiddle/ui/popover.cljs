(ns hyperfiddle.ui.popover
  (:require
    [cats.core :refer [fmap]]
    [contrib.css :refer [css]]
    [contrib.data :refer [dissoc-nils]]
    [contrib.eval :as eval]
    [contrib.keypress :refer [with-keychord]]
    [contrib.reactive :as r]
    [contrib.pprint :refer [pprint-str]]
    [contrib.string :refer [blank->nil]]
    [contrib.try$ :refer [try-promise]]
    [contrib.ui.tooltip :refer [tooltip tooltip-props]]
    [cuerdas.core :as string]
    [hypercrud.browser.context :as context]
    [hypercrud.util.branch :as branch]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.api]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.tempid :as tempid :refer [stable-entity-key]]
    [promesa.core :as p]
    [re-com.core :as re-com]
    [taoensso.timbre :as timbre]
    [hypercrud.browser.base :as base]))


(let [safe-eval-string #(try-promise (eval/eval-expr-str! %))
      memoized-eval-string (memoize safe-eval-string)]
  (defn- with-swap-fn [link-ref eav ctx f]
    (let [{:keys [:link/tx-fn] :as link} @link-ref]
      (-> (if (and (string? tx-fn) (not (string/blank? tx-fn)))
            (memoized-eval-string tx-fn)                    ; TODO migrate type to keyword
            (p/resolved (constantly nil)))
          (p/then
            (fn [user-txfn]
              (p/promise
                (fn [resolve! reject!]
                  (let [swap-fn-async (fn []
                                        (let [result (let [[e a v] eav
                                                           result (hyperfiddle.api/txfn user-txfn e a v ctx)]
                                                       ; txfn may be sync or async
                                                       (if-not (p/promise? result) (p/resolved result) result))]
                                          ; let the caller of this :stage fn know the result
                                          ; This is super funky, a swap-fn should not be effecting, but seems like it would work.
                                          (-> result (p/branch (fn [v] (resolve! nil))
                                                               (fn [e]
                                                                 (reject! e)
                                                                 (timbre/warn e))))

                                          ; return the result to the action, it could be a promise
                                          result))]
                    (runtime/dispatch! (:peer ctx) [:txfn (:link/rel link) (:link/path link)])
                    (f swap-fn-async))))))
          ; todo something better with these exceptions (could be user error)
          (p/catch (fn [err] (js/alert (pprint-str err))))))))

(defn stage! [link-ref eav popover-id child-branch ctx r-popover-data props]
  (let [f (fn [swap-fn-async]
            ; the swap-fn could be determined via the link rel
            (->> (actions/stage-popover (:peer ctx) child-branch
                                        #(->> (swap-fn-async)
                                              (fmap
                                                (fn [tx]
                                                  (-> {:tx {(hypercrud.browser.context/uri ctx) tx}
                                                       :app-route (if-let [f (::redirect props)] (f @r-popover-data))}
                                                      dissoc-nils))))
                                        (actions/close-popover (:branch ctx) popover-id))
                 (runtime/dispatch! (:peer ctx))))]
    (with-swap-fn link-ref eav ctx f)))

(defn close! [popover-id ctx]
  (runtime/dispatch! (:peer ctx) (actions/close-popover (:branch ctx) popover-id)))

(defn cancel! [popover-id child-branch ctx]
  (runtime/dispatch! (:peer ctx) (actions/batch
                                   (actions/close-popover (:branch ctx) popover-id)
                                   (actions/discard-partition child-branch))))

(defn managed-popover-body [route popover-id ?child-branch link-ref ctx eav props]
  (let [popover-ctx (cond-> (context/clean ctx)             ; hack clean for block level errors
                            ?child-branch (assoc :branch ?child-branch))

        r-popover-data (->> (base/data-from-route route popover-ctx)
                            (r/>>= :hypercrud.browser/data))]
    [:div.hyperfiddle-popover-body                          ; wrpaper helps with popover max-width, hard to layout without this
     ; NOTE: this ctx logic and structure is the same as the popover branch of browser-request/recurse-request
     [hyperfiddle.ui/iframe popover-ctx {:route route}]     ; cycle
     (when ?child-branch
       [:button {:on-click (r/partial stage! link-ref eav popover-id ?child-branch ctx r-popover-data props)} "stage"])
     (if ?child-branch
       [:button {:on-click #(cancel! popover-id ?child-branch ctx)} "cancel"]
       [:button {:on-click #(close! popover-id ctx)} "close"])]))

(defn open! [route popover-id child-branch ctx]
  (runtime/dispatch! (:peer ctx)
                     (if child-branch
                       (actions/add-partition (:peer ctx) route child-branch (::runtime/branch-aux ctx)
                                              (actions/open-popover (:branch ctx) popover-id))
                       (actions/open-popover (:branch ctx) popover-id))))

(defn- show-popover? [popover-id ctx]
  (runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :popovers popover-id]))

(defn- wrap-with-tooltip [popover-id ctx props child]
  ; omit the formula tooltip when popover is open
  (if @(show-popover? popover-id ctx)
    child
    [tooltip (tooltip-props (:tooltip props)) child]))

(defn run-txfn! [link-ref eav ctx props]
  (let [popover-data nil]
    (letfn [(f [swap-fn-async]
              (-> (->> (swap-fn-async)
                       (fmap (fn [tx]
                               (-> {:tx {(hypercrud.browser.context/uri ctx) tx}
                                    :app-route (if-let [f (::redirect props)] (f popover-data))}
                                   dissoc-nils))))
                  (p/then
                    (fn [{:keys [tx app-route]}]
                      (->> (actions/with-groups (:peer ctx) (:branch ctx) tx :route app-route)
                           (runtime/dispatch! (:peer ctx)))))))]
      (with-swap-fn link-ref eav ctx f))))

(defn effect-cmp [link-ref eav ctx props label]
  (let [props (-> props
                  (assoc :on-click (r/partial run-txfn! link-ref eav ctx props))
                  ; use twbs btn coloring but not "btn" itself
                  (update :class css "btn-warning"))]
    [:button props [:span (str label "!")]]))

(defn popover-cmp [link-ref eav ctx visual-ctx props label]
  ; try to auto-generate branch/popover-id from the product of:
  ; - link's :db/id
  ; - route
  ; - visual-ctx's data & path (where this popover is being drawn NOT its dependencies)
  (let [child-branch (let [child-id-str (-> [(tempid/tempid-from-ctx visual-ctx)
                                             @(r/fmap :db/id link-ref)
                                             (:route props)
                                             @(r/fmap (r/partial stable-entity-key ctx) (:hypercrud.browser/fiddle ctx))]
                                            hash str)]
                       (branch/encode-branch-child (:branch ctx) child-id-str))
        popover-id child-branch                             ; just use child-branch as popover-id
        child-branch (when @(r/fmap (r/comp some? blank->nil :link/tx-fn) link-ref)
                       child-branch)
        btn-props (-> props
                      (dissoc :route :tooltip)
                      (assoc :on-click (r/partial open! (:route props) popover-id child-branch ctx))
                      ; use twbs btn coloring but not "btn" itself
                      (update :class css "btn-default"))]
    [wrap-with-tooltip popover-id ctx props
     [with-keychord
      "esc" #(do (js/console.warn "esc") (if child-branch
                                           (cancel! popover-id child-branch ctx)
                                           (close! popover-id ctx)))
      [re-com/popover-anchor-wrapper
       :showing? (show-popover? popover-id ctx)
       :position :below-center
       :anchor [:button btn-props [:span (str label "▾")]]
       :popover [re-com/popover-content-wrapper
                 :no-clip? true
                 :body [managed-popover-body (:route props) popover-id child-branch link-ref ctx eav props]]]]]))
