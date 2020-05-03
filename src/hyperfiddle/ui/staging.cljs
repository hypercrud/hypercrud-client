(ns hyperfiddle.ui.staging
  (:require
    [cats.monad.either :as either]
    [clojure.string :as string]
    [contrib.css :refer [css]]
    [contrib.pprint :refer [pprint-datoms-str]]
    [contrib.reactive :as r]
    [contrib.reader :refer [read-edn-string!]]
    [contrib.hfrecom :refer [anchor-tabs]]
    [contrib.ui :refer [code debounced easy-checkbox validated-cmp]]
    [contrib.ui.tooltip :refer [tooltip]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.state :as state]
    [re-com.core :as re-com]
    [re-com.tabs]
    [hyperfiddle.api :as hf]))


(defn- default-tab-model [selected-dbname tab-ids]
  (if (contains? (set tab-ids) selected-dbname)
    selected-dbname
    (first tab-ids)))

(defn- writes-allowed?+ [rt selected-dbname]
  (let [hf-db (hf/database (hf/domain rt) selected-dbname)
        subject (runtime/get-user-id rt)]
    (hf/subject-may-transact+ hf-db subject)))

(defn transact-button [rt pid selected-dbname-ref label]
  (let [selected-dbname @selected-dbname-ref
        writes-allowed?+ (writes-allowed?+ rt selected-dbname)
        stage (runtime/get-stage rt pid selected-dbname)]
    [tooltip (cond
               (either/left? writes-allowed?+) {:status :warning :label @writes-allowed?+}
               (empty? stage) {:status :warning :label "no changes"})
     (let [color (domain/database-color (hf/domain rt) selected-dbname)
           is-disabled (or (either/left? writes-allowed?+) (empty? stage))]
       [:button {:disabled is-disabled
                 :class (css "btn btn-sm hf-btn-xs" (if is-disabled "btn-outline-secondary" "btn-secondary"))
                 :style (cond-> {(if is-disabled :color :background-color) color
                                 :border-color color}
                          is-disabled (assoc :pointer-events "none"))
                 :on-click (fn [] (runtime/transact rt pid selected-dbname))}
        (str label " transact!")])]))

(defn auto-transact-control [rt pid selected-dbname-ref]
  (let [selected-dbname @selected-dbname-ref
        writes-allowed?+ (writes-allowed?+ rt selected-dbname)
        stage (runtime/get-stage rt pid selected-dbname)]
    [tooltip (cond
               (either/left? writes-allowed?+) {:status :warning :label @writes-allowed?+}
               (not (empty? @stage)) {:status :warning :label "Please transact! all changes first"})
     (let [is-disabled (or (either/left? writes-allowed?+) (not (empty? @stage)))
           is-auto-transact (runtime/get-auto-transact rt selected-dbname)]
       [easy-checkbox {:disabled is-disabled
                       :style (cond-> {:margin-left "0.5em"}
                                is-disabled (assoc :pointer-events "none"))
                       :checked (boolean is-auto-transact)
                       :on-change #(runtime/set-auto-transact rt selected-dbname (not is-auto-transact))}
        " auto-transact"])]))

(let [parse-string (fn [s]
                     (let [v (read-edn-string! s)]
                       (assert (and (or (nil? v) (vector? v) (seq? v))
                                    (every? (fn [v] (or (map? v) (vector? v) (seq? v))) v)))
                       (some-> v reverse vec)))
      to-string (fn [v] (pprint-datoms-str (some-> v reverse)))
      on-change (fn [rt pid dbname-ref o n] (runtime/set-stage rt pid @dbname-ref n))]
  (defn- tab-content [rt pid dbname-ref & [child]]
    [:div.hyperfiddle-stage-content
     {:style {:border-color (domain/database-color (hf/domain rt) @dbname-ref)}}
     child
     (let [props {:value (runtime/get-stage rt pid @dbname-ref)
                  :readOnly (runtime/get-auto-transact rt @dbname-ref)
                  :on-change (r/partial on-change rt pid dbname-ref)
                  :lineNumbers false}]
       ^{:key (str @dbname-ref)}
       [debounced props validated-cmp parse-string to-string code])]))

(defn default-dbname-labels [rt]
  (->> (hf/domain rt) hf/databases keys sort
       (map (fn [%] {:id % :label (domain/dbname-label %)}))))

(defn dirty-dbs [rt pid]
  (->> (runtime/get-stage rt pid)
       (remove (comp empty? second))
       (map first)
       set))

(defn ^:export editor-cmp
  ([selected-dbname ctx]
   [editor-cmp selected-dbname (:runtime ctx) (:partition-id ctx) (default-dbname-labels (:runtime ctx))])
  ([selected-dbname rt pid dbname-labels & [child]]
   (let [tabs-definition (mapv (fn [{:keys [id] s-label :label}]
                                 {:id id
                                  :label [:span
                                          {:style {:border-color (domain/database-color (hf/domain rt) id)}
                                           :class (when (contains? (dirty-dbs rt pid) id) "stage-dirty")}
                                          s-label]})
                               dbname-labels)
         selected-dbname' (r/fmap-> selected-dbname (default-tab-model (mapv :id dbname-labels)))]
     [:div.hyperfiddle-staging-editor-cmp
      [re-com.tabs/horizontal-tabs
       :model selected-dbname'
       :tabs tabs-definition
       :on-change (r/partial reset! selected-dbname)]
      [tab-content rt pid selected-dbname' child]])))

(defn- tooltip-content [rt dbname-labels]
  [:div {:style {:text-align "left"}}
   [hyperfiddle.ui/markdown
    (->> (sort-by :label dbname-labels)
         (mapv (fn [{:keys [id] s-label :label}]
                 (let [prefix (if (runtime/get-auto-transact rt id)
                                "- [x] "
                                "- [ ] ")]
                   (str prefix s-label))))
         (string/join "\n")
         (str "##### Auto-transact:\n\n"))
    {:hyperfiddle.ui.markdown-extensions/unp true}]])

(defn ^:export popover-button [rt pid dbname-labels & {:keys [show-auto-tx]}]
  (let [show-tooltip (r/atom false)
        show-stage (r/atom false)
        selected-dbname (r/cursor (state/state rt) [:staging/selected-uri])]
    (fn [rt pid dbname-labels & {:keys [show-auto-tx]}]
      (let [maybe-tooltip-wrapper (fn [button-cmp]
                                    (if show-auto-tx
                                      [re-com/popover-tooltip
                                       :showing? (r/atom (and @show-tooltip (not @show-stage)))
                                       :label [tooltip-content rt dbname-labels]
                                       :anchor button-cmp]
                                      button-cmp))]
        [:div.hyperfiddle-staging-popover-button
         (maybe-tooltip-wrapper
           [re-com/popover-anchor-wrapper
            :showing? show-stage
            :position :below-center
            :anchor (let [stage-is-dirty (not @(r/fmap empty? (r/track runtime/get-stage rt pid)))]
                      [:button (cond-> {:on-click #(reset! show-stage true)
                                        :class (cond-> "hyperfiddle btn-default"
                                                 stage-is-dirty (css "stage-dirty"))}
                                 show-auto-tx (assoc :on-mouse-over #(do (reset! show-tooltip true) nil)
                                                     :on-mouse-out #(do (reset! show-tooltip false) nil)))
                       "stage▾"])
            :popover [re-com/popover-content-wrapper
                      :no-clip? true?
                      :body (let [selected-dbname' (r/fmap-> selected-dbname (default-tab-model (mapv :id dbname-labels)))]
                              [:div.hyperfiddle-popover-body
                               [editor-cmp selected-dbname rt pid dbname-labels
                                [:<>
                                 [transact-button rt pid selected-dbname']
                                 (when show-auto-tx
                                   [auto-transact-control rt pid selected-dbname'])
                                 [:button.close-popover {:on-click (r/partial reset! show-stage false)} "close"]]]])]])]))))

(defn inline-stage
  ([ctx] (inline-stage (:runtime ctx) (:partition-id ctx)))
  ([rt pid] (inline-stage rt pid (default-dbname-labels rt)))
  ([rt pid dbname-labels]
   (let [tabs-definition (mapv (fn [{:keys [id] s-label :label}]
                                 {:id id
                                  :href "#"
                                  :label [:span
                                          {:style {:border-color (domain/database-color (hf/domain rt) id)}
                                           :class (when (contains? (dirty-dbs rt pid) id) "stage-dirty")}
                                          s-label]})
                               dbname-labels)
         selected-dbname (r/cursor (state/state rt) [:staging/selected-uri])
         selected-dbname' (r/fmap-> selected-dbname (default-tab-model (mapv :id dbname-labels)))
         label (->> dbname-labels (some #(when (= (:id %) @selected-dbname') (:label %))))]
     [:div.hyperfiddle-staging-editor-cmp
      [tab-content rt pid selected-dbname'
       [:div.hyperfiddle-stage-actions {:style {:display "flex"}}
        [:div {:style {:margin-left "auto"}} "stage: "
         [anchor-tabs
          :model selected-dbname'
          :tabs tabs-definition
          :on-change (r/partial reset! selected-dbname)]
         [transact-button rt pid selected-dbname' label]]]]])))
