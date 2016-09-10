(ns hypercrud.ui.multi-select
  (:require [hypercrud.ui.auto-control :refer [auto-control]]))


(defmulti multi-select-markup (fn [click-add! control-tuples] :default))


(defn multi-select* [markupfn value add-item! {:keys [change! field] :as widget-args}]
  (let [control-tuples (seq (mapv (fn [v]
                                    (let [click-remove! #(change! [v] nil)
                                          new-args (-> widget-args
                                                       (assoc-in [:field :attribute/cardinality] :db.cardinality/one)
                                                       (update :expanded-cur #(% [(:attribute/ident field)] {})))
                                          control [auto-control v new-args]]
                                      [v click-remove! control]))
                                  value))]
    (markupfn add-item! control-tuples)))


(defmethod multi-select-markup :default [click-add! control-tuples & [css-class]]
  [:div.multi-select {:class css-class}
   (map (fn [[v click-remove! control]]
          ^{:key (str v)}                                   ;(str v) so this works when v is nil
          [:div.multi-select-group
           [:button {:on-click click-remove!} "-"]
           control])
        control-tuples)
   [:button {:on-click click-add!} "+"]])
