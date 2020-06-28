(ns hyperfiddle.ui.error
  (:require
    [contrib.pprint :refer [pprint-str]]
    [contrib.reactive :as r]
    [hypercrud.types.Err :as Err]
    [hyperfiddle.ui.staging :as staging]))


(defn e->map [e]
  (cond
    (Err/Err? e) {:message (:msg e)
                  :data (:data e)}
    (map? e) e
    (string? e) {:message e}
    :else {:message (ex-message e)
           :data (ex-data e)
           :cause (ex-cause e)}))

(defn ex-data->human-detail [{:keys [ident error-msg] :as data}]
  (or error-msg (pprint-str data)))

(defn error-inline [e & [props]]
  (let [{:keys [cause data message]} (e->map e)]
    [:span props
     (str message #_#_" " (str " -- " (ex-data->human-detail data)))]))

(defn error-block [e & [props]]
  (let [{:keys [cause data message]} (e->map e)]            ; we don't always return an error with a message
    [:div props
     [:h3 message]
     [:pre (str (ex-data->human-detail data))]
     (if (:human-hint data) [:p (:human-hint data)])]))

(defn error-block-with-stage [ctx e & [props]]
  (let [selected-dbname (r/atom nil)]
    (fn [e & [props]]
      [:<>
       [error-block e props]
       [staging/editor-cmp selected-dbname ctx]])))

(defn error-comp [ctx]
  (cond
    (> (count (:hypercrud.browser/pull-path ctx)) 0) error-inline

    (:hyperfiddle.ui/error-with-stage? ctx) (r/partial error-block-with-stage ctx)

    :else error-block))
