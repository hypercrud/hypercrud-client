(ns hyperfiddle.ui-new
  (:require
   [contrib.data :refer [empty->nil]]
   [contrib.dataflow :as df]
   [datascript.parser :as datascript]
   [hyperfiddle.api :as hf]
   [hyperfiddle.context :as context]
   [hyperfiddle.runtime :as runtime]
   [hyperfiddle.ui.util :as util]
   [reagent.core :as reagent])
  (:require-macros
   [hyperfiddle.ui :refer [|>]]))

(defn react
  [df]
  (let [force (reagent/atom @df)]
    (->> df
         df/dedupe
         (df/consume (fn [& val] (reset! force val))))
    @force))

(defn fiddle-shape-columns
  [ctx]
  (some->> ctx
           ::context/fiddle
           :fiddle/shape
           datascript/parse-query
           :qfind
           :elements
           (mapcat (comp :value :pattern))))

(defn data-columns
  [ctx]
  (some->> ctx
           ::context/value
           (reduce (fn [acc row] (into acc (keys row))) #{})))

(defn columns
  [ctx & [props]]
  (or
    (empty->nil (fiddle-shape-columns ctx))
    (empty->nil (data-columns ctx))))

(defn on-change
  [ctx]
  (let [last (atom (::value ctx))]
    (fn [o n]
      (runtime/with-tx
        (context/youngest ctx ::context/runtime)
        (context/youngest ctx ::context/partition-id)
        "$"
        [[:db/add (context/youngest ctx ::context/entity) (::context/attribute ctx) n]]))))

(defn cell-
  [<ctx>]
  (|> [value (->> <ctx> (df/map ::context/value))]
    [:td {} [:input {:on-change (on-change @<ctx>) :value @value}]]))

(defn row-
  [<ctx> columns]
  (|> [<id> (df/map context/entity <ctx>)
       <cells> (->> <ctx>
                 (df/map context/focus*)
                 (df/map (fn [ctxs] (select-keys ctxs columns))))]
    (into
      [:tr {:key @<id>}]
      (map (fn [<ctx>] [cell- <ctx>])
           (vals (df/sequence <cells>))))))

(defn table-
  [<ctx>]
  (|> [<columns> (->> <ctx> (df/map columns))
       <empty?> (->> <ctx> (df/map ::context/value) (df/map empty?))
       <rows> (->> <ctx> (df/map context/focus*))]
    (df/consume println <ctx>)
    [:table
     [:thead {}
      (->> @<columns>
         (map (fn [v] [:td (str v)]))
         (apply (fn [tds] (into [:tr {}] tds))))]
     (when-not @<empty?>
       (into
        [:tbody {}]
        (map (fn [<ctx>] [row- <ctx> @<columns>]) (df/sequence <rows>))))]))


(defonce <ctx> (df/input nil (fn [] (println 'open!) (fn [] (println 'close!)))))
(defonce indirection (atom nil))
(add-watch indirection ::watcher (fn [k r o n]
                                   (df/put <ctx> n)))

(defn ui0-adapter
  [{:keys [:hypercrud.browser/result] :as ctx}]
  (reagent/create-class
   {:constructor
    (fn [_]
      (df/put <ctx> (context/ctx0-adapter ctx)))
    :render
    (fn [x]
      (js/setTimeout (fn [] (reset! indirection (context/ctx0-adapter (second (.. ^js x -props -argv)))) 0))
      [table- <ctx>])}))
