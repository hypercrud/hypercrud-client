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
    (fn [e]
      (let [n (.. e -target -value)]
        (runtime/with-tx
          (context/youngest ctx ::context/runtime)
          (context/youngest ctx ::context/partition-id)
          "$"
          [[:db/add (context/youngest ctx ::context/entity) (::context/attribute ctx) n]])))))

(defn cell-
  [<ctx>]
  (|> [value (->> <ctx> (df/map ::context/value))]
    [:td {} [:input {:on-change (on-change @<ctx>) :value @value}]]))

(defn row-
  [<ctx> columns]
  (|> [<id> (df/map context/entity <ctx>)
       <cells> (->> <ctx>
                 (df/map context/focus*)
                 (df/map (fn [ctxs] (select-keys ctxs columns)))
                 df/sequence-map
                 (df/map vals)
                 (df/mmap (fn [<ctx>] [cell- <ctx>]))
                 (df/map (fn [cells] (into [:tr {:key @<id>}] cells))))]
    @<cells>))

(defn table-
  [<ctx>]
  (|> [<columns> (->> <ctx>
                      (df/map columns))
       <thead>   (->> <columns>
                      (df/mmap (fn [column] [:td (str column)]))
                      (df/map (fn [columns] (into [:tr {}] columns)))
                      (df/map (fn [header-row] [:thead {} header-row])))
       <tbody> (->> <ctx>
                    (df/map context/focus*)
                    df/sequence
                    (df/mmap (fn [<ctx>] [row- <ctx> @<columns>]))
                    (df/map (fn [rows] (into [:tbody {}] rows))))
       <xbody> (->> <ctx>
                    (df/map context/focus*)
                    df/sequence
                    (df/mmap (fn [<ctx>] [row- <ctx> @<columns>]))
                    (df/map (fn [rows] (into [:tbody {}] rows))))
       <tbody> (->> <ctx>
                    (df/map context/focus*)
                    df/sequence
                    (df/mmap (fn [<ctx>] [row- <ctx> @<columns>]))
                    (df/map (fn [rows] (into [:tbody {}] rows))))]
    [:table
     @<thead>
     @<tbody>]))


(defonce <ctx> (df/input nil))

(defn ui0-adapter
  [{:keys [:hypercrud.browser/result] :as ctx}]
  (reagent/create-class
   {:constructor
    (fn [_]
      (df/put <ctx> (context/ctx0-adapter ctx)))
    :render
    (fn [x]
      (doseq [[k v] ctx]
        (when (satisfies? IDeref v)
          @v))
      (js/setTimeout (fn [] (df/put <ctx> (context/ctx0-adapter ctx))) 0)
      [table- (df/remove nil? <ctx>)])}))
