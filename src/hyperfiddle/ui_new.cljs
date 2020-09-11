(ns hyperfiddle.ui-new
  (:require
   [clojure.walk :as walk]
   [contrib.dataflow :as df]
   [contrib.polymorphism :as p]
   [hyperfiddle.context :as context]
   [hyperfiddle.runtime :as runtime]
   [hyperfiddle.spec :as s]
   [hyperfiddle.ui.util :as util]
   [reagent.core :as reagent])
  (:require-macros
   [hyperfiddle.ui :refer [component]]))


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

; (defn cell-
;   [<ctx>]
;   (|> [value (->> <ctx> (df/map ::context/value))]
;     [:td {} [:input {:on-change (on-change @<ctx>) :value @value}]]))
;
; (defn row-
;   [<ctx> columns]
;   (|> [<id> (df/map ::context/entity <ctx>)
;        <cells> (->> <ctx>
;                  (df/map context/focus*)
;                  (df/map (fn [ctxs] (select-keys ctxs columns)))
;                  df/sequence-map
;                  (df/map vals)
;                  (df/mmap (fn [<ctx>] [cell- <ctx>]))
;                  (df/map (fn [cells] (into [:tr {:key @<id>}] cells))))]
;     @<cells>))
;
; (defn table-
;   [<ctx>]
;   (|> [<columns> (->> <ctx>
;                       (df/map columns))
;        <thead>   (->> <columns>
;                       (df/mmap (fn [column] [:td (str column)]))
;                       (df/map (fn [columns] (into [:tr {}] columns)))
;                       (df/map (fn [header-row] [:thead {} header-row])))
;        <tbody> (->> <ctx>
;                     (df/map context/focus*)
;                     df/sequence
;                     (df/mmap (fn [<ctx>] [row- <ctx> @<columns>]))
;                     (df/map (fn [rows] (into [:tbody {}] rows))))
;        <tbody> (->> <ctx>
;                     (df/map context/focus*)
;                     df/sequence
;                     (df/mmap (fn [<ctx>] [row- <ctx> @<columns>]))
;                     (df/map (fn [rows] (into [:tbody {}] rows))))]
;     [:table
;      @<thead>
;      @<tbody>]))


'[[::s/coll :ret] ::s/keys]

(defn spec|matches-current?
  [pattern ctx]
  (let [pattern (if (vector? pattern) pattern [pattern])
        [type focus] pattern]
    (cond
      (= :? type)
      true

      (fn? type)
      (apply type [ctx])

      :else
      (and (= type (:type (::context/spec ctx)))
           (or (nil? focus)
               (= focus (::context/focus ctx)))))))

(defn spec|matches-parents?
  [pattern ctx]
  (or (empty? pattern)
      (let [p (peek pattern)
            p (if (vector? p) p [p])
            [type focus] p]
        (and (spec|matches-current? p ctx)
             (or (not (contains? ctx ::context/parent))
                 (spec|matches-parents? (pop pattern) (::context/parent ctx)))))))

(defn spec|matches-children?
  [pattern ctx]
  (or (empty? pattern)
      (let [p (first pattern)
            p (if (vector? p) p [p])
            [type focus] p]
        (and (spec|matches-current? p ctx)
             (or (::context/leaf ctx)
                 (spec|matches-children? (rest pattern)
                                         (if focus
                                           (context/focus ctx focus)
                                           (context/focus ctx))))))))


[[::s/fn :ret] :-> ::s/coll ::s/keys]

(defn current-pattern
  [pattern]
  (let [target-index (.indexOf pattern :->)]
    (if (= -1 target-index)
      (pop pattern)
      (nth pattern (inc target-index)))))

(defn parse-pattern
  [pattern]
  (let [target-index (.indexOf pattern :->)]
    (if (= -1 target-index)
      [(pop pattern) (peek pattern)]
      [(subvec pattern 0 target-index)
       (nth pattern (inc target-index))
       (subvec pattern (inc (inc target-index)))])))

(defn spec|matches?
  [pattern ctx]
  (if (vector? pattern)
    (let [[parents-pattern current-pattern children-pattern] (parse-pattern pattern)
          children-ctx (when (and (seq children-pattern) (context/focusable? ctx))
                         (let [focus (when (vector? children-pattern) (second children-pattern))]
                           (if focus (context/focus ctx focus) (context/focus ctx))))]
      ; (println
      ;      (:type (::context/spec ctx)) pattern (and (spec|matches-parents? parents-pattern ctx)
      ;                                                (spec|matches-children? children-pattern children-ctx)
      ;                                                (spec|matches-current? current-pattern ctx))
      ;      \newline
      ;      parents-pattern (spec|matches-parents? parents-pattern (::context/parent ctx))
      ;      \newline
      ;      current-pattern (spec|matches-current? current-pattern ctx)
      ;      \newline
      ;      children-pattern (spec|matches-children? children-pattern children-ctx))
      (if (< (count parents-pattern) (count children-pattern))
        (and (spec|matches-current? current-pattern ctx)
             (spec|matches-parents? parents-pattern (::context/parent ctx))
             (spec|matches-children? children-pattern children-ctx))
        (and (spec|matches-current? current-pattern ctx)
             (spec|matches-children? children-pattern children-ctx)
             (spec|matches-parents? parents-pattern (::context/parent ctx)))))

    (spec|matches-current? pattern ctx)))

(defn spec|better?
  [p0 p1]
  ; (println p0 p1 (> (if (seqable? p0) (count p0) 1) (if (seqable? p1) (count p1) 1)))
  (if (> (if (seqable? p0) (count p0) 1) (if (seqable? p1) (count p1) 1))
    p0
    p1))

(def render (p/XMultiFn. 'render
                         (fn [<ctx> props]
                           @<ctx>)
                         spec|matches?
                         spec|better?
                         :default
                         (get-global-hierarchy)
                         (atom {})
                         (atom {})
                         (atom {})
                         (atom {})))

(defmethod render :default
  [<ctx> props]
  (component
   [:div props
    [:pre ~@(str ~(:type ~(::context/spec <ctx>)))]]))

(defmethod render ::s/fn
  [<ctx> props]
  (component
   [:div.fn props
     [render ~(context/focus <ctx> :ret)]]))

(defmethod render [:-> ::s/coll ::s/keys]
  [<ctx> props]
  (component
   [:table.coll props
    [:thead.coll props
     (into [:tr] ~@(map (fn [k] [:td [:h5 (str k)]]) ~(:keys ~(::context/spec ~(context/focus <ctx>)))))]
    [:tbody
     ~@(map (fn [<ctx> i] ^{:key [i]} [render <ctx> props]) (df/sequence ~(context/focus* <ctx>)) (range))]]))

(defmethod render [::s/coll ::s/keys]
  [<ctx> props]
  (component
   [:tr.keys props
    ~@(map (fn [[k <ctx>]] ^{:key k} [:td [render <ctx> props]])
           (df/sequence-map ~(context/focus* <ctx>)))]))

(defmethod render [::s/fn ::s/keys]
  [<ctx> props]
  [:div.keys props
   ~@(map (fn [[k <ctx>]]
            ^{:key k}
            [:div
             k
             [render <ctx> props]])
          (df/sequence-map ~(context/focus* <ctx>)))])

(defmethod render ::s/identifier
  [<ctx> props]
  (component
   [render ~(context/focus <ctx>)]))

(defmethod render [::s/identifier ::s/predicate]
  [<ctx> props]
  (component
   [:a {:href ~@(str ~(::context/entity <ctx>))} ~@(::context/value <ctx>)]))

(defmethod render ::s/predicate
  [<ctx> props]
  (component
   [:input (merge props {:value ~@(::context/value <ctx>)})]))

(defn ui0-adapter
  [{:keys [:hypercrud.browser/result] :as ctx}]
  (let [<ctx> (df/input nil)]
    (df/consume identity <ctx>)
    (reagent/create-class
     {:component-did-mount
      (fn []
        (df/put <ctx> (context/ctx0-adapter ctx)))
      :render
      (fn [this]
        (doseq [[k v] ctx]
          (when (satisfies? IDeref v)
            @v))
        (when-not (.-firstRender ^js this)
          (set! (.-firstRender ^js this) true)
          (df/put <ctx> (context/ctx0-adapter ctx)))
        (js/setTimeout (fn [] (df/put <ctx> (context/ctx0-adapter ctx)) 1))
        (when @<ctx>
          [render <ctx> {}]))})))
