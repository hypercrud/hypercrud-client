(ns hyperfiddle.ui-new0
  (:require
   [clojure.walk :as walk]
   [contrib.data :refer [map-by]]
   [contrib.dataflow :as df]
   [contrib.polymorphism :as p]
   [contrib.ui.tooltip :refer [tooltip-thick]]
   [hyperfiddle.context :as context]
   [hyperfiddle.route :as route]
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


[:-> ::s/coll ::s/keys]

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
  (if (> (if (seqable? p0) (count p0) 1) (if (seqable? p1) (count p1) 1))
    p0
    p1))

(def render (p/XMultiFn. 'render
                         (fn [<ctx> props]
                           @<ctx>)
                         spec|matches?
                         spec|better?
                         (fn [f & args] (apply f args))
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
  (component
   [:div.keys props
    ~@(map (fn [[k <ctx>]]
             ^{:key k}
             [:div.field
              [tooltip-thick ~@(str ~(::context/spec <ctx>))
               (let [label-props (select-keys props [:on-click :class])] ; https://github.com/hyperfiddle/hyperfiddle/issues/511
                 [:label.hyperfiddle props k #_(if help-md [:sup "†"])])]
              [:div
               [render <ctx> props]]])
           (df/sequence-map ~(context/focus* <ctx>)))]))

(defmethod render ::s/identifier
  [<ctx> props]
  (component
   [render ~(context/focus <ctx>)]))

(defmethod render [::s/identifier ::s/predicate]
  [<ctx> props]
  (component
   @(let [fiddle ~(context/youngest <ctx> ::context/fiddle)
          link ~(get ~(map-by :link/path ~(:fiddle/links fiddle)) ~(::context/attribute <ctx>))
          route ~(list ~(:fiddle/ident ~(:link/fiddle link)) ~(::context/entity <ctx>))]

      (if @(react link)
        ~[:a ~{:href ~(route/url-encode route nil)} ~(::context/value <ctx>)]
        ~[:input ~{:value ~(::context/value <ctx>)}]))))

(defmethod render ::s/predicate
  [<ctx> props]
  (component
    ~@[:input ~{:value ~(::context/value <ctx>)}]))

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
