(ns hyperfiddle.ui.util
  (:require
    [clojure.set :as set]
    [hyperfiddle.transaction :as tx]
    [contrib.reactive :as r]
    [contrib.data :as data]
    [contrib.string :refer [empty->nil]]
    [hypercrud.browser.context :as context]
    [hyperfiddle.api :as hf]
    [hyperfiddle.security]
    [hyperfiddle.runtime :as runtime]
    [taoensso.timbre :as timbre]
    [clojure.spec.alpha :as s]
    [hyperfiddle.spec :as spec]))

(defn change-tx
  [e a o n]
  "Creates transactions based on value e.a changing from o to n
   Has the caveat that it creates extraneous retracts when dealing
   with cardinality/one case."
  (cond-> [] (some? o) (conj [:db/retract e a o])
             (some? n) (conj [:db/add e a n])))

(defn ^:export picker-tx [e a rets adds]                    ; used directly in Rosie, don't change
  (vec (concat
         (map (fn [v] [:db/retract e a v]) rets)
         (map (fn [v] [:db/add e a v]) adds))))

(defn entity-change->tx
  [ctx o n]                                                 ; idea - (f e a rets adds)
  (let [[e a v] @(:hypercrud.browser/eav ctx)
        attribute (context/hydrate-attribute! ctx a)
        component? (context/attr? (:hypercrud.browser/parent ctx) :db/isComponent)
        n' (empty->nil n)
        {a :db/ident {cardinality :db/ident} :db/cardinality} (context/hydrate-attribute! ctx a)
        [pe pa pv] @(:hypercrud.browser/eav (:hypercrud.browser/parent ctx))
        {component? :db/isComponent} (context/hydrate-attribute! (:hypercrud.browser/parent ctx) pa)]
    (case cardinality
      :db.cardinality/one
      (if (and component?
               (or (nil? e)
                   (and (vector? e) (= a (first e)))))
        (let [e (str pe pa)]
          (into
            [(if (vector? pe)
               (conj {pa e} pe)
               (let [pe (runtime/id->tempid! (:runtime ctx) (:partition-id ctx) (context/dbname ctx) pe)]
                 {:db/id pe pa e}))]
            (change-tx e a o n)))
        (change-tx e a o n))

      :db.cardinality/many
      (let [o (set o)
            n (set n)]
        (picker-tx e a
          (set/difference o n)
          (set/difference n o))))))

(defn ^:deprecated with-tx!
  ([ctx tx]
   (timbre/warn "deprecated. invoke runtime/with-tx directly")
   (runtime/with-tx (:runtime ctx) (:partition-id ctx) (context/dbname ctx) tx))
  ([ctx dbname tx]
   (timbre/warn "deprecated. invoke runtime/with-tx directly")
   (runtime/with-tx (:runtime ctx) (:partition-id ctx) dbname tx)))

(defn with-entity-change! [ctx] #_[tx]
  (r/comp (r/partial runtime/with-tx (:runtime ctx) (:partition-id ctx) (context/dbname ctx)) ; [ctx vorvs]
          (r/partial identity)                              ; translate ui tx to database tx
          (r/partial entity-change->tx ctx)))               ; [tx]


(defn position-in [key [f & args]]
  (when-let [spec (s/get-spec f)]
    (let [index-of  #(.indexOf %2 %1)
          spec-args (-> spec (spec/parse) (spec/args-spec))
          names     (spec/names spec-args)]
      (assert (= ::spec/cat (:type spec-args)) (str "Please provide an s/cat args spec for this fiddle. " f))
      (->> (index-of key (vec names))))))

(defn- expand-route-to [args-count route]
  (-> (repeat (inc args-count) nil)
      (data/zipseq route)
      (vec)))

(defn- assoc-in-route [route path value]
  (let [[position :as path] (-> path
                                (update 0 position-in route)
                                (update 0 inc))]
    (if (<= position (count route))
      (assoc-in route path value)
      (assoc-in (expand-route-to position route) path value))))

(defn with-entity-change-route!
  ; Curried both ways for backwards compat with `with-entity-change!
  ([ctx]
   (r/partial with-entity-change-route! ctx)
   #_(fn [vorvs] (with-entity-change-route! ctx vorvs)))
  ([ctx vorvs]
   ; Note we are ignoring EAV in ctx, unlike the tx version
   (hf/swap-route! ctx assoc-in-route (:hypercrud.browser/result-path ctx) vorvs))
  ([ctx _old new]
   (with-entity-change-route! ctx new)))

(defn change-handler
  [ctx]
  (fn [e {rets :- adds :+}]
    (runtime/with-tx
      ctx
      (into (mapv (fn [[a v]] [:db/retract e a v]) rets)
            (mapv (fn [[a v]] [:db/add e a v]) adds)))))
