(ns hyperfiddle.ui.util
  (:require
    [clojure.set :as set]
    [contrib.datomic-tx :as tx]
    [contrib.reactive :as r]
    [contrib.string :refer [empty->nil]]
    [hypercrud.browser.context :as context]
    [hyperfiddle.api :as hf]
    [hyperfiddle.security]
    [hyperfiddle.runtime :as runtime]
    [taoensso.timbre :as timbre]))

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
  #_(js/console.log `with-entity-change! ctx)
  (r/comp (r/partial runtime/with-tx (:runtime ctx) (:partition-id ctx) (context/dbname ctx)) ; [ctx vorvs]
          (r/partial identity)                              ; translate ui tx to database tx
          (r/partial entity-change->tx ctx)))               ; [tx]

(defn with-entity-change-route!
  ; Curried both ways for backwards compat with `with-entity-change!
  ([ctx]
   (r/partial with-entity-change-route! ctx)
   #_(fn [vorvs] (with-entity-change-route! ctx vorvs)))
  ([ctx vorvs]
   ; Note we are ignoring EAV in ctx, unlike the tx version
   (hf/swap-route! ctx assoc-in (:hypercrud.browser/result-path ctx) vorvs))
  ([ctx _old new]
   (with-entity-change-route! ctx new)))

(defn with-entity-tx!
  [ctx tx]
  (runtime/with-tx (:runtime ctx) (:partition-id ctx) (context/dbname ctx) tx))
