(ns hyperfiddle.ui.util
  (:require
    [contrib.datomic-tx :as tx]
    [contrib.reactive :as r]
    [contrib.string :refer [empty->nil]]
    [hypercrud.browser.context :as context]
    [hyperfiddle.api :as hf]
    [hyperfiddle.security]
    [hyperfiddle.runtime :as runtime]
    [taoensso.timbre :as timbre]))


(defn entity-change->tx                                     ; :Many editor is probably not idiomatic
  ([ctx vorvs]
   ; wut is going on with eav here in :many case
   ; the parent would still be in scope i guess
   (let [[_ a v] @(:hypercrud.browser/eav ctx)
         o (if (not= :db.type/ref (contrib.datomic/valueType @(:hypercrud.browser/schema ctx) a))
             v                                              ;(get entity a)      ; scalar
             (case (contrib.datomic/cardinality @(:hypercrud.browser/schema ctx) a) ; backwards refs good here? lol
               :db.cardinality/one v                        ;(context/smart-entity-identifier ctx (get entity a))
               :db.cardinality/many (map (partial context/smart-entity-identifier ctx) vorvs)))]
     (entity-change->tx ctx o vorvs)))
  ([ctx o n]
   (let [[e a v] @(:hypercrud.browser/eav ctx)
         attribute (context/hydrate-attribute! ctx a)
         component? (context/attr? (:hypercrud.browser/parent ctx) :db/isComponent)
         n' (empty->nil n)]
     ; hack for garbage string controls
     (when (and (some? n) (nil? n'))
       (timbre/warn "Trimming empty value to nil. This will be removed in a future release"))
     (if (and component?
              (or (nil? e)
                  (and (vector? e) (= (first e) a))))
       (let [[pe pa pv] @(:hypercrud.browser/eav (:hypercrud.browser/parent ctx))
             pe (runtime/id->tempid! (:runtime ctx) (:partition-id ctx) "$" pe)
             e (str pe pa)
             tx (tx/edit-entity e attribute o n')]
         (into [{:db/id pe pa {:db/id e}}] tx))
       (tx/edit-entity e attribute o n')))))

(defn ^:deprecated with-tx!
  ([ctx tx]
   (timbre/warn "deprecated. invoke runtime/with-tx directly")
   (runtime/with-tx (:runtime ctx) (:partition-id ctx) (context/dbname ctx) tx))
  ([ctx dbname tx]
   (timbre/warn "deprecated. invoke runtime/with-tx directly")
   (runtime/with-tx (:runtime ctx) (:partition-id ctx) dbname tx)))

(defn with-entity-change! [ctx]
  (r/comp (r/partial runtime/with-tx (:runtime ctx) (:partition-id ctx) (context/dbname ctx))
          (r/partial entity-change->tx ctx)))
