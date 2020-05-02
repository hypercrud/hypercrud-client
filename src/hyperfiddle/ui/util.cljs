(ns hyperfiddle.ui.util
  (:require
    [contrib.datomic-tx :as tx]
    [contrib.reactive :as r]
    [contrib.string :refer [empty->nil]]
    [hypercrud.browser.context :as context]
    [hyperfiddle.api :as hf]
    [hyperfiddle.security]
    [hyperfiddle.security.client :as security]
    [hyperfiddle.runtime :as runtime]
    [taoensso.timbre :as timbre]
    [hyperfiddle.domain :as domain]))


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
         n' (empty->nil n)]                                 ; hack for garbage string controls
     (when (and (some? n) (nil? n'))
       (timbre/warn "Trimming empty value to nil. This will be removed in a future release"))
     (tx/edit-entity e attribute o n'))))

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

(defn writable-entity? [ctx]
  (and
    ; If the db/id was not pulled, we cannot write through to the entity
    (boolean (hf/e ctx))
    @(r/track security/writable-entity? (:hypercrud.browser/parent ctx))))

(defn writable-attr? [ctx]
  (let [domain (runtime/domain (:runtime ctx))
        database (domain/database domain (context/dbname ctx))]
    (if-not (= (get-in database [:database/write-security :db/ident] ::security/allow-anonymous)
              :hyperfiddle.security/tx-operation-whitelist)
      true
      (let [{a :db/ident is-whitelisted :hyperfiddle/whitelist-attribute} (hf/attr ctx)]
        (or (true? is-whitelisted)
          ((set (:hf/transaction-operation-whitelist database)) a))))))
