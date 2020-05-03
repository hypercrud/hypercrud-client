(ns hyperfiddle.security.client
  (:require
    [cats.core :refer [mlet return]]
    [cats.monad.maybe :refer [from-maybe]]
    [cats.monad.either :refer [right left]]
    [contrib.ct :refer [maybe]]
    [contrib.datomic]
    [hyperfiddle.api :as hf]
    [hyperfiddle.runtime :as runtime]))


(defn parent-m [ctx]
  (if (:db/isComponent (hf/attr ctx (hf/a ctx)))
    (parent-m (:hypercrud.browser/parent ctx))
    (hf/data ctx)))

(defn new-entity? [rt pid dbname dbid]
  (or (contrib.datomic/tempid? dbid)
    (some-> (get (runtime/get-tempid-lookup! rt pid dbname) dbid)
      some?)
    (if-let [parent-id (runtime/parent-pid rt pid)]
      (new-entity? rt parent-id dbname dbid)
      false)))

(defmethod hf/subject-may-transact+ ::entity-ownership [hf-db subject]
  (if (some? subject) (right) (left "Please login")))

(defmethod hf/subject-may-create? ::entity-ownership [hf-db subject ctx]
  (some? subject))

(defmethod hf/subject-may-edit-entity? ::entity-ownership [ctx]
  (let [subject (hf/subject ctx)]
    (and (some? subject)
      (or (contains? (set (:hyperfiddle/owners (hf/db-record ctx))) subject)
        (-> (mlet [m (maybe (parent-m ctx))
                   dbname (maybe (hf/dbname ctx))]
              (return (or (new-entity? (:runtime ctx) (:partition-id ctx) dbname (:db/id m))
                        (contains? (set (:hyperfiddle/owners m)) subject))))
          ; ui probably in an invalid/error state when m or uri are nil
          (from-maybe false))))))
