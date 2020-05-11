(ns hyperfiddle.query
  (:require
    [contrib.ct :refer [unwrap]]
    [contrib.try$ :refer [try-either]]
    [datomic.api :as d]
    [hypercrud.browser.q-util :as q-util]
    [hyperfiddle.fiddle :as fiddle]))


; These are used from datalog e.g. [(hyperfiddle.query/attr-archived? ?attr)]

(defn attr-archived? [$ e-attr]
  (let [{a :db/ident} (d/pull $ [:db/ident] e-attr)]
    ; "zzz." and "zzz/"
    (some-> (namespace a) (clojure.string/starts-with? "zzz"))))

; https://forum.datomic.com/t/is-this-a-bug-in-datalog-symbol-resolution-with-not/613
(def attr-not-archived? (complement attr-archived?))

(defn attr-datomic? [$ e-attr]
  (<= e-attr 62))

(defn entity-creation-tx [$ e]
  (->> (d/q '[:find [?time ...]
              :in $ ?e
              :where
              [?e _ _ ?tx]
              [?tx :db/txInstant ?time]]
            $ e)
       sort
       first))

(comment
  (def $ (db! "datomic:free://datomic:4334/hyperfiddle-users"))
  (->> (d/q
         '[:find
           ?name
           ?t
           :where
           [?user :user/user-id]
           [?user :user/name ?name]
           [(user/entity-creation-tx $ ?user) ?t]]
         $)
       (sort-by first))
  )