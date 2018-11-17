(ns hyperfiddle.api
  (:require
    [hyperfiddle.tempid :refer [with-tempid-color tempid-from-ctx tempid-from-stage]]))


(defn ^:export tempid-child "stable and idempotent, but implemented through parent-child ctx"
  [ctx val]
  (with-tempid-color ctx tempid-from-ctx))

; This returns a new value each time the transaction changes - can't call it again later.
; So tx-fns must inspect the modal-route, they can't re-create the dbid.
(defn ^:export tempid-detached "unstable but guaranteed unique tempid"
  ([dbname ctx]
   (with-tempid-color dbname ctx (partial tempid-from-stage dbname)))
  ([ctx]
   (with-tempid-color ctx tempid-from-stage)))

(defmulti txfn (fn [user-txfn e a v ctx] user-txfn))

(defmethod txfn :default [_ e a v ctx]
  nil)

(defmethod txfn :zero [_ e a v ctx]
  [])                     ; hack to draw as popover

(defmethod txfn :db/add [_ e a v ctx]
  [[:db/add e a v]])

(defmethod txfn :db/retract [_ e a v ctx]
  [[:db/retract e a v ctx]])

(defmethod txfn :db.fn/retractEntity [_ _ _ v ctx]
  [[:db.fn/retractEntity v]])
