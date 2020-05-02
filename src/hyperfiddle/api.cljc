(ns hyperfiddle.api                                         ; cljs can always import this
  (:require
    [clojure.spec.alpha :as s]
    [taoensso.timbre :as timbre]))


; This protocol can be multimethods
(defprotocol Browser
  (a [ctx])
  (attr [ctx] [ctx a])
  (browse-element [ctx i])
  (data [ctx])
  (eav [ctx])
  (e [ctx])
  (element [ctx])
  (element-type [ctx])
  (fiddle [ctx])
  (identity? [ctx])
  (link-tx [ctx])
  (qfind [ctx])
  (qfind-level? [ctx])
  (spread-attributes [ctx])
  (id [ctx pulltree])
  (row-key [ctx row])
  (tempid! [ctx] [ctx dbname])
  (v [ctx]))

(defprotocol UI
  (display-mode [ctx])
  (display-mode? [ctx k]))

;(def def-validation-message hypercrud.browser.context/def-validation-message)
; Circular dependencies and :require order problems. This is a static operation, no ctx dependency.
; But hyperfiddle.api can only have protocols, no concrete impls for the require order to work.
; Protocols and methods need a dispatch parameter, and static fns don't have one.
(defmulti def-validation-message (fn [pred & [s]] :default)) ; describe-invalid-reason

(defmulti tx (fn [ctx eav props]
               (let [dispatch-v (hyperfiddle.api/link-tx ctx)]
                 ; UX - users actually want to see this in console
                 (timbre/info "hf/tx: " dispatch-v " eav: " (pr-str eav))
                 dispatch-v)))

(declare render-dispatch)

; Dispatch is a set
(defmulti render (fn [ctx props]
                   (render-dispatch ctx props)))

(defn extract-set [ctx & fs]
  (->> ctx ((apply juxt fs)) set))

(defn render-dispatch [ctx props]
  ; Is there a method which is a subset of what we've got?
  (or
    (if (hyperfiddle.api/display-mode? ctx :user)
      (or
        (let [d (extract-set ctx hyperfiddle.api/fiddle hyperfiddle.api/a)]
          (if (contains? (methods render) d)
            d))
        (let [d (extract-set ctx hyperfiddle.api/a)]
          (if (contains? (methods render) d)
            d))))
    ; Legacy compat - options by fiddle/renderer explicit props route to select via ref renderer
    (if (:options props)
      (extract-set (hyperfiddle.api/attr ctx) :db/valueType :db/cardinality))
    (if (hyperfiddle.api/identity? ctx) #{:db.unique/identity})
    (if-let [attr (hyperfiddle.api/attr ctx)]
      (extract-set attr :db/valueType :db/cardinality))
    (if (hyperfiddle.api/element ctx)
      (extract-set ctx hyperfiddle.api/element-type))                    ; :hf/variable, :hf/aggregate, :hf/pull
    ;(contrib.datomic/parser-type (context/qfind ctx))       ; :hf/find-rel :hf/find-scalar
    ;:hf/blank
    ))

#?(:clj (def ^:dynamic *route* nil))
#?(:clj (def ^:dynamic *$* nil))

(defmethod tx :default [ctx eav props]
  nil)

(defmethod tx :default [ctx eav props]
  nil)

(defmethod tx :zero [ctx eav props]
  [])                                                       ; hack to draw as popover

(defmethod tx :db/add [ctx [e a v] props]
  {:pre [e a v]}
  [[:db/add e a v]])

(defmethod tx :db/retract [ctx [e a v] props]
  {:pre [e a v]}
  [[:db/retract e a v]])

(defmethod tx :db/retractEntity [ctx [e a v] props]
  {:pre [v]}
  [[:db/retractEntity v]])

; Compat

(defmulti txfn (fn [user-txfn e a v ctx] user-txfn))

(defmethod txfn :default [_ e a v ctx]
  nil)

(defmethod txfn :zero [_ e a v ctx]
  [])                                                       ; hack to draw as popover

(defmethod txfn :db/add [_ e a v ctx]
  {:pre [e a v]}
  [[:db/add e a v]])

(defmethod txfn :db/retract [_ e a v ctx]
  {:pre [e a v]}
  [[:db/retract e a v]])

(defmethod txfn :db/retractEntity [_ _ _ v ctx]
  {:pre [v]}
  [[:db/retractEntity v]])

; All hyperfiddle specs should be here in this namespace.
; Namespaces other than "hyperfiddle" are henceforth forbidden and all legacy namespaces should be migrated.
; If you feel the need to organize attributes that "traverse together" use a comment in this file
; or (future) use spec2 schema & select.

(s/def ::invalid-messages (s/coll-of string?))
(s/def ::is-invalid boolean?)
