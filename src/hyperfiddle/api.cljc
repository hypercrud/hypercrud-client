(ns hyperfiddle.api                                         ; cljs can always import this
  (:refer-clojure :exclude [memoize])
  (:require
    [cats.monad.either :refer [left right]]
    ; No hyperfiddle requires allowed due to cycles; hyperfiddle internals require us
    [cats.monad.either :as either :refer [right]]

    [clojure.spec.alpha :as s]
    [clojure.string :as str]
    [taoensso.timbre :as timbre]
    [hypercrud.types.ThinEntity :refer [thinentity?]]))     ; Exceptional hf require, the deftype should lift up to here or be removed


(defprotocol ConnectionFacade
  :extend-via-metadata true
  (basis [conn])                                            ; Warning: protocol #'hyperfiddle.api/Domain is overwriting method basis of protocol ConnectionFacade
  (db [conn])
  (transact [conn arg-map])                                 ; This is raw Datomic transact and does not perform hf/process-tx
  (with-db [conn]))

(defprotocol DbFacade
  :extend-via-metadata true
  (as-of [db time-point])
  (basis-t [db])
  (pull [db arg-map])
  (with [db arg-map])
  (history [db]))                                            ; TODO


; This protocol can be multimethods
(defprotocol Browser
  (a [ctx])
  (attr [ctx] [ctx a])
  (attr? [ctx corcs] [ctx a corcs])
  (browse-element [ctx i])
  (data [ctx])
  (dbname [ctx])
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

(defmulti subject (fn [ctx] (type ctx)))
(defmulti db-record (fn [ctx] (type ctx)))

(defprotocol UI
  (display-mode [ctx])
  (display-mode? [ctx k]))

(defprotocol Domain
  (basis [domain])
  (type-name [domain])
  (fiddle-dbname [domain])
  (database [domain dbname])                                ; database-record
  (databases [domain])
  (environment [domain])
  (url-decode [domain s])
  (url-encode [domain route])
  (api-routes [domain])
  #?(:clj (connect [domain dbname] [domain dbname on-created!])))

(defprotocol State
  (state [rt]))

(defprotocol HF-Runtime
  (domain [rt])
  (io [rt])
  (request [rt pid request])
  (set-route [rt pid route] [rt pid route force-hydrate] "Set the route of the given branch. This may or may not trigger IO. Returns a promise"))

(defn swap-route!
  [{:keys [:hypercrud.browser/route runtime partition-id]} f & args]
  (set-route runtime partition-id (apply f @route args)))

;(def def-validation-message hypercrud.browser.context/def-validation-message)
; Circular dependencies and :require order problems. This is a static operation, no ctx dependency.
; But hyperfiddle.api can only have protocols, no concrete impls for the require order to work.
; Protocols and methods need a dispatch parameter, and static fns don't have one.
(defmulti def-validation-message (fn [pred & [s]] :default)) ; describe-invalid-reason

;#?(:cljs)
(defmulti tx (fn [ctx eav props]                            ; you can get the eav from the ctx, but they always need it
               (let [dispatch-v (link-tx ctx)]
                 ; UX - users actually want to see this in console
                 (timbre/info "hf/tx: " dispatch-v " eav: " (pr-str eav))
                 dispatch-v)))

(defmulti process-tx                                   ; todo tighten params
  "clj only"
  (fn [$                                               ; security can query the database e.g. for attribute whitelist
       domain                                          ; spaghetti dependency, todo fix
       dbname
       #_hf-db                                         ; security can inspect domain/database configuration, e.g. for database-level user whitelist
       ; Removed to reduce parameter noise downstack - the one use case is able to reconstruct hf-db from [domain, dbname]
       subject                                         ; security can know the user submitting this tx
       tx]
    (get-in (databases domain) [dbname :database/write-security :db/ident] ::allow-anonymous-edits)))

(defmethod process-tx ::allow-anonymous-edits [$ domain dbname subject tx] tx)

(defmulti tx-meta
  (fn [schema tx]
    (if (map? tx)
      ::map
      (first tx))))

(s/def ::tx-cardinality (s/or :one :many))
(s/def ::tx-identifier map?)
(s/def ::tx-inverse fn?)
(s/def ::tx-special fn?)

(s/def ::transaction-meta
  (s/keys :req [::tx-identifier]
          :opt [::tx-cardinality ::tx-inverse ::tx-special ::tx-conflicting?]))

(s/fdef tx-meta
  :ret ::transaction-meta)


; clj only
(def ^:dynamic *$* nil)
(def ^:dynamic *get-db* nil)
(def ^:dynamic *domain* nil)
(def ^:dynamic *subject*)                              ; FK into $hyperfiddle-users, e.g. #uuid "b7a4780c-8106-4219-ac63-8f8df5ea11e3"
(def ^:dynamic *route* nil)

; cljs!
(defmulti stmt-id->tempid "Deep introspection of args to transaction fns in order to reverse tempids"
  (fn [id->tempid schema [op e a v :as stmt]]
    (case op                                                ; backwards compat with old Datomic
      :db.fn/cas :db/cas
      :db.fn/retractEntity :db/retractEntity
      op)))

; #?(:cljs)
(defn domain-security
  ([ctx] (get-in (db-record ctx) [:database/write-security :db/ident] ::allow-anonymous-edits))
  ([hf-db subject] (get-in hf-db [:database/write-security :db/ident] ::allow-anonymous-edits)))

(defmulti subject-may-transact+ "returns (left tooltip-msg) or (right)" (fn [hf-db subject] (domain-security hf-db subject))) ; todo pass ctx
(defmulti subject-may-create? domain-security)
(defmulti subject-may-edit-entity? "Attribute whitelist is not implemented here, this is about entity level writes" domain-security)
(defmulti subject-may-edit-attr? domain-security)

(defmethod subject-may-transact+ ::allow-anonymous-edits [hf-db subject] (right)) ; :hyperfiddle.security/allow-anonymous
(defmethod subject-may-create? ::allow-anonymous-edits [hf-db subject ctx] true)
(defmethod subject-may-edit-entity? ::allow-anonymous-edits [hf-db subject ctx] true)
(defmethod subject-may-edit-attr? ::allow-anonymous-edits [ctx] true) ; no tx constraints by default

(declare render-dispatch)

;; Dispatch is a set
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
    (let [?parent-a (some-> (:hypercrud.browser/parent ctx) (hyperfiddle.api/a))
          is-component (boolean (if ?parent-a (hyperfiddle.api/attr? ctx ?parent-a :db/isComponent)))]
      (if (and (not is-component) (hyperfiddle.api/identity? ctx))
        #{:db.unique/identity}))
    (if-let [attr (hyperfiddle.api/attr ctx)]
      (extract-set attr :db/valueType :db/cardinality))
    (if (hyperfiddle.api/element ctx)
      (extract-set ctx hyperfiddle.api/element-type))))       ; :hf/variable, :hf/aggregate, :hf/pull
    ;(contrib.datomic/parser-type (context/qfind ctx))       ; :hf/find-rel :hf/find-scalar
    ;:hf/blank


(defmulti render-fiddle (fn [_val ctx _props] (fiddle ctx)))

(defmulti formula (fn [_ctx link _value] (:link/formula link)))

(defn route
  "Get route, with default values. Rember default route values get overwritten by
  user-provided values"
  [ctx]
  @(:hypercrud.browser/route-defaults ctx))

;; multiple params can depend on each other so set defaults centrally per fiddle
(defmulti defaults first)

(defmulti view-defaults first)

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

; All hyperfiddle specs should be here in this namespace.
; Namespaces other than "hyperfiddle" are henceforth forbidden and all legacy namespaces should be migrated.
; If you feel the need to organize attributes that "traverse together" use a comment in this file
; or (future) use spec2 schema & select.

(s/def ::invalid-messages (s/coll-of string?))
(s/def ::is-invalid boolean?)
(s/def ::view-change! fn?)

(s/def :html/placeholder string?)

(s/def :hf/where any?)
(s/def :hf/where-spec any?)

(defn arg* [e]
  (cond
    (thinentity? e) (.-id e)
    (number? e) e                                        ; dbid
    (string? e) e                                        ; tempid
    (some? e) (throw (ex-info "unrecognized route param type" {:e e}))
    () nil ; don't crash if missing entirely
    ))

(defn arg "Silly extractor for a HF deftype with poor ergonomics. Todo cleanup.
  Used by Rosie"
  ;([] (hf-arg hf/*route*))
  ([hf-route] (arg hf-route 0))
  ([hf-route ix]
   (arg* (get (:hyperfiddle.route/datomic-args hf-route) ix))))

(defn with-hf-args [f] (f *$* (arg *route*)))

(defn ^:temporary ->either-domain                           ; todo remove
  "Wrap a domain `x` as `Right x`. Useful to make existing (either-branched) code
  compatible with unested, reshaped domain values."
  [x]
  (cond
    ;; identity
    (either/either? x)    x
    ;; pure
    (satisfies? Domain x) (either/right x)
    :else                 (either/left (ex-info "Not a domain"
                                                {:value x
                                                 :type  (type x)}))))

(def root-pid "root")
(def browser-query-limit 20)

(defn needle-match [v needle]
  (str/includes?
   (.toLowerCase (or v ""))
   (.toLowerCase (or needle ""))))

(defmacro serve!
  "For demo purposes, avoid complex :require in demo nss so people can focus on
  other aspects. What's hf-def? -> they shouldn't care."
  []
  `(hyperfiddle.def/serve-ns! ~*ns*))

(defn ^::fiddle index [])
