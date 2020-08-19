(ns hypercrud.browser.base
  #?(:cljs
     (:require-macros
       [contrib.do :refer [do-result]]))
  (:require
    #?(:clj [backtick :refer [template]])
    [cats.core :as cats :refer [mlet return]]
    [cats.monad.either :as either]
    [contrib.data :refer [update-existing for-kv]]
    [contrib.do :as do :refer [do-result from-result]]
    [clojure.string :as string]
    [contrib.datomic.common.query :as query]
    [contrib.reactive :as r]
    [contrib.reader :as reader :refer [memoized-read-edn-string+]]
    [contrib.try$ :refer [try-either]]
    [hypercrud.browser.context :as context]
    [hypercrud.types.DbName :refer [#?(:cljs DbName)]]
    [hypercrud.types.EntityRequest :refer [->EntityRequest]]
    [hypercrud.types.QueryRequest :refer [->QueryRequest ->EvalRequest]]
    [hypercrud.types.ThinEntity :refer [#?(:cljs ThinEntity)]]
    [hyperfiddle.api :as hf]
    [hyperfiddle.fiddle :as fiddle]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.state :as state]
    [taoensso.timbre :as timbre]
    [contrib.expr :as expr]
    [hyperfiddle.def :as hf-def])
  #?(:clj
     (:import
       (hypercrud.types.DbName DbName)
       (hypercrud.types.ThinEntity ThinEntity))))

(declare eval-fiddle+)
(declare resolve-fiddle+)
(declare request-for-fiddle+)
(declare nil-or-hydrate+)

(defn explode-result
  "Extract a fiddle response into [route-defaults result]"
  [r-response]
  [(from-result (r/fmap first r-response)) ; completed route
   (from-result (r/fmap second r-response))])


; internal bs abstraction to support hydrate-result-as-fiddle
(defn- internal-browse-route+ [{rt :runtime pid :partition-id :as ctx} route]
  {:pre [route]}
  (do-result

    (when-let [e (runtime/get-error rt pid)]
      #_(throw e)                                           ; hypercrud.types.Err is not Throwable
      (throw (ex-info (pr-str e) {} (ex-cause e))))

    (let [; todo runtime should prevent invalid routes from being set
          route (from-result (route/invert-route route (partial runtime/tempid->id! rt pid)))
          ;_ (timbre/debug "route " route)
          r-fiddle (from-result @(r/apply-inner-r (r/track resolve-fiddle+ route ctx))) ; inline query
          ;_ (timbre/debug "fiddle" @r-fiddle)
          r-request (from-result @(r/apply-inner-r (r/fmap->> r-fiddle (request-for-fiddle+ rt pid route))))
          ;_ (timbre/debug "request" @r-request)
          r-response (from-result @(r/apply-inner-r (r/fmap->> r-request (nil-or-hydrate+ rt pid))))
          ;_ (timbre/debug "result" @r-fiddle :-> @r-result)
          [r-route-defaults r-result] (explode-result r-response)]

      ;; FIXME this is a hack, a new runtime impl is needed
      #?(:clj (state/dispatch! rt [:partition-route-defaults pid @r-route-defaults]))

      ; fiddle request can be nil for no-arg pulls (just draw readonly form)
      (context/result
        (from-result
          (-> ctx
              ; route should be a ref, provided by the caller, that we fmap over
              ; because it is not, this is obviously fragile and will break on any change to the route
              ; this is acceptable today (Jun-2019) because changing a route in ANY way assumes the entire iframe will be re-rendered
              (assoc :hypercrud.browser/route (r/pure (vec route))) ; vec for direct positional access
              (assoc :hypercrud.browser/route-defaults r-route-defaults)
              (context/fiddle+ r-fiddle)))
        r-result))))

(defn browse-partition+ [ctx]
  (let [route (runtime/get-route (:runtime ctx) (:partition-id ctx))]
    (assert route (str "route not found for pid: " (:partition-id ctx)))
    (internal-browse-route+ ctx route)))

(defn browse-result-as-fiddle+ [{rt :runtime pid :partition-id :as ctx}]
  (timbre/warn "legacy invocation; browse-route+ is deprecated")
  ; This only makes sense on :fiddle/type :query because it has arbitrary arguments
  ; EntityRequest args are too structured.
  (let [[inner-fiddle & inner-args] (::route/datomic-args (runtime/get-route rt pid))
        route (cond-> {:hyperfiddle.route/fiddle inner-fiddle}
                (seq inner-args) (assoc :hyperfiddle.route/datomic-args (vec inner-args)))]
    (internal-browse-route+ ctx route)))

(defn legacy-fiddle-ident->lookup-ref [fiddle]
  ; SHould be an ident but sometimes is a long today
  ; Keywords are not a db/ident, turn it into the fiddle-id lookup ref.
  ; Otherwise, pass it through, its already a lookup-ref or eid or whatever.
  (cond
    (keyword? fiddle) [:fiddle/ident fiddle]
    (uuid? fiddle) [:fiddle/uuid fiddle]
    :else fiddle))

(defn legacy-lookup-ref->fiddle-ident [lookup-ref]
  (if (coll? lookup-ref)
    (case (first lookup-ref)
      :fiddle/ident (second lookup-ref)
      :fiddle/uuid (second lookup-ref)
      lookup-ref)
    lookup-ref))

(declare eval-attr)
(declare interp-attr)
(declare normalize)

(defrecord Eval [fiddle scope]
  do/Do-via
  (resolver-for [h]
    {:Eval.fiddle
     (fn [_] (:fiddle do/*this))
     :Eval.scope
     (fn [_] (:scope do/*this))
     :Eval.get-var
     (fn [[_ name]] (get (:scope do/*this) name))
     :Eval.set-var!
     (fn [[_ name val]]
       (set! do/*this (assoc-in do/*this [:scope name] val)))}))

(defn eval-fiddle+ [fiddle {:keys [domain route ctx] :as env}]
  (do/scope (into [`eval-fiddle+ (:fiddle/ident fiddle)] (seq (:fiddle/apply fiddle)))
    (->
      (do-result
        (let [fiddle (fiddle/apply-defaults fiddle)]

          (do/via*
            (->Eval
              fiddle
              (merge
                {:eval/env  env
                 :eval/mode (fiddle/kind fiddle)
                 :route     route}
                (let [[_ system-fiddle db] (re-find #"([^\$]*)(\$.*)" (name (:fiddle/ident fiddle)))]
                  (when system-fiddle
                    {'domain       domain
                     'db           db}))))

            (doseq [[name x]
                    (merge
                      (zipmap (:fiddle/args fiddle)
                              (concat (:fiddle/apply fiddle) (repeat nil)))
                      (:fiddle/with fiddle))]
              (do/! :Eval.set-var! name (eval-attr x)))

            (let [fiddle
                  (-> fiddle
                      (dissoc :fiddle/args :fiddle/with :fiddle/apply)
                      (assoc :fiddle/ident
                             (or (do/! :Eval.get-var :ident) (:fiddle/ident fiddle))))]

              (for-kv fiddle {}
                      (fn [fiddle' attr val]
                        (assoc fiddle' attr
                               ;; We only have :eval (:static) fiddles for now, no
                               ;; need to interpret attributes. But this might come
                               ;; back. interp-attr is a costy function ATM.
                               #_(normalize attr (interp-attr val))
                               (normalize attr val))))))))

      (either/branch-left
        (fn [e]
          (timbre/error e)
          (either/left e))))))

(defn- resolve-route [route]
  (let [fiddle (::route/fiddle route)]
    (if-let [[_ fiddle-name db] (re-find #"([^\$]*)(\$.*)" (name fiddle))]
      (assoc route
             ::route/fiddle (keyword (namespace fiddle) (str fiddle-name "$"))
             ::route/db db)
      route)))

(def fiddle-pull [:db/id
                  :fiddle/ident
                  {:fiddle/links [:db/id
                                  :link/class
                                  {:link/fiddle [:db/id
                                                 :fiddle/ident       ; routing
                                                 :fiddle/query       ; validation
                                                 :fiddle/type        ; validation
                                                 #_:fiddle/eval      ; todo validation ?
                                                 ]}
                                  :link/formula
                                  :link/path
                                  :link/tx-fn]}
                  :fiddle/markdown
                  :fiddle/pull
                  :fiddle/pull-database
                  :fiddle/query
                  :fiddle/renderer
                  :fiddle/type
                  #_*                                                ; For hyperblog, so we can access :hyperblog.post/title etc from the fiddle renderer
                  ])

(defn- resolve-fiddle+ [[f & _ :as route] ctx]
  (do-result
   (let [record (get-in @hf-def/*defs [:fiddle f])]

     (when-not (or (:db/id record) (:fiddle/source record))
       (throw (ex-info (str :hyperfiddle.error/fiddle-not-found)
                       {:ident        :hyperfiddle.error/fiddle-not-found
                        :fiddle/ident f
                        :fiddle       record
                        :error-msg    (str "Fiddle not found (" f ")")
                        :human-hint   "Did you forget to serve-ns?"})))

     (eval-fiddle+ (fiddle/apply-defaults record)
                   {:ctx    ctx
                    :route  route
                    :domain (hf/domain (:runtime ctx))}))))

; This factoring is legacy, can the whole thing can be inlined right above the datomic IO?
; UI runs it as validation for tooltip warnings (does the query match the params)
(defn request-for-fiddle+ [rt pid route fiddle]             ; no ctx
  (case (:fiddle/type fiddle)
    ; Needles, which should really be query params. Query params can be resolved client-side with code splicing?
    ; How does this impact routing? Server rendering?

    ; Schema editor - many params
    ; Sub requests - peer function, no params other than entity

    ; Can we just do sub requests first, no need for splicing?

    :query (mlet [q (cond (string? (:fiddle/query fiddle)) (reader/memoized-read-string+ (:fiddle/query fiddle))
                      () (either/right (:fiddle/query fiddle))) ; todo why read-string instead of read-edn-string?
                  needle-clauses (either/right nil) #_(reader/memoized-read-edn-string+ (:fiddle/query-needle fiddle))
                  :let [[inputs appended-$] (if-let [in (:in (query/q->map q))]
                                              [(mapv str in) false]
                                              [["$"] true])
                        [query-args unused appended-needle]
                        (loop [query-args []
                               [route-arg & next-route-args :as route-args] (::route/datomic-args route)
                               [hole & next-holes] inputs
                               appended-needle false]
                          (let [[query-arg next-route-args] (cond
                                                              (string/starts-with? hole "$")
                                                              (if false #_(instance? DbName route-arg)
                                                                [(runtime/db rt pid (:dbname route-arg)) next-route-args]
                                                                [(runtime/db rt pid hole) (seq route-args)])

                                                              (instance? ThinEntity route-arg)
                                                              ; I think it already has the correct identity and tempid is already accounted
                                                              #_(context/smart-entity-identifier ctx route-arg)
                                                              [(.-id route-arg) next-route-args] ; throw away dbname

                                                              :else [route-arg next-route-args])
                                query-args (conj query-args query-arg)]
                            (cond
                              next-holes (recur query-args next-route-args next-holes appended-needle)
                              (and next-route-args (some? needle-clauses)) (recur query-args next-route-args ["?hf-needle"] true)
                              :else [query-args next-route-args appended-needle])))]]
             (cond
               #_#_(seq unused) (either/left (ex-info "unused param" {:query q :params query-args :unused unused}))

               (not= (count query-args) (if appended-needle
                                          (+ 1 (count inputs))
                                          (count inputs)))
               (either/left (ex-info "missing params" {:query q :params query-args :unused unused}))

               :else (let [q (cond-> q
                               appended-needle (-> ((partial apply query/append-where-clauses) needle-clauses)
                                                   (as-> q (if appended-$
                                                             (query/append-inputs q '$ '?hf-needle)
                                                             (query/append-inputs q '?hf-needle))))

                               (seq (::route/where route)) ((partial apply query/append-where-clauses) (::route/where route)))]
                       (return (->QueryRequest q query-args {:limit hf/browser-query-limit})))))

    :eval                                                   ; '(datomic.api/pull $ [:db/ident '*] :db/ident)
    (let [#_#_form (-> #_(memoized-read-edn-string+ (:fiddle/eval fiddle))
                 (reader/memoized-read-string+ (:fiddle/eval fiddle))
                 (either/branch (constantly nil) identity))]
      ; todo, this path needs abstraction assistance for paging/offset
      ;(timbre/debug :eval "fiddle" (:fiddle/eval fiddle))
      ;(timbre/debug :eval "form" form)
      (either/right (->EvalRequest (:fiddle/ident fiddle)
                                   pid                      ; dbval is reconstructed from the pid on the backend
                                   ; ::route/where ::route/datomic-args
                                   route)))                 ; Other request types are able to abstract over the route; but the eval path needs full control

    :entity
    (let [args (::route/datomic-args route)                 ; Missing entity param is valid state now https://github.com/hyperfiddle/hyperfiddle/issues/268
          [dbname ?e] (if false #_(instance? DbName (first args))
                        [(:dbname (first args)) (second args)]
                        [nil (first args)])]
      (if-let [dbname (or dbname (:fiddle/pull-database fiddle))]
        (let [db (runtime/db rt pid dbname)
              pull-exp (or (-> (cond (string? (:fiddle/pull fiddle)) (reader/memoized-read-string+ (:fiddle/pull fiddle))
                                     () (either/right (:fiddle/pull fiddle)))
                               ; todo it SHOULD be assertable that fiddle/pull is valid edn (and datomic-pull) by now (but its not yet)
                               (either/branch (constantly nil) identity))
                           ; todo this default is garbage, fiddle/pull should never be nil (defaults already applied)
                           ; and invalid edn should just fail, not nil-pun
                           ['*])]
          (either/right (->EntityRequest (or (:db/id ?e) ?e) db pull-exp)))
        (either/left (ex-info "Missing :fiddle/pull-database" {:fiddle (:fiddle/ident fiddle)}))))))

(defn- nil-or-hydrate+ [rt pid request]
  (if request
    @(hf/request rt pid request)
    (either/right nil)))

(defn eval-error [x msg]
  (throw (ex-info msg
                  {:fiddle ((do/! :Eval.fiddle))
                   :scope  (do/! :Eval.scope)
                   :x      x})))

(defn get-fiddle-def [fiddle #_{rt :runtime pid :partition-id :as ctx}]
  (when (find-ns 'hyperfiddle.def)
    (@(resolve 'hyperfiddle.def/get-fiddle) fiddle)))

(defn eval-attr [x]
  (->
    (do-result
      (cond-> (interp-attr x)
        (= (do/! :Eval.get-var :eval/mode) :eval) eval))
    (either/branch-left
      (fn [e]
        (timbre/error e "error in fiddle eval")
        (either/left e)))
    from-result))

(defn interp-attr [x]
  (let [eval-mode (= = (do/! :Eval.get-var :eval/mode) :eval :eval)]
    (expr/unquote-via x
     (fn eval-expr [x]
       (cond (qualified-keyword? x) {:fiddle/ident (str x)}

             (expr/form? x)
             (cond (qualified-keyword? (first x))
                   {:fiddle/ident (first x)
                    :fiddle/apply (vec (rest x))}
                   () (if eval-mode x (list 'quote (list 'unquote x))))

             (not eval-mode)
             (list 'quote (list 'unquote x))

             (symbol? x)
             (do/! :Eval.get-var x x)

             (simple-keyword? x)
             (or (if (contains? (do/! :Eval.scope) x) (do/! :Eval.get-var x))
                 (eval-error x (str "var " x " not defined in" (:fiddle/ident (do/! :Eval.fiddle)))))

             () (eval-error x (str "unknown form" x "in" (:fiddle/ident (do/! :Eval.fiddle)))))))))

(defn normalize [key val]
  (case key
    :fiddle/links
    (mapv (fn [link]
            (reduce-kv
              (fn [kv k v]
                (assoc kv k (normalize k v)))
              {} link))
          val)

    :link/fiddle
    (cond (expr/form? val)
          val

          (map? val)
          (do
            (do/! :Eval.set-var! :eval/refs (concat (do/! :Eval.get-var :eval/refs) [(:fiddle/ident val)]))
            (cond (and (:fiddle/apply val)
                       (= (:eval/mode val) :eval))
                  (from-result
                    (eval-fiddle+
                      (merge (get-fiddle-def (:fiddle/ident val))
                        (select-keys val [:fiddle/apply]))
                      (do/! :Eval.get-var :eval/env)))
                  () (merge val
                       (-> (get-fiddle-def (:fiddle/ident val))
                           (select-keys [:db/id :fiddle/type :fiddle/query :fiddle/pull :fiddle/spec])
                           (update-existing :fiddle/query str)
                           (update-existing :fiddle/pull str)
                           (update-existing :fiddle/eval str)))))

          () val)

    (cond-> val
      ((-> key name keyword) #{:renderer :markdown :query :pull :eval}) str)))
