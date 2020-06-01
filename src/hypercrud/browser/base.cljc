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
    [taoensso.timbre :as timbre]
    [contrib.expr :as expr])
  #?(:clj
     (:import
       (hypercrud.types.DbName DbName)
       (hypercrud.types.ThinEntity ThinEntity))))

(declare eval-fiddle+)
(declare resolve-fiddle+)
(declare request-for-fiddle+)
(declare nil-or-hydrate+)

(def browser-query-limit 50)

; internal bs abstraction to support hydrate-result-as-fiddle
(defn- internal-browse-route+ [{rt :runtime pid :partition-id :as ctx} route]
  (do-result

    (when-let [e (runtime/get-error rt pid)]
      #_(throw e)                                           ; hypercrud.types.Err is not Throwable
      (throw (ex-info (pr-str e) {})))

    (let [; todo runtime should prevent invalid routes from being set
          route (from-result (route/validate-route+ route)) ; terminate immediately on a bad route
          ;_ (timbre/debug "route " route)
          route (from-result (route/invert-route route (partial runtime/tempid->id! rt pid)))
          ;_ (timbre/debug "route " route)
          r-fiddle (from-result @(r/apply-inner-r (r/track resolve-fiddle+ rt pid route ctx))) ; inline query
          ;_ (timbre/debug "fiddle" @r-fiddle)
          r-request (from-result @(r/apply-inner-r (r/fmap->> r-fiddle (request-for-fiddle+ rt pid route))))
          ;_ (timbre/debug "request" @r-request)
          r-result (from-result @(r/apply-inner-r (r/fmap->> r-request (nil-or-hydrate+ rt pid))))
          ;_ (timbre/debug "result" @r-fiddle :-> @r-result)
          ]

      ; fiddle request can be nil for no-arg pulls (just draw readonly form)
      (context/result
        (from-result
          (-> ctx
              ; route should be a ref, provided by the caller, that we fmap over
              ; because it is not, this is obviously fragile and will break on any change to the route
              ; this is acceptable today (Jun-2019) because changing a route in ANY way assumes the entire iframe will be re-rendered
              (assoc :hypercrud.browser/route (r/pure route))
              (context/fiddle+ r-fiddle)))
        r-result))))

(defn browse-partition+ [ctx]
  (internal-browse-route+ ctx (runtime/get-route (:runtime ctx) (:partition-id ctx))))

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

;(defrecord Eval [scope])

(defn eval-fiddle+ [fiddle {:keys [domain route ctx] :as env}]
  (do/scope (into [`eval-fiddle+ (:fiddle/ident fiddle)] (seq (:fiddle/apply fiddle)))
    (->
      (do-result
        (let [fiddle (fiddle/apply-defaults fiddle)]

          (do/via*
            {:fiddle fiddle
             :scope  (merge
                       {:eval/env  env
                        :eval/mode (fiddle/kind fiddle)
                        :route     route}
                       (let [[_ system-fiddle db] (re-find #"([^\$]*)(\$.*)" (name (:fiddle/ident fiddle)))]
                         (when system-fiddle
                           {'domain       domain
                            'db           db
                            'user-db->ide (:hyperfiddle.ide.domain/user-dbname->ide domain)})))}
            {:Eval.fiddle
             (fn [_] (:fiddle do/*state))
             :Eval.scope
             (fn [_] (:scope do/*state))
             :Eval.get-var
             (fn [[_ name]] (get (:scope do/*state) name))
             :Eval.set-var!
             (fn [[_ name val]]
               (set! do/*state (update do/*state assoc name val)))}

            (doseq [[name x]
                    (merge
                      (zipmap (:fiddle/args fiddle)
                              (concat (:fiddle/apply fiddle) (repeat nil)))
                      (:fiddle/with fiddle))]
              (do/! :Eval.set-var! name (eval-attr x)))

            (case (do/! :Eval.get-var :eval/mode)

              :skip (merge {:fiddle/type :blank}
                      (select-keys fiddle [:fiddle/source]))

              (let [fiddle
                    (-> fiddle
                        (dissoc :fiddle/args :fiddle/with :fiddle/apply)
                        (assoc :fiddle/ident
                          (or (do/! :Eval.get-var :ident) (:fiddle/ident fiddle))))]

                (for-kv fiddle {}
                  (fn [fiddle' attr val]
                    (assoc fiddle' attr
                      (normalize attr (interp-attr val))))))))))

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

(defn- resolve-fiddle+ [rt pid route ctx]
  (do-result

    (let [route (resolve-route route)
          fiddle-ident (::route/fiddle route)
          db (runtime/db rt pid (hf/fiddle-dbname (hf/domain rt)))
          request (->EntityRequest (legacy-fiddle-ident->lookup-ref fiddle-ident) db :default)
          record (from-result @(hf/request rt pid request))]

      (when-not (or (:db/id record) (:fiddle/source record))
        (throw (ex-info (str :hyperfiddle.error/fiddle-not-found)
                 {:ident        :hyperfiddle.error/fiddle-not-found
                  :fiddle/ident fiddle-ident
                  :fiddle       record
                  :error-msg    (str "Fiddle not found (" fiddle-ident ")")
                  :human-hint   "Did you just edit :fiddle/ident?"})))

      (eval-fiddle+ (fiddle/apply-defaults record)
        {:ctx    ctx
         :route  route
         :domain (hf/domain (:runtime ctx))})
      )))

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
                       (return (->QueryRequest q query-args {:limit browser-query-limit})))))

    :eval                                                   ; '(datomic.api/pull $ [:db/ident '*] :db/ident)
    (let [#_#_form (-> #_(memoized-read-edn-string+ (:fiddle/eval fiddle))
                 (reader/memoized-read-string+ (:fiddle/eval fiddle))
                 (either/branch (constantly nil) identity))]
      ; todo, this path needs abstraction assistance for paging/offset
      ;(timbre/debug :eval "fiddle" (:fiddle/eval fiddle))
      ;(timbre/debug :eval "form" form)
      (either/right (->EvalRequest (:fiddle/eval fiddle)
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
        (either/left (ex-info "Missing :fiddle/pull-database" {:fiddle (:fiddle/ident fiddle)}))))

    :blank (either/right nil)))

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

    (eval
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

                () (eval-error x (str "unknown form" x "in" (:fiddle/ident (do/! :Eval.fiddle))))
                ))))))

(defn normalize [key val]
  (case key
    :fiddle/links
    (mapv (fn [link]
            (-> (reduce-kv
                  (fn [kv k v] (assoc kv k (normalize k v)))
                  {} link)
                (assoc :db/id
                  ; hack for https://github.com/hyperfiddle/hyperfiddle/issues/1022
                  ; :db/id must be `long? if Datomic specs are in the spec registry
                  (-> (do/! :Eval.get-var :eval/env) :route :hyperfiddle.route/fiddle hash))))
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
                           (select-keys [:db/id :fiddle/type :fiddle/query :fiddle/pull])
                           (update-existing :fiddle/query str)
                           (update-existing :fiddle/pull str)
                           (update-existing :fiddle/eval str)))))

          () val)

    (cond-> val
      ((-> key name keyword) #{:code :cljs-ns :renderer :markdown :css
                               :query :pull :eval :formula :path}) str)))
