(ns hypercrud.browser.base
 #?(:cljs
    (:require-macros [backtick :refer [template]]))
  (:require
    #?(:clj [backtick :refer [template]])
    [cats.core :as cats :refer [mlet return]]
    [cats.monad.either :as either]
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
    [hyperfiddle.domain :as domain]
    [hyperfiddle.fiddle :as fiddle]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [taoensso.timbre :as timbre])
  #?(:clj
     (:import
       (hypercrud.types.DbName DbName)
       (hypercrud.types.ThinEntity ThinEntity))))


(defn legacy-fiddle-ident->lookup-ref [fiddle]              ; SHould be an ident but sometimes is a long today
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

(defn- hydrate-fiddle-from-datomic+ [rt pid fiddle-ident]   ; no ctx
  (mlet [:let [db (runtime/db rt pid (domain/fiddle-dbname (runtime/domain rt)))
               request (->EntityRequest (legacy-fiddle-ident->lookup-ref fiddle-ident) db fiddle/browser-pull)]
         record @(runtime/hydrate rt pid request)
         record (if-not (:db/id record)
                  (either/left (ex-info (str :hyperfiddle.error/fiddle-not-found)
                                        {:ident :hyperfiddle.error/fiddle-not-found
                                         :error-msg "Fiddle not found"
                                         :human-hint "Did you just edit :fiddle/ident?"}))
                  (either/right record))
         fiddle (case (:fiddle/type record :blank)          ; defaults have NOT yet been applied
                  :query (either/right record)  #_(mlet [query-needle (memoized-read-edn-string+ (:fiddle/query-needle record))]
                                                    (return record))
                  :entity (either/right record) #_(mlet [pull (memoized-read-edn-string+ (:fiddle/pull record))]
                                                    (return (assoc record :fiddle/pull pull)))
                  :eval (either/right record)
                  :blank (either/right record))]
    (return (fiddle/apply-defaults fiddle))))

(defn- hydrate-fiddle+ [rt pid fiddle-ident]                ; no ctx
  (-> (if (domain/system-fiddle? (runtime/domain rt) fiddle-ident)
        (->> (domain/hydrate-system-fiddle (runtime/domain rt) fiddle-ident)
             (cats/fmap fiddle/apply-defaults))
        (hydrate-fiddle-from-datomic+ rt pid fiddle-ident))))

(def browser-query-limit 50)

; This factoring is legacy, can the whole thing can be inlined right above the datomic IO?
; UI runs it as validation for tooltip warnings (does the query match the params)
(defn request-for-fiddle+ [rt pid route fiddle]             ; no ctx
  (case (:fiddle/type fiddle)
    ; Needles, which should really be query params. Query params can be resolved client-side with code splicing?
    ; How does this impact routing? Server rendering?

    ; Schema editor - many params
    ; Sub requests - peer function, no params other than entity

    ; Can we just do sub requests first, no need for splicing?

    :query (mlet [q (reader/memoized-read-string+ (:fiddle/query fiddle)) ; todo why read-string instead of read-edn-string?
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
    (let [form (-> #_(memoized-read-edn-string+ (:fiddle/eval fiddle))
                 (reader/memoized-read-string+ (:fiddle/eval fiddle))
                 (either/branch (constantly nil) identity))]
      ; todo, this path needs abstraction assistance for paging/offset
      (either/right (->EvalRequest form
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
              pull-exp (or (-> (memoized-read-edn-string+ (:fiddle/pull fiddle))
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
    @(runtime/request rt pid request)
    (either/right nil)))

; internal bs abstraction to support hydrate-result-as-fiddle
(defn- internal-browse-route+ [{rt :runtime pid :partition-id :as ctx} route]
  (mlet [_ (if-let [e (runtime/get-error rt pid)]
             (either/left e)
             (either/right nil))
         ; todo runtime should prevent invalid routes from being set
         route (route/validate-route+ route)                ; terminate immediately on a bad route
         ;:let [_ (println "route " route)]
         route (try-either (route/invert-route route (partial runtime/tempid->id! rt pid)))
         ;:let [_ (println "route " route)]
         r-fiddle @(r/apply-inner-r (r/track hydrate-fiddle+ rt pid (::route/fiddle route))) ; inline query
         ;:let [_ (println "fiddle " @r-fiddle)]
         r-request @(r/apply-inner-r (r/fmap->> r-fiddle (request-for-fiddle+ rt pid route)))
         ;:let [_ (println "request " @r-request)]
         r-result @(r/apply-inner-r (r/fmap->> r-request (nil-or-hydrate+ rt pid)))
         ;:let [_ (println "result " @r-result)]
         ctx (-> ctx
                 ; route should be a ref, provided by the caller, that we fmap over
                 ; because it is not, this is obviously fragile and will break on any change to the route
                 ; this is acceptable today (Jun-2019) because changing a route in ANY way assumes the entire iframe will be re-rendered
                 (assoc :hypercrud.browser/route (r/pure route))
                 (context/fiddle+ r-fiddle))
         :let [ctx (context/result ctx r-result)]]
    ; fiddle request can be nil for no-arg pulls (just draw readonly form)
    (return ctx)))

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
