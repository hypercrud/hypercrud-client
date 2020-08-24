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
    [hypercrud.browser.context :as context]
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
       (hypercrud.types.ThinEntity ThinEntity))))

(declare eval-fiddle+)
(declare resolve-fiddle+)
(declare nil-or-hydrate+)

(defn explode-result
  "Extract a fiddle response into [route-defaults result]"
  [r-response]
  [(from-result (r/fmap first r-response)) ; completed route
   (from-result (r/fmap second r-response))
   (from-result (r/fmap last r-response))])


; internal bs abstraction to support hydrate-result-as-fiddle
(defn- internal-browse-route+ [{rt :runtime pid :partition-id :as ctx} route]
  {:pre [route]}
  (do-result

    (when-let [e (runtime/get-error rt pid)]
      (throw e))

    (let [; todo runtime should prevent invalid routes from being set
          route (from-result (route/invert-route route (partial runtime/tempid->id! rt pid)))
          ;_ (timbre/debug "route " route)
          r-fiddle (from-result @(r/apply-inner-r (r/track resolve-fiddle+ route ctx))) ; inline query
          ;_ (timbre/debug "fiddle" @r-fiddle)
          r-request (from-result @(r/apply-inner-r (r/fmap (fn [_] (either/right (list route pid))) r-fiddle)))
          ;_ (timbre/debug "request" @r-request)
          r-response (from-result @(r/apply-inner-r (r/fmap->> r-request (nil-or-hydrate+ rt pid))))
          ;_ (timbre/debug "result" @r-fiddle :-> @r-result)
          [r-route-defaults r-route-defaults-hydrated r-result] (explode-result r-response)]

      ;; FIXME this is a hack, a new runtime impl is needed
      #?(:clj (state/dispatch! rt [:partition-route-defaults pid @r-route-defaults-hydrated]))

      ; fiddle request can be nil for no-arg pulls (just draw readonly form)
      (context/result
        (from-result
          (-> ctx
              ; route should be a ref, provided by the caller, that we fmap over
              ; because it is not, this is obviously fragile and will break on any change to the route
              ; this is acceptable today (Jun-2019) because changing a route in ANY way assumes the entire iframe will be re-rendered
              (assoc :hypercrud.browser/route (r/pure (vec route))) ; vec for direct positional access
              (assoc :hypercrud.browser/route-defaults-hydrated r-route-defaults-hydrated)
              (assoc :hypercrud.browser/route-defaults-symbolic r-route-defaults)
              (context/fiddle+ r-fiddle)))
        r-result))))

(defn browse-partition+ [ctx]
  (let [route (runtime/get-route (:runtime ctx) (:partition-id ctx))]
    (assert route (str "route not found for pid: " (:partition-id ctx)))
    (internal-browse-route+ ctx route)))

(defn browse-result-as-fiddle+ [{rt :runtime pid :partition-id :as ctx}]
  (timbre/warn "legacy invocation; browse-route+ is deprecated")
  ; TODO remove, this is legacy from query fiddles
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
                                                 ]}
                                  :link/formula
                                  :link/path
                                  :link/tx-fn]}
                  :fiddle/markdown
                  :fiddle/renderer
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
                           (select-keys [:db/id :fiddle/spec])
                           (update-existing :fiddle/eval str)))))

          () val)

    (cond-> val
      ((-> key name keyword) #{:markdown :eval}) str)))
