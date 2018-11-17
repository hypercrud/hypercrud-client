(ns hypercrud.browser.routing
  (:require
    [cats.core :as cats :refer [mlet return]]
    [cats.monad.either :as either :refer [left right]]
    [clojure.set :as set]
    [clojure.walk :as walk]
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [xorxs]]
    [contrib.eval :as eval]
    [contrib.reactive :as r]
    [contrib.reader :as reader]
    [contrib.string :refer [blank->nil]]
    [contrib.try$ :refer [try-either]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.q-util :as q-util]
    [hypercrud.types.ThinEntity :refer [->ThinEntity #?(:cljs ThinEntity)]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.route :as route]
    [hyperfiddle.tempid :refer [smart-entity-identifier tempid?]]
    [taoensso.timbre :as timbre])
  #?(:clj
     (:import (hypercrud.types.ThinEntity ThinEntity))))


(defn invert-route [domain [_ args :as route] invert-id]
  (let [args (->> {:request-params args}                    ; code compat
                  (walk/postwalk (fn [v]                    ; works on [args] instead of (:request-param args) ?
                                   (if (instance? ThinEntity v)
                                     (let [uri (domain/dbname->uri (.-dbname v) domain)
                                           id (invert-id (.-id v) uri)]
                                       (->ThinEntity (.-dbname v) id))
                                     v)))
                  :request-params)]
    (apply route/canonicalize (assoc route 1 args))))

(defn id->tempid+ [route ctx]
  (let [invert-id (fn [id uri]
                    (if (tempid? id)
                      id
                      (let [id->tempid (context/ctx->id-lookup uri ctx)]
                        (get id->tempid id id))))]
    (try-either (invert-route (:hypercrud.browser/domain ctx) route invert-id))))

(defn tempid->id+ [route ctx]
  (let [invert-id (fn [temp-id uri]
                    (if (tempid? temp-id)
                      (let [tempid->id (-> (context/ctx->id-lookup uri ctx)
                                           (set/map-invert))]
                        (get tempid->id temp-id temp-id))
                      temp-id))]
    (try-either (invert-route (:hypercrud.browser/domain ctx) route invert-id))))

(defn route+ [ctx [_ params :as route]]                     ; circular, this can be done sooner
  (mlet [_ (if (or (nil? params) (vector? params))          ; validate normalized already
             (either/right nil)
             (either/left (ex-info "Route not normalized, params must be nil or vector" {:params params})))
         ; route should be a ref, provided by the caller, that we fmap over
         ; because it is not, this is obviously fragile and will break on any change to the route
         ; this is acceptable today (Sep-2018) because changing a route in ANY way assumes the entire iframe will be re-rendered
         reactive-route @(r/apply-inner-r (r/track tempid->id+ route ctx))]
    (cats/return (assoc ctx :hypercrud.browser/route reactive-route))))

(defn validated-route+ [?fiddle ?route ctx]                 ; can validate with link, not just fiddle
  ; We specifically hydrate this deep just so we can validate anchors like this.
  (let [[_ [$1 :as params]] ?route]
    (case (:fiddle/type ?fiddle)
      :query (let [q (unwrap                                ; todo whats the point of this monad?
                       #(timbre/warn %)
                       (mlet [q (reader/memoized-read-string+ (:fiddle/query ?fiddle))]
                         (if (vector? q)
                           (cats/return q)
                           (either/left {:message (str "Invalid query '" (pr-str q) "', only vectors supported")}))))]
               (mlet [q-params (q-util/validate-query-params+ q params ctx)]
                 ; Ignore the params, just care that they validated.
                 (right ?route)))
      :entity (if (not= nil $1)                             ; todo check conn
                (right ?route)
                (left {:message "malformed entity param" :data {:params params}}))
      ; nil means :blank
      (either/right ?route))))

(defn normalize-args [porps]
  ; There is some weird shit hitting this assert, like {:db/id nil}
  {:pre [#_(not (map? porps)) #_"legacy"]
   :post [(vector? %) #_"route args are associative by position"]}
  (vec (xorxs porps)))

(defn pull->colored-eid "Adapt pull to a datomic primitive suitable for :in" [ctx v]
  (if-not (context/has-entity-identity? ctx)                ; maybe a ref but you didn't pull an identity? Can't do much for that (we can't generate links there and neither should userland try)
    v                                                       ; In edge cases, this could be a colorless opaque value (composite or scalar)
    (->ThinEntity (context/dbname ctx) (smart-entity-identifier ctx v))))

(let [eval-string!+ (memoize eval/eval-expr-str!+)]
  (defn build-args+
    "formulas have to run as part of link refocusing so formula tempids can occlude the eschewed pulled-tree data"
    [ctx {:keys [:link/fiddle :link/tx-fn] :as link}]
    (mlet [f-wrap (if-let [formula-str (blank->nil (:link/formula link))]
                    (eval-string!+ (str "(fn [ctx] \n" formula-str "\n)"))
                    (either/right (constantly (constantly nil))))
           f (try-either (f-wrap ctx))
           colored-args (try-either @(r/fmap->> (or (:hypercrud.browser/data ctx) (r/track identity nil))
                                                (pull->colored-eid ctx)
                                                f))]
      (return (normalize-args colored-args)))))

(defn ^:export build-route' "There may not be a route! Fiddle is sometimes optional"
  [+args ctx {:keys [:link/fiddle :link/tx-fn] :as link}]
  (if (and (not fiddle) tx-fn)
    (mlet [colored-args +args] (return nil))               ; :hf/remove doesn't have one by default, :hf/new does, both can be customized
    (mlet [colored-args +args                              ; part of error chain
           fiddle-id (if fiddle
                       (right (:fiddle/ident fiddle))
                       (left {:message ":link/fiddle required" :data {:link link}}))
           route (id->tempid+ (route/canonicalize fiddle-id colored-args) ctx)
           route (route/validate-route+ route)]
      (return route))))
