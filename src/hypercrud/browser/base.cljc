(ns hypercrud.browser.base
  #?(:cljs
     (:require-macros
      [contrib.do :refer [do-result]]))
  (:require
   [cats.monad.either :as either]
   [contrib.do :as do :refer [do-result from-result]]
   [contrib.reactive :as r]
   [hypercrud.browser.context :as context]
   [hyperfiddle.api :as hf]
   [hyperfiddle.def :as hf-def]
   [hyperfiddle.fiddle :as fiddle]
   [hyperfiddle.route :as route]
   [hyperfiddle.runtime :as runtime]
   [hyperfiddle.state :as state]))

(defn- nil-or-hydrate+ [rt pid request]
  (if request
    @(hf/request rt pid request)
    (either/right nil)))

(defn- resolve-fiddle+ [[f & _]]
  (do-result
   (or (some-> f hf-def/get-fiddle hf-def/parse fiddle/apply-defaults)
       (throw (ex-info (str :hyperfiddle.error/fiddle-not-found)
                       {:ident        :hyperfiddle.error/fiddle-not-found
                        :fiddle/ident f
                        :error-msg    (str "Fiddle not found (" f ")")
                        :human-hint   "Did you forget to serve-ns?"})))))

(defn explode-result
  "Extract a fiddle response into [route-defaults result]"
  [r-response]
  [(from-result (r/fmap first r-response)) ; completed route
   (from-result (r/fmap second r-response))
   (from-result (r/fmap last r-response))])

(defn- internal-browse-route+ [{rt :runtime pid :partition-id :as ctx} route]
  {:pre [route]}
  (do-result

   (if-let [e (runtime/get-error rt pid)]
     (throw e)

     (let [; todo runtime should prevent invalid routes from being set
           route      (from-result (route/invert-route route (partial runtime/tempid->id! rt pid)))
           ;;_ (timbre/debug "route " route)
           r-fiddle   (from-result @(r/apply-inner-r (r/track resolve-fiddle+ route))) ; inline query
           ;;_ (timbre/debug "fiddle" @r-fiddle)
           r-request  (from-result @(r/apply-inner-r (r/fmap (fn [_] (either/right `(~route ~pid))) r-fiddle)))
           ;;_ (timbre/debug "request" @r-request)
           r-response (from-result @(r/apply-inner-r (r/fmap->> r-request (nil-or-hydrate+ rt pid))))
           ;;_ (timbre/debug "result" @r-fiddle :-> @r-result)
           [r-route-defaults r-route-defaults-hydrated r-result]
           (explode-result r-response)]

       ;; FIXME this is a hack, a new runtime impl is needed
       #?(:clj (state/dispatch! rt [:partition-route-defaults pid @r-route-defaults-hydrated]))

       ;; fiddle request can be nil for no-arg pulls (just draw readonly form)
       (context/result
        (from-result
         (-> ctx
             ;; route should be a ref, provided by the caller, that we fmap over
             ;; because it is not, this is obviously fragile and will break on any change to the route
             ;; this is acceptable today (Jun-2019) because changing a route in ANY way assumes the entire iframe will be re-rendered
             (assoc :hypercrud.browser/route                   (r/pure (vec route)) ; vec for direct positional access
                    :hypercrud.browser/route-defaults-hydrated r-route-defaults-hydrated
                    :hypercrud.browser/route-defaults-symbolic r-route-defaults)
             (context/fiddle+ r-fiddle)))
        r-result)))))

(defn browse-partition+ [ctx]
  (let [route (runtime/get-route (:runtime ctx) (:partition-id ctx))]
    (assert route (str "route not found for pid: " (:partition-id ctx)))
    (internal-browse-route+ ctx route)))
