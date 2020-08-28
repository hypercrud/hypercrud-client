(ns hypercrud.browser.base
  #?(:cljs
     (:require-macros
      [contrib.do :refer [do-result]]))
  (:require
   [contrib.do :as do :refer [do-result from-result]]
   [contrib.reactive :as r]
   [hypercrud.browser.context :as context]
   [hyperfiddle.api :as hf]
   [hyperfiddle.def :as hf-def]
   [hyperfiddle.fiddle :as fiddle]
   [hyperfiddle.route :as route]
   [hyperfiddle.runtime :as runtime]
   [hyperfiddle.state :as state]))

(defn- nil-or-hydrate [rt pid request]
  (when request
    (from-result (hf/request rt pid request))))

(defn- resolve-fiddle [[f & _]]
  (or (some-> f hf-def/get-fiddle hf-def/parse fiddle/apply-defaults)
      (throw (ex-info (str :hyperfiddle.error/fiddle-not-found)
                      {:ident        :hyperfiddle.error/fiddle-not-found
                       :fiddle/ident f
                       :error-msg    (str "Fiddle not found (" f ")")
                       :human-hint   "Did you forget to serve-ns?"}))))

; internal bs abstraction to support hydrate-result-as-fiddle
(defn- internal-browse-route+ [{rt :runtime pid :partition-id :as ctx} route]
  {:pre [route]}
  (do-result
   (if-let [e (runtime/get-error rt pid)]
     (throw e)
     (let [;; todo runtime should prevent invalid routes from being set
           route    (route/invert-route route (partial runtime/tempid->id! rt pid))
           ;;_ (taoensso.timbre/debug "route " route)
           fiddle   (resolve-fiddle route) ; inline query
           ;;_ (taoensso.timbre/debug "fiddle" @r-fiddle)
           request  `(~route ~pid)
           ;; _ (taoensso.timbre/debug "request" @r-request)
           response (from-result @(nil-or-hydrate rt pid request))

           ;; _ (taoensso.timbre/debug "result" @r-fiddle :-> @r-response)
           [route-defaults route-defaults-hydrated result] response]

       ;; FIXME this is a hack
       #?(:clj (state/dispatch! rt [:partition-route-defaults pid route-defaults-hydrated]))

       ;; fiddle request can be nil for no-arg pulls (just draw readonly form)
       (context/result
        (from-result
         (-> ctx
             ;; route should be a ref, provided by the caller, that we fmap over
             ;; because it is not, this is obviously fragile and will break on any change to the route
             ;; this is acceptable today (Jun-2019) because changing a route in ANY way assumes the entire iframe will be re-rendered
             (assoc :hypercrud.browser/route                   (r/pure (vec route)) ; vec for direct positional access
                    :hypercrud.browser/route-defaults-hydrated (r/pure route-defaults-hydrated)
                    :hypercrud.browser/route-defaults-symbolic (r/pure route-defaults))
             (context/fiddle+ (r/pure fiddle))))
        (r/pure result))))))

(defn browse-partition+ [ctx]
  (let [route (runtime/get-route (:runtime ctx) (:partition-id ctx))]
    (assert route (str "route not found for pid: " (:partition-id ctx)))
    (internal-browse-route+ ctx route)))
