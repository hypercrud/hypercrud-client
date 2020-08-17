(ns hypercrud.browser.browser-request
  (:require
    [cats.monad.either :as either]
    [contrib.data :refer [unqualify]]
    [contrib.do :as do]
    [contrib.datomic]
    [contrib.reactive :as r]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hyperfiddle.api :as hf]
    [hyperfiddle.data :as data]
    [hyperfiddle.runtime :as runtime]
    [taoensso.timbre :as timbre]))


(declare requests)

(defn requests-here [{rt :runtime :as ctx}]
  (doseq [link @(data/select-many-here ctx #{:hf/iframe})]  ; would like to select #{:hf/iframe (not :tx-fn)}
    (if (context/branched-link?' link)
      nil                                                   ; this fn performs effects, the result is never inspected
      (either/branch
        (context/refocus-build-route-and-occlude+ ctx (r/pure link))
        (fn [e] (timbre/warn e))                            ; do we actually care about this error?
        (fn [[link-ctx route]]
          (let [new-pid (context/build-pid-from-link ctx link-ctx route)]
            (condp = (runtime/get-route rt new-pid)
              nil (do (runtime/create-partition rt (:partition-id ctx) new-pid false)
                      (hf/set-route rt new-pid route))
              route (timbre/warn "Revisiting already created pid. Potential performance issue" route)
              (throw (ex-info "pid generation non-unique" {:new-pid        new-pid
                                                           :existing-route (runtime/get-route rt new-pid)
                                                           :route          route
                                                           :link           link})))
            (-> (context/set-partition link-ctx new-pid)
                (base/browse-partition+)
                (either/branch
                 (fn [e]
                   (timbre/warn e)
                   (runtime/set-error rt new-pid e))
                 requests))))))))

; at this point we only care about inline links and popovers are hydrated on their on hydrate-route calls
; On the request side, we walk the whole resultset and load each iframe from exactly the right place
; without any refocusing. Only on the view side do we care about drawing things in some other place.

(defn request-attr-level [ctx]
  (->> (context/spread-attributes ctx)
       (contrib.data/pmap
        (fn [[a ctx]]
          (requests-here ctx)
          ;; UnsupportedOperationException: Can only recur from tail position
          (request-attr-level ctx)))
       (doall)))

; Spread across the resultset
; Create the links at each slot
; Index this by result-path
; index links by result-path
; [ctx route] (context/link slot :hf/iframe)

; Spread the slots
; at each slot foreach link Does the link match the slot?
; Accumulate a map of slot->#{links} (links is a sub index? {#{corcs}->link}
; At any slot it is very easy to know what the links are
; Very easy to refocus to another slot to get that link

(defn requests [ctx]
  ; More efficient to drive from links. But to do this, we need to refocus
  ; from the top, multiplying out for all possible dependencies.
  (do/scope [`requests {:pid    (:partition-id ctx)
                     :fiddle (:fiddle/ident @(:hypercrud.browser/fiddle ctx))
                     :route  @(:hypercrud.browser/route ctx)}]
    (do/scope 'requests-here
      (requests-here ctx))
    (do/scope 'spread
      (doseq [[_ ctx] (context/spread-result ctx)]
        (requests-here ctx)                                 ; depend on result? Not sure if right
        (doseq [[_ ctx] (context/spread-rows ctx)]
          #_(requests-here ctx)
          (doseq [[_ {el :hypercrud.browser/element :as ctx}] (context/spread-elements ctx)]
            (case (unqualify (contrib.datomic/parser-type @el))
              :variable nil #_(requests-here ctx)
              :aggregate nil #_(requests-here ctx)
              :pull (do (requests-here ctx)
                        ; Dependent attr links, slow af though, so disabled for now.
                        (request-attr-level ctx)))))

        ; no rows - independent
        (doseq [[_ {el :hypercrud.browser/element :as ctx}] (context/spread-elements ctx)]
          (case (unqualify (contrib.datomic/parser-type @el))
            :variable nil #_(requests-here ctx)
            :aggregate nil #_(requests-here ctx)
            :pull (do (requests-here ctx)
                      (request-attr-level ctx))))))))
