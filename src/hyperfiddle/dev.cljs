(ns hyperfiddle.dev
  (:require
    [contrib.data :refer [update-existing]]
    [contrib.pprint :refer [pprint pprint-str]]
    [hypercrud.browser.routing :as routing]
    [hyperfiddle.runtime :as runtime]))


(defn set-globals []
  (set! js/pr pr)
  (set! js/pr_str pr-str)
  (set! js/kw keyword)
  (set! js/get get)
  (set! js/pprint pprint)
  (set! js/pprint_str pprint-str)
  (set! js/hc_where (fn [ctx]
                      (-> ctx
                          (select-keys [:hypercrud.browser/route ; ordered for glance debugging
                                        :hypercrud.browser/data
                                        :hypercrud.browser/field
                                        :hypercrud.browser/path])
                          (update-existing :hypercrud.browser/route deref)
                          (update-existing :hypercrud.browser/data deref)
                          (update-existing :hypercrud.browser/field deref)
                          (update-existing :hypercrud.browser/parent select-keys [:hypercrud.browser/field :hypercrud.browser/path])
                          (pprint-str 150))))
  (set! js/hc_route (fn [ctx] (-> ctx :hypercrud.browser/route deref pprint-str)))
  (set! js/hc_root_route (fn []
                           (js/console.warn "WARNING: hc_root_route needs to use the runtime for decoding, this will be broken with custom user routing")
                           (-> js/document.location.pathname
                               routing/decode
                               pprint-str)))
  )
