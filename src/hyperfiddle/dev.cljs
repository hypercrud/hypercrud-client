(ns hyperfiddle.dev
  (:require
    [contrib.data :refer [update-existing]]
    [contrib.pprint :refer [pprint pprint-str]]
    [hyperfiddle.api]))


(set! js/pr pr)
(set! js/pr_str pr-str)
(set! js/kw keyword)
(set! js/get get)
(set! js/pprint pprint)
(set! js/pprint_str pprint-str)
(set! js/hc_route (fn [ctx] (-> ctx :hypercrud.browser/route deref pprint-str)))
(set! js/hf js/hyperfiddle.api)
