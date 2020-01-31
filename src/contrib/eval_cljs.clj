(ns contrib.eval-cljs
  (:require [cljs.env]
            [contrib.template :refer [load-resource]]))


(defmacro build-cljsjs-empty-state []
  (if-not (false? (get-in @cljs.env/*compiler* [:options :dump-core]))
    '(cljs.js/empty-state)
    ; https://clojurescript.org/guides/self-hosting
    `(let [rdr# (cognitect.transit/reader :json)
           cache# (cognitect.transit/read rdr# (load-resource "core.cljs.cache.aot.json"))
           st# (cljs.js/empty-state)]
       (cljs.js/load-analysis-cache! st# 'cljs.core cache#)
       st#)))
