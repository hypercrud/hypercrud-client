(ns hypercrud.browser.q-util
  (:require
    [clojure.string]
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [parse-query-element]]
    [contrib.reader :as reader]
    [contrib.try$ :refer [try-either]]
    ))


(defn parse-holes [q]
  {:pre [(vector? q)]
   :post [(vector? %)]}
  (as-> (parse-query-element q :in) elements
                                    (if (empty? elements) ['$] elements)
                                    ;; the string conversion should happen at the other side imo
                                    (mapv str elements)))

(defn args [s-query]
  (let [q (unwrap (constantly nil) (reader/memoized-read-string+ s-query))
        holes (unwrap (constantly nil) (try-either (parse-holes q)))]
    (remove #(clojure.string/starts-with? % "$") holes)))
