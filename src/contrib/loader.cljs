(ns contrib.loader
  (:require
    [contrib.cljs-platform :refer [code-for-browser]]
    [contrib.string :refer [blank->nil]]
    [goog.object]
    [promesa.core :as p]
    [reagent.core]))


(defn script! [href & [{:keys [defer async]
                        :or {defer false
                             async false}}]]
  (p/promise
    (fn [resolve reject]
      (code-for-browser                                     ; can be called from domain/cljs
        (let [el (js/document.createElement "script")]
          ; https://stackoverflow.com/questions/21214515/is-the-order-of-onload-handler-and-src-set-important-in-a-script-element
          ; https://stackoverflow.com/questions/16230886/trying-to-fire-the-onload-event-on-script-tag
          (doto el
            (goog.object/set "defer" defer)                 ; In SSR case, defer onLoad until after body is parsed, but before DOMContentLoaded
            (goog.object/set "async" async)                 ; In non-SSR case, this reduces time-to-glass but can introduce a flash of "unstyled" content
            (goog.object/set "onload" #(resolve el))
            (goog.object/set "src" href)
            (js/document.body.appendChild)))))))

(defn style! [href & [props]]
  (p/promise
    (fn [resolve reject]
      (code-for-browser
        (let [el (js/document.createElement "link")]
          (doto el
            (goog.object/set "rel" "stylesheet")
            (goog.object/set "type" "text/css")
            (goog.object/set "href" href)
            (js/document.head.appendChild))
          (resolve el))))))

(defn load-asset! [href props]
  (condp #(clojure.string/ends-with? %2 %1) href
    ".css" (style! href props)
    (script! href props)))

(def ^:export Loader!
  (reagent.core/create-class
    {:reagent-render (fn [])
     :component-did-mount
     (fn [this]
       (let [[_ asset-hrefs resolve! props] (reagent.core/argv this)]
         (-> (->> asset-hrefs
                  (filter blank->nil)
                  (map #(load-asset! % props))
                  doall)
             p/all
             (p/then (fn [els]
                       (goog.object/set this "refs" els)
                       (when resolve!
                         (resolve!)))))))
     :component-will-unmount
     (fn [this]
       (doseq [el (goog.object/get this "refs")]
         (.removeChild (.-parentNode el) el)))}))
