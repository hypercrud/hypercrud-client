(ns contrib.pprint
  (:require
    [clojure.pprint]
    [clojure.string :as string]
    [hyperfiddle.pprint]
    #?(:cljs [contrib.printer :as printer])))

(defn ^:export pprint [o]
  (binding [clojure.pprint/*print-pprint-dispatch* hyperfiddle.pprint/simple-dispatch]
    (clojure.pprint/pprint o)))

;; This function should not be used for debug purposes. Please use Chrome
;; DevTools instead. It's a costly function and should only run for essential
;; features (stagging area, error message formatting, …). If you still want to
;; use it for logging purposes, be sure to use a proper level-enabled logger or
;; a flag.
(defn ^:export pprint-str [v & [columns]]
  (string/trimr
   ;; Previously, clojure.pprint/*print-miser-width* was set to nil in main
   (binding [clojure.pprint/*print-right-margin* (or columns clojure.pprint/*print-right-margin*)]
     (with-out-str (pprint v)))))

(defn pprint-datoms-str [vv]
  (if (nil? vv)
    ""
    (let [vs (->> (map pr-str vv)
                  (interpose "\n ")
                  vec)]
      (->> (cons "[" (conj vs "]"))
           (apply str)))))

(defn pprint-async
  "Like pprint, but run in another thread."
  [o callback]
  #?(:clj  (callback (pprint o))
     :cljs (printer/pprint o callback)))
