(ns contrib.cljs-platform
  #?(:cljs (:require [goog.object :as object]))
  #?(:cljs (:require-macros [contrib.cljs-platform])))


(defmacro nodejs-target?
  "At bootstrap(JVM) compile time inspects the configured build target. only works in macros"
  []
  '(= :nodejs (get-in @cljs.env/*compiler* [:options :target])))

; a compile time check for environment
; most of the time a runtime check will do
; (= "nodejs" *target*); https://cljs.github.io/api/cljs.core/STARtargetSTAR
; which is also public runtime, unlike these macros
; https://stackoverflow.com/a/47499855
(defmacro code-for-nodejs
  [& body]
  (when (nodejs-target?)
    `(do ~@body)))

(defmacro code-for-browser
  [& body]
  (when-not (nodejs-target?)
    `(do ~@body)))

#?(:cljs                                                    ; runtime only
   ; todo this doesn't belong in this ns
   (defn merge-user! [m]
     (when-not (exists? js/user) (set! js/user #js {}))
     (doseq [[k v] m]
       ; They are referenced using FFI syntax: `js/user.docs-fiddle-ident-link`
       (object/set js/user k v))))
