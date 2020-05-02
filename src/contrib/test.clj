(ns contrib.test
  (:gen-class)
  (:require
    [clojure.spec.alpha :as s]
    [taoensso.timbre :refer [warn error info]]))


(defmacro check!
  "Checks v (defaults to *1) against spec, throwing on failure. Returns nil."
  ([spec]
   `(check! ~spec *1))
  ([spec v]
   `(s/assert ~spec ~v)))

(def *tests* (atom {}))

(defmacro tests [& body]
  (if true                                                  ; target :test
    `(swap! *tests* assoc (ns-name *ns*) '~body)))

;(def script-counter (atom 0))
(defn run-tests []
  ;(println ">> " @*tests*)
  (doseq [[ns body] @*tests*]
    ;(let [ns (symbol (str "cognitect.transcriptor.t_" (swap! script-counter inc)))])
    (in-ns ns)
    ;(clojure.core/use 'clojure.core)
    (doseq [form body]
      (try
        (println "=> " form)
        ; What is this classloader gibblygook
        ; https://github.com/cognitect-labs/transcriptor/blob/05e9eb093b21d9a3458e0d946ad9dab76ae1e1c8/transcriptor.clj#L46-L47
        (let [value (eval form)]
          (set! *3 *2) (set! *2 *1) (set! *1 value))
        (catch Exception e
          (error e))))))

; https://github.com/weavejester/ns-tracker
; https://github.com/weavejester/reloaded.repl

(defn -main [ns & args]
  ; clj -A:test-clj -m contrib.test hyperfiddle.readers-test2
  (alter-var-root #'*err* (constantly *out*))
  (taoensso.timbre/set-level! :info)
  (info "Running tests ...")
  ; load all namespaces
  (require (symbol ns))
  (run-tests))

(comment
  ; Example

  (ns user
    (:require [contrib.test]))

  (tests
    (inc 1)
    (check! #{2})

    (inc 1)
    (check! #{3})

    (inc 42)
    (check! (s/and odd? #{43} #{44}))

    )

  (contrib.test/run-tests)
  )
