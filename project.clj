
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This project uses clojure deps, not leiningen.   ;;
;; This file is required for some developement time ;;
;; lein plugins, like `lein-hiera`.                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defproject hyperfiddle "0.1.0-SNAPSHOT"
  :dependencies [[org.clojure/clojure "1.10.0"]]
  :source-paths ["src"]
  :profiles {:dev {:plugins [[lein-hiera "1.2.0-SNAPSHOT"]]}})
