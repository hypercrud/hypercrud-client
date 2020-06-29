(ns hyperfiddle.def
  (:require-macros [hyperfiddle.def])
  (:require [hyperfiddle.spec]))

(def *defs (atom (hyperfiddle.def/lookup)))                 ; Copy JVM defs into CLJS via macro
; Can we get shadow-cljs to recompile this line whenever the clj *defs map changes?

(defn get-def [& ks] (get-in @*defs ks))

(defn get-schemas []
  (@*defs :schema))

(defn get-fiddle [id] (get-in @*defs [:fiddle id]))
