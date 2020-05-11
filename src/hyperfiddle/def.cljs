(ns hyperfiddle.def
  (:require-macros [hyperfiddle.def]))

(def *defs (atom (hyperfiddle.def/lookup)))

(defn get-def [& ks] (get-in @*defs ks))

(defn get-schemas []
  (@*defs :schema))

(defn get-attrs []
  (into {} (map (fn [[k v]] {k (:renderer v)}) (@*defs :attr))))

(defn get-fiddle [id] (get-in @*defs [:fiddle id]))
