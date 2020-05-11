(ns hyperfiddle.scope
  #?(:cljs
     (:require-macros hyperfiddle.scope)))

(def ^:dynamic *vars {})
(def ^:dynamic *scope nil)

(defmacro scope [desc & run]
  `(binding [*scope (conj (or *scope []) ~desc)] ~@run))

(defn push-scope [desc]
  (set! *scope (conj (or *scope []) desc)))

(defn with-scope [f]
  (let [s *scope]
    (fn [& args] (binding [*scope s] (apply f args)))))
