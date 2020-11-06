(ns contrib.fabric
  (:require
    [promesa.core :as p])
  #?(:clj
     (:import
       haxe.lang.VarArgsBase
       haxe.root.Array
       hyperfiddle.hx.Origin)))


(defn -primary-predicate [v]                                ; this is really bad
  (cond                                                     ; order matters, they overlap
    ;(nil? v) nil?
    ;(keyword? v) keyword?
    ;(symbol? v) symbol?
    (seqable? v) seqable?
    ;(map? v) map?
    ;(set? v) set?
    ;(sequential? v) sequential?
    (fn? v) fn?
    () :default))

(defmulti clj->hx -primary-predicate)
(defmulti hx->clj type)                                     ; e.g. haxe.root.Array

(defmethod clj->hx :default [v] v)
;(defmethod hx->clj :default [v] v)

(defmethod clj->hx fn? [f]
  #?(:cljs (fn [& args] (apply f (seq args)))
     :clj  (proxy [haxe.lang.VarArgsBase] [-1 -1]           ; constructor params
             (__hx_invokeDynamic [args]
               (apply f (seq args))))))

(defmethod clj->hx seqable? [xs]
  #?(:cljs (object-array xs)
     :clj  (let [o (haxe.root.Array.)]
             (doseq [s xs]                                  ; (seq xs) internally
               (.push o s))
             o)))

(defmethod hx->clj haxe.root.Array [v!]
  (let [it (.iterator v!)]
    (iterator-seq
     (reify #?(:clj java.util.Iterator)
       (hasNext [this] (.hasNext it))
       (next [this] (.next it))))))

(defmethod hx->clj haxe.lang.Function [hxf]
  (fn hx-call-proxy [& args]
    (.__hx_invokeDynamic hxf (into-array Object args))))

(set! (. Origin -onError) (clj->hx #(throw %)))

(defn input [& [lifecycle-fn]] (Origin/input lifecycle-fn))

(defn on [>a f] (Origin/on >a (clj->hx f)))
(defn off [output] (.off output))

(defn put [>a v] (.put >a v) nil)

(defn fmap [f & >as]
  (Origin/apply (clj->hx >as)
    (clj->hx (fn [hx-args]
               (apply f (hx->clj hx-args))))))

(defn fmap-async [f & >as]
  (Origin/applyAsync (clj->hx >as)
                     (clj->hx (fn [hx-args, hx-reject, hx-resolve]
                                (let [reject (hx->clj hx-reject)]
                                  (try
                                    (-> (apply f (hx->clj hx-args))
                                        (p/then (hx->clj hx-resolve))
                                        (p/catch reject))
                                    (catch Throwable t
                                      (reject t))))))))

(def fapply fmap)
