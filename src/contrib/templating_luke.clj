; (ns contrib.templating-luke
;   (:require
;    [clojure.pprint :refer [pprint]]
;    [clojure.walk :as walk]
;    [contrib.dataflow :as df]))
;
; (defn quixote*
;   [dispatch form]
;   (walk/prewalk
;     (fn [form]
;       (cond
;         (or (list? form) (instance? clojure.lang.Cons form))
;         (cond
;           (contains? dispatch (first form))
;           (let [f (eval (get dispatch (first form)))
;                 arg (nth form 1)]
;             (if (or (map? f) (vector? f))
;               (reduce
;                (fn [arg [k v]]
;                  (cond
;                    (class? k)
;                    (if (instance? k arg)
;                      (apply v [arg])
;                      arg)
;                    (fn? k)
;                    (if (k arg)
;                      (apply v [arg])
;                      arg)))
;                arg
;                f)
;               (apply f [arg])))
;           :else
;           (map
;            (fn [form']
;              (quixote* dispatch form'))
;            form))
;         :else form))
;     form))
;
;
; (defmacro quixote
;   [dispatch & form]
;   `(do
;     ~@(quixote* dispatch form)))
;
; ; (defmacro component
; ;   [& body]
; ;   `(let [deps# (atom {})]
; ;      (quixote
; ;        {clojure.core/unquote
; ;         {seq? (fn [form#] (cons df/map form#))
; ;          (constantly true)}
; ;         clojure.core/deref
; ;         {df/reactive
; ;          (fn [form#] (swap! deps# form#) (list 'deref form#))}}
; ;        ~@body)))
;
;
; (defmacro baxorata
;   [& body]
;   (let [f (fn [form]
;             (let [fmap (first form)
;                   form (rest form)
;                   reactors (mapv (fn [arg] `(if (df/dataflow? ~arg) ~arg (df/pure ~arg))) form)]
;               `(apply df/map ~fmap ~reactors)))]
;     `(quixote
;       {clojure.core/unquote
;        {seq? ~f}}
;       ~@body)))
;
;
; ; (let [in (df/input 0)]
; ;   ; (pprint
; ;   ;  (macroexpand
; ;   ;   '(baxorata
; ;   ;     (let [x ~(inc in)
; ;   ;           y ~(dec in)]
; ;   ;       ~(println ~(* x y))))))
; ;   ; (pprint (macroexpand '(baxorata ~(println '--- in)))))
; ;   ; (df/map println in))
; ;   (baxorata
; ;    (let [x ~(inc in)
; ;          y ~(dec in)]
; ;      ~(println ~(* x y))))
; ;   (df/put in 10))
