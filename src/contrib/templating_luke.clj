(ns contrib.templating-luke
  (:require
   [contrib.dataflow :as df]))

(defn unquote-via*
  [dispatch form]
  (cond
    (or (list? form) (instance? clojure.lang.Cons form))
    (cond
      (contains? dispatch (first form))
      (let [f (eval (get dispatch (first form)))
            arg (nth form 1)]
        (unquote-via*
          dispatch
          (if (or (map? f) (vector? f))
            (reduce
              (fn [arg [k v]]
                (cond
                  (class? k)
                  (if (instance? k arg)
                    (apply v [arg])
                    arg)
                  (fn? k)
                  (if (k arg)
                    (apply v [arg])
                    arg)))
              arg
              f)
            (apply f [arg]))))
      :else
      (map
       (fn [form']
         (unquote-via* dispatch form'))
       form))
    (vector? form)
    (mapv
     (fn [form']
       (unquote-via* dispatch form'))
     form)
    (map? form)
    (into {}
      (map
       (fn [[k v]]
         [(unquote-via* dispatch k)
          (unquote-via* dispatch v)])
       form))
    :else form))


(defmacro unquote-via
  [dispatch & form]
  `(do
    ~@(unquote-via* dispatch form)))



(defmacro component
  [& body]
  `(let [deps# (atom {})]
     (unquote-via
       {clojure.core/unquote
        {seq? (fn [form#] (cons df/map form#))
         (constantly true) }
        clojure.core/deref
        {df/reactive
         (fn [form#] (swap! deps# form#) (list 'deref form#))}}
       ~@body)))

(defn cell-
  [<ctx>]
  (component [:td {} [:input {:value ~@(:context/value <ctx>)}]]))
