(ns hyperfiddle.ui
  (:require
   [clojure.pprint :refer [pprint]]
   [clojure.walk :as walk]))

(defn quixote*
  [dispatch form]
  (walk/prewalk
    (fn [form]
      (cond
        (or (list? form) (instance? clojure.lang.Cons form))
        (cond
          (contains? dispatch (first form))
          (let [f (get dispatch (first form))
                arg (nth form 1)]
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
                     arg)
                   :else (throw (ex-info (str "Invalid target for modification " k " -- " (type k)) {:target k :form form}))))
               arg
               f)
              (apply (if (symbol? f) (resolve f) f) [arg])))
          :else
          (map
           (fn [form']
             (quixote* dispatch form'))
           form))
        :else form))
    form))

(defmacro quixote
  [dispatch & form]
  `(do
    ~@(quixote* dispatch form)))

(defmacro baxorata
  [deps-sym & body]
  (let [cons? #(instance? clojure.lang.Cons %)

        listish? (some-fn list? cons?)

        unquote
        (fn [form]
          (let [fmap (first form)
                form (rest form)
                reactors (mapv (fn [arg] `(if (df/dataflow? ~arg) ~arg (df/pure ~arg))) form)]
            `(apply df/map ~fmap ~reactors)))

        react
        (fn [form]
          (let [stream (gensym "stream_")]
            `(get (vswap! ~deps-sym assoc ~stream ~form) ~stream)))]

    `(quixote
      {clojure.core/unquote
       {~listish? ~unquote
        ~vector? ~(comp unquote (partial cons `vector))}

       clojure.core/unquote-splicing
        {~listish? ~(comp (partial list `deref) react unquote)
         ~symbol? ~(comp (partial list `deref) react)
         ~vector? ~(comp (partial list `deref) react unquote (partial cons `vector))}

       ~'react
       ~react}
      ~@body)))

(defmacro component
  [& body]
  (let [deps-sym (gensym 'deps_)]
    `(let [~deps-sym (volatile! nil)
           render-body# (volatile! nil)
           render-count# (volatile! 0)]
       (reagent/create-class
        {:render
         (fn [this#]
           (baxorata ~deps-sym ~@body))

         :component-did-mount
         (fn [this#]
           (doseq [dep# (vals @~deps-sym)]
             (df/consume
              (fn [val#]
                ; (println 'update! this#)
                (reagent/force-update ^js this#))
              (df/dedupe dep#))))

         :component-will-unmount
         (fn [_#]
           (doseq [dep# (vals @~deps-sym)]
             (df/end dep#)))}))))


(defmacro |>
  [bindings & body]
  (let [this-sym (gensym 'this)
        outputs-sym (gensym 'outputs)
        state-marker-sym (gensym 'state-marker)]
    `(let [~outputs-sym (volatile! [])
           ~state-marker-sym (atom 0)]
       (let ~bindings
           (reagent/create-class
            {:render
             (fn []
               ~@body)

             :component-did-mount
             (fn [~this-sym]
               ~@(map
                  (fn [sym]
                    `(vswap! ~outputs-sym conj
                             (df/consume
                              (fn [val#] (reagent/force-update ~(with-meta this-sym {:tag 'js})))
                              (df/dedupe ~sym))))
                  (map first (partition 2 bindings))))

             :component-will-unmount
             (fn [_#]
               (doseq [output# @~outputs-sym]
                 (df/end output#)))})))))

; i <- [1 2 3 4]
; sequence
; | -> stream[1]  -> render
;   -> stream[2]  -> render
;   -> stream[3]  -> render
;   -> stream[4]  -> render

;------
; i <- [1 2 3 5]
; sequence
; | -> stream[1]  -> render
;   -> stream[2]  -> render
;   -> stream[3]  -> render
;   -> stream[5]  -> render

; (df/put i [1 2 3 4])
; (df/put i [1 2 3 5])
;
;
; {:a 1 :b 2}
;
; (|> [<a> (df/map :a <ctx>)]
;     [:pre (str a)])
;
;

; (defmacro <|>
;   [bindings & body]
;   (let [stream-bindings (into {} (map vector (take-nth 2 bindings) (repeatedly #(gensym))))
;         this-sym (gensym 'this)
;         outputs-sym (gensym 'outputs)]
;
;     `(let [~outputs-sym (volatile! [])]
;
;        (let ~(vec (mapcat
;                    (fn [[n v]]
;                      [(get stream-bindings n) v])
;                    (partition 2 bindings)))
;          (let ~(vec (mapcat
;                      (fn [[n v]]
;                        [n `(deref ~(get stream-bindings n))])
;                      (partition 2 bindings)))
;            (reagent/create-class
;             {:render
;              (fn []
;                ~@body)
;
;              ; :constructor
;              ; (fn [this#]
;              ;   (df/put <ctx> (context/ctx0-adapter ctx)))
;
;              :component-did-mount
;              (fn [~this-sym]
;                ~@(map
;                   (fn [sym]
;                     `(vswap! ~outputs-sym conj
;                              (df/consume
;
;                               (df/dedupe ~(get stream-bindings sym)))))
;                   (map first (partition 2 bindings))))
;
;              :component-will-unmount
;              (fn [_#]
;                (doseq [output# @~outputs-sym]
;                  (df/end output#)))}))))))
