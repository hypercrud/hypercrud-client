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
                args (rest form)]
            (if (or (map? f) (vector? f))
              (reduce
               (fn [arg [k v]]
                 (cond
                   (class? k)
                   (if (instance? k arg)
                     (apply v arg)
                     arg)
                   (fn? k)
                   (if (apply k arg)
                     (apply v arg)
                     arg)
                   :else (throw (ex-info (str "Invalid target for modification " k " -- " (type k)) {:target k :form form}))))
               args
               f)
              (apply (if (symbol? f) (resolve f) f) args)))
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

        unquote-map
        (fn [form]
          (let [fmap `hash-map
                form (mapcat identity form)
                reactors (mapv (fn [arg] `(if (df/dataflow? ~arg) ~arg (df/pure ~arg))) form)]
            `(apply df/map ~fmap ~reactors)))

        unquote-vector
        (fn [form]
          (let [fmap `vector
                reactors (mapv (fn [arg] `(if (df/dataflow? ~arg) ~arg (df/pure ~arg))) form)]
            `(apply df/map ~fmap ~reactors)))

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
        ~vector?  ~(comp unquote-vector)
        ~map?     ~(comp unquote-map)}

       clojure.core/unquote-splicing
        {~listish? ~(comp (partial list `deref) react unquote)
         ~symbol?  ~(comp (partial list `deref) react)
         ~vector?  ~(comp (partial list `deref) react unquote-vector)
         ~map?     ~(comp (partial list `deref) react unquote-map)}

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

; ~(g ~(f ~x ~y))

(defn ehutupapa*
  [form]
  (let [forms (volatile! {})
        form (clojure.walk/postwalk
              (fn [form]
                (if (and (seq? form) (= (first form) 'clojure.core/unquote))
                  (let [form (second form)]
                    (or (get @forms form)
                        (let [sym (gensym #_(str (first (remove seqable? (iterate first form))) "_"))]
                          (vswap! forms assoc sym form)
                          `(unquote ~sym))))
                  form))
              form)]
    [form @forms]))

(defn ehutupapa
  [form]
  (let [[form forms] (ehutupapa* form)
        resolved-sym? (fn [resolved sym]
                        (if (and (seq? sym) (= `unquote (first sym)))
                          (and (get resolved (second sym)) (second sym))
                          sym))
        sym? (fn [sym] (and (seq? sym) (= `unquote (first sym)) (second sym)))
        unresolved (into {} (remove (fn [[_ form]] (symbol? form)) forms))
        resolved (into {} (filter (fn [[_ form]] (symbol? form)) forms))]
    (loop [forms unresolved
           acc [resolved]
           resolved-syms (set (keys resolved))]
      (if (empty? forms)
        [form (vec (remove empty? acc))]
        (let [resolved (into {}
                             (remove
                              (comp nil? second)
                              (map
                               (fn [[sym form]]
                                 [sym
                                  (when (every? (partial resolved-sym? resolved-syms) form)
                                    [(map sym? (filter sym? form))
                                     (map (partial resolved-sym? resolved-syms) form)])])
                               forms)))
              remain (into {}
                           (remove
                            (fn [[_ form]]
                              (every? (partial resolved-sym? resolved-syms) form))
                            forms))]
          (when (= remain forms)
            (throw (Exception. "Unsolvable")))
          (recur remain (conj acc resolved) (into resolved-syms (keys resolved))))))))

(defn bucatini*
  [fmap fapply f [form & forms]]
  (if (empty? forms)
    (vals form)
    (cond
      (empty? form)
      (bucatini* fmap fapply f forms)
      (= (count form) 1)
      `((~fmap
          ~(reduce
            (fn [acc v]
              `(fn [~v] ~@acc))
            [:fuck]
            (keys form))
          ~@(bucatini* fmap fapply f forms)))
      :else
      `((~fapply
         ~(reduce
           (fn [acc v]
             `(fn [~v] ~acc))
           (first (bucatini* fmap fapply f forms))
           (reverse (keys form)))
         ~@(vals form))))))

(defn bucatini*
  [fmap fapply f [form & forms]]
  (if (empty? forms)
    (if (> (count form) 1)
      (map (fn [x] (if (symbol? x) x (second x))) (vals form))
      (let [form (first (vals form))]
        (if (symbol? form)
          form
          (second form))))
    (if (empty? form)
      (bucatini* fmap fapply f forms)
      (let [[_ [syms form]] (first form)]
        (if (= (count syms) 1)
          `(~fmap
             (fn [~@syms]
               ~form)
             ~(bucatini* fmap fapply f forms))
          `(~fapply
             ~(reduce
               (fn [acc v]
                 `(fn [~v] ~acc))
               form
               syms)
             ~@(bucatini* fmap fapply f forms)))))))


(defn bucatini
  [fmap fapply [form forms]]
  `~(bucatini* fmap fapply form (reverse forms)))

(defn jabberwocky
  [fmap fapply body]
  (clojure.walk/prewalk
   (fn [form]
     (if (and (seq? form) (= (first form) 'clojure.core/unquote))
       (bucatini fmap fapply (ehutupapa form))
       form))
   body))

(defmacro defjester
  [name fmap fapply]
  `(defmacro ~name
     [& body#]
     (let [forms# (jabberwocky ~fmap ~fapply body#)]
       (if (> (count forms#) 1)
         (cons `do forms#)
         (first forms#)))))

(cats
 (let [x ~(just 1)
       y ~(inc ~x)]
   (+ x y)))

(cats (let [x ~(just 1) y ~(inc ~x)] (fapply x y)))

~(f ~x)



(cats
 (let [x ~(just 1)
       y ~(inc x)]
   (+ x y)))

; (do
;  (hyperfiddle.ui/fmap
;   (clojure.core/fn
;    [G__19242]
;    (hyperfiddle.ui/fmap
;     (clojure.core/fn [G__19243] :end)
;     (inc G__19242)))
;   (just 1)))
;
; (do
;  (hyperfiddle.ui/fmap
;   (clojure.core/fn
;    [G__19242]
;    (inc G__19242))
;   (hyperfiddle.ui/fmap
;    (clojure.core/fn [G__19243] :end)
;    (just 1))))

(require '[cats.core :as cats])
(require '[cats.monad.maybe :refer [just nothing]])

(defjester cats cats/fmap (fn [f & av] (apply cats/fapply (just f) av)))

; (cats ~(inc ~(just 1))) ;=> #<Just 2>
; (cats ~(~(just +) ~(just 1) ~(just 2))) ;=> #<Just 3>
; (cats ~(+ ~(just 1) ~(nothing))) ;=> #<Nothing>

; (pprint
;  (ehutupapa
;    '(let [x ~(just 1)
;           y ~(inc ~x)
;           z ~(+ ~x ~y)]
;       z)))


(require '[promesa.core :as p])

(defjester via-promise
  p/map
  (fn [f & ps]
    (let [ps (pmap deref ps)]
      (p/promise
       (reduce apply f (map vector ps))))))


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
