(ns hyperfiddle.ui)

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

             ; :constructor
             ; (fn [this#]
             ;   (df/put <ctx> (context/ctx0-adapter ctx)))

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
