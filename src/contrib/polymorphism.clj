; (ns contrib.polymorphism)
;
; (defn ->multifn
;   [name dispatch-fn default-dispatch hierarchy invoke-fn]
;   (let [get-fn (fn [this dispatch-value]
;                  (let [method (or (.getMethod this dispatch-value) (.getMethod this default-dispatch))]
;                    (when-not method
;                      (IllegalArgumentException.
;                       (str "No method in multimethod '"
;                            name
;                            "' for dispatch value: "
;                            dispatch-value)))
;                    method))]
;     (proxy [clojure.lang.MultiFn]
;       [name dispatch-fn default-dispatch hierarchy]
;       (invoke ([]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn))))
;               ([a]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn a))
;                 a))
;               ([a b]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn a b))
;                 a b))
;               ([a b c]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn a b c))
;                 a b c))
;               ([a b c d]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn a b c d))
;                 a b c d))
;               ([a b c d e]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn a b c d e))
;                 a b c d e))
;               ([a b c d e f]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn a b c d e f))
;                 a b c d e f))
;               ([a b c d e f g]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn a b c d e f g))
;                 a b c d e f g))
;               ([a b c d e f g h]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn a b c d e f g h))
;                 a b c d e f g h))
;               ([a b c d e f g h i]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn a b c d e f g h i))
;                 a b c d e f g h i))
;               ([a b c d e f g h i j]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn a b c d e f g h i j))
;                 a b c d e f g h i j))
;               ([a b c d e f g h i j k]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn a b c d e f g h i j k))
;                 a b c d e f g h i j k))
;               ([a b c d e f g h i j k l]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn a b c d e f g h i j k l))
;                 a b c d e f g h i j k l))
;               ([a b c d e f g h i j k l m]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn a b c d e f g h i j k l m))
;                 a b c d e f g h i j k l m))
;               ([a b c d e f g h i j k l m n]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn a b c d e f g h i j k l m n))
;                 a b c d e f g h i j k l m n))
;               ([a b c d e f g h i j k l m n o]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn a b c d e f g h i j k l m n o))
;                 a b c d e f g h i j k l m n o))
;               ([a b c d e f g h i j k l m n o p]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn a b c d e f g h i j k l m n o p))
;                 a b c d e f g h i j k l m n o p))
;               ([a b c d e f g h i j k l m n o p q]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn a b c d e f g h i j k l m n o q))
;                 a b c d e f g h i j k l m n o q))
;               ([a b c d e f g h i j k l m n o p q r]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn a b c d e f g h i j k l m n o q r))
;                 a b c d e f g h i j k l m n o q r))
;               ([a b c d e f g h i j k l m n o p q r s]
;                (invoke-fn
;                 (get-fn this (.invoke dispatch-fn a b c d e f g h i j k l m n o q r rest))
;                 a b c d e f g h i j k l m n o q r rest))))))
;
; ; (defmacro defmulti*
; ;   "Creates a new multimethod with the associated dispatch function.
; ;   The docstring and attr-map are optional.
; ;
; ;   Options are key-value pairs and may be one of:
; ;
; ;   :default
; ;
; ;   The default dispatch value, defaults to :default
; ;
; ;   :hierarchy
; ;
; ;   The value used for hierarchical dispatch (e.g. ::square is-a ::shape)
; ;
; ;   :invoke-fn
; ;   The function used to invoke the dispatched on function
; ;
; ;   Hierarchies are type-like relationships that do not depend upon type
; ;   inheritance. By default Clojure's multimethods dispatch off of a
; ;   global hierarchy map.  However, a hierarchy relationship can be
; ;   created with the derive function used to augment the root ancestor
; ;   created with make-hierarchy.
; ;
; ;   Multimethods expect the value of the hierarchy option to be supplied as
; ;   a reference type e.g. a var (i.e. via the Var-quote dispatch macro #'
; ;   or the var special form)."
; ;   {:arglists '([name docstring? attr-map? dispatch-fn & options])
; ;    :added "1.0"}
; ;   [mm-name & options]
; ;   (let [docstring   (if (string? (first options))
; ;                       (first options)
; ;                       nil)
; ;         options     (if (string? (first options))
; ;                       (next options)
; ;                       options)
; ;         m           (if (map? (first options))
; ;                       (first options)
; ;                       {})
; ;         options     (if (map? (first options))
; ;                       (next options)
; ;                       options)
; ;         dispatch-fn (first options)
; ;         options     (next options)
; ;         invoke-fn   (first options)
; ;         options     (next options)
; ;         m           (if docstring
; ;                       (assoc m :doc docstring)
; ;                       m)
; ;         m           (if (meta mm-name)
; ;                       (conj (meta mm-name) m)
; ;                       m)
; ;         mm-name (with-meta mm-name m)]
; ;     (when (= (count options) 1)
; ;       (throw (Exception. "The syntax for defmulti has changed. Example: (defmulti name dispatch-fn :default dispatch-value)")))
; ;     (let [options   (apply hash-map options)
; ;           default   (get options :default :default)
; ;           hierarchy (get options :hierarchy #'clojure.core/global-hierarchy)]
; ;       (@#'clojure.core/check-valid-options options :default :hierarchy)
; ;       `(let [v# (def ~mm-name)]
; ;          (when-not (and (.hasRoot v#) (instance? clojure.lang.MultiFn (deref v#)))
; ;            (def ~mm-name
; ;              (if ~invoke-fn
; ;                (->multifn ~(name mm-name) ~dispatch-fn ~default ~hierarchy ~invoke-fn)
; ;                (new clojure.lang.MultiFn ~(name mm-name) ~dispatch-fn ~default ~hierarchy))))))))
;
; (defmacro defmulti*
;   [mm-name & options]
;   (let [docstring   (if (string? (first options))
;                       (first options)
;                       nil)
;         options     (if (string? (first options))
;                       (next options)
;                       options)
;         m           (if (map? (first options))
;                       (first options)
;                       {})
;         options     (if (map? (first options))
;                       (next options)
;                       options)
;         dispatch-fn (first options)
;         options     (next options)
;         invoke-fn   (first options)
;         options     (next options)
;         m           (if docstring
;                       (assoc m :doc docstring)
;                       m)
;         m           (if (meta mm-name)
;                       (conj (meta mm-name) m)
;                       m)
;         mm-ns (-> &env :ns :name str)]
;     (when (= (count options) 1)
;       (throw
;         (ex-info {} "The syntax for defmulti has changed. Example: (defmulti name dispatch-fn :default dispatch-value)")))
;     (let [options (apply hash-map options)
;                default (get options :default :default)]
;       ; (cljs.core/check-valid-options options :default :hierarchy)
;       `(defonce ~(with-meta mm-name m)
;          (let [method-table# (atom {})
;                prefer-table# (atom {})
;                method-cache# (atom {})
;                cached-hierarchy# (atom {})
;                hierarchy# (cljs.core/get ~options :hierarchy (get-global-hierarchy))]
;            (if ~invoke-fn
;              (PolymorphismMultiFn. (cljs.core/symbol ~mm-ns ~(name mm-name)) ~dispatch-fn ~default hierarchy#
;                                    method-table# prefer-table# method-cache# cached-hierarchy# ~invoke-fn)
;              (cljs.core/MultiFn. (cljs.core/symbol ~mm-ns ~(name mm-name)) ~dispatch-fn ~default hierarchy#
;                                  method-table# prefer-table# method-cache# cached-hierarchy#)))))))
