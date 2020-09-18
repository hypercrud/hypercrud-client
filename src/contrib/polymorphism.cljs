(ns contrib.polymorphism)

(defn- reset-cache
  [method-cache method-table cached-hierarchy hierarchy]
  (swap! method-cache (fn [_] (deref method-table)))
  (swap! cached-hierarchy (fn [_] (deref hierarchy))))

(defn- prefers*
  [x y prefer-table]
  (let [xprefs (@prefer-table x)]
    (or
     (when (and xprefs (xprefs y))
       true)
     (loop [ps (parents y)]
       (when (pos? (count ps))
         (when (prefers* x (first ps) prefer-table)
           true)
         (recur (rest ps))))
     (loop [ps (parents x)]
       (when (pos? (count ps))
         (when (prefers* (first ps) y prefer-table)
           true)
         (recur (rest ps))))
     false)))

(defn- dominates
  [x y prefer-table hierarchy]
  (or (prefers* x y prefer-table) (isa? hierarchy x y)))

(defn- find-and-cache-best-method
  [name match-fn compare-fn dispatch-val hierarchy method-table prefer-table method-cache cached-hierarchy default-dispatch-val]
  (let [match-fn (or match-fn isa?)
        compare-fn (or compare-fn (fn [x y] (dominates y x prefer-table hierarchy)))
        best-entry (reduce (fn [be [k _ :as e]]
                             (if (match-fn k dispatch-val)
                               (let [be2 (if (or (nil? be) (not (compare-fn k (first be))))
                                           e
                                           be)]
                                 (when-not (compare-fn (first be2) k)
                                   (throw (js/Error.
                                            (str "Multiple methods in multimethod '" name
                                              "' match dispatch value: " dispatch-val " -> " k
                                              " and " (first be2) ", and neither is preferred"))))
                                 be2)
                               be))
                     nil @method-table)
        best-entry (if-let [entry (and (nil? best-entry) (@method-table default-dispatch-val))]
                     [default-dispatch-val entry]
                     best-entry)]
    (when best-entry
      (if (= @cached-hierarchy @hierarchy)
        (do
          (swap! method-cache assoc dispatch-val (second best-entry))
          (second best-entry))
        (do
          (reset-cache method-cache method-table cached-hierarchy hierarchy)
          (find-and-cache-best-method name match-fn compare-fn dispatch-val hierarchy method-table prefer-table
            method-cache cached-hierarchy default-dispatch-val))))))


(deftype XMultiFn [name dispatch-fn match-fn compare-fn invoke-fn default-dispatch-val hierarchy
                   method-table prefer-table method-cache cached-hierarchy]
  IFn
  (-invoke [mf]
    (let [dispatch-val (dispatch-fn)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn)))
  (-invoke [mf a]
    (let [dispatch-val (dispatch-fn a)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a)))
  (-invoke [mf a b]
    (let [dispatch-val (dispatch-fn a b)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a b)))
  (-invoke [mf a b c]
    (let [dispatch-val (dispatch-fn a b c)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a b c)))
  (-invoke [mf a b c d]
    (let [dispatch-val (dispatch-fn a b c d)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a b c d)))
  (-invoke [mf a b c d e]
    (let [dispatch-val (dispatch-fn a b c d e)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a b c d e)))
  (-invoke [mf a b c d e f]
    (let [dispatch-val (dispatch-fn a b c d e f)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a b c d e f)))
  (-invoke [mf a b c d e f g]
    (let [dispatch-val (dispatch-fn a b c d e f g)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a b c d e f g)))
  (-invoke [mf a b c d e f g h]
    (let [dispatch-val (dispatch-fn a b c d e f g h)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a b c d e f g h)))
  (-invoke [mf a b c d e f g h i]
    (let [dispatch-val (dispatch-fn a b c d e f g h i)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a b c d e f g h i)))
  (-invoke [mf a b c d e f g h i j]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a b c d e f g h i j)))
  (-invoke [mf a b c d e f g h i j k]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a b c d e f g h i j k)))
  (-invoke [mf a b c d e f g h i j k l]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k l)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a b c d e f g h i j k l)))
  (-invoke [mf a b c d e f g h i j k l m]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k l m)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a b c d e f g h i j k l m)))
  (-invoke [mf a b c d e f g h i j k l m n]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k l m n)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a b c d e f g h i j k l m n)))
  (-invoke [mf a b c d e f g h i j k l m n o]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k l m n o)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a b c d e f g h i j k l m n o)))
  (-invoke [mf a b c d e f g h i j k l m n o p]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k l m n o p)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a b c d e f g h i j k l m n o p)))
  (-invoke [mf a b c d e f g h i j k l m n o p q]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k l m n o p q)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a b c d e f g h i j k l m n o p q)))
  (-invoke [mf a b c d e f g h i j k l m n o p q r]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k l m n o p q r)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a b c d e f g h i j k l m n o p q r)))
  (-invoke [mf a b c d e f g h i j k l m n o p q r s]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k l m n o p q r s)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a b c d e f g h i j k l m n o p q r s)))
  (-invoke [mf a b c d e f g h i j k l m n o p q r s t]
    (let [dispatch-val (dispatch-fn a b c d e f g h i j k l m n o p q r s t)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (invoke-fn target-fn a b c d e f g h i j k l m n o p q r s t)))
  (-invoke [mf a b c d e f g h i j k l m n o p q r s t rest]
    (let [dispatch-val (apply dispatch-fn a b c d e f g h i j k l m n o p q r s t rest)
          target-fn (-get-method mf dispatch-val)]
      (when-not target-fn
        (throw-no-method-error name dispatch-val))
      (apply invoke-fn target-fn a b c d e f g h i j k l m n o p q r s t rest)))

  IMultiFn
  (-reset [mf]
    (swap! method-table (fn [_] {}))
    (swap! method-cache (fn [_] {}))
    (swap! prefer-table (fn [_] {}))
    (swap! cached-hierarchy (fn [_] nil))
    mf)

  (-add-method [mf dispatch-val method]
    (swap! method-table assoc dispatch-val method)
    (reset-cache method-cache method-table cached-hierarchy hierarchy)
    mf)

  (-remove-method [mf dispatch-val]
    (swap! method-table dissoc dispatch-val)
    (reset-cache method-cache method-table cached-hierarchy hierarchy)
    mf)

  (-get-method [mf dispatch-val]
    (when-not (= @cached-hierarchy @hierarchy)
      (reset-cache method-cache method-table cached-hierarchy hierarchy))
    (if-let [target-fn (@method-cache args)]
      target-fn
      (find-and-cache-best-method name match-fn compare-fn dispatch-val hierarchy method-table
        prefer-table method-cache cached-hierarchy default-dispatch-val)))

  (-prefer-method [mf dispatch-val-x dispatch-val-y]
    (when (prefers* dispatch-val-x dispatch-val-y prefer-table)
      (throw (js/Error. (str "Preference conflict in multimethod '" name "': " dispatch-val-y)
                   " is already preferred to " dispatch-val-x)))
    (swap! prefer-table
           (fn [old]
             (assoc old dispatch-val-x
                    (conj (get old dispatch-val-x #{})
                          dispatch-val-y))))
    (reset-cache method-cache method-table cached-hierarchy hierarchy))

  (-methods [mf] @method-table)
  (-prefers [mf] @prefer-table)
  (-default-dispatch-val [mf] default-dispatch-val)
  (-dispatch-fn [mf] dispatch-fn)

  INamed
  (-name [this] (-name name))
  (-namespace [this] (-namespace name))

  IHash
  (-hash [this] (goog/getUid this)))
