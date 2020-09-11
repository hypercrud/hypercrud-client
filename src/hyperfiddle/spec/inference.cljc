(ns hyperfiddle.spec.inference
  (:refer-clojure :exclude [derive])
  (:require [clojure.core.match :refer [match]]
            [clojure.set :as set]))

(defn derive
  "Like `clojure.core/derive` but keep a parent → child relationship too. Two way
  linking."
  [h tag parent]
  (let [children (:children h)]
    (-> (clojure.core/derive h tag parent)
        (assoc :children (update children parent #(if (nil? %) #{tag} (conj % tag)))))))

(defn children [h tag]
  (get-in h [:children tag]))

(def ^:dynamic *hierarchy*
  "A mapping of type -> parent. Remember your Type Theory: you can’t produce a
  value of :void (⊥), and :any (⊤) doesn’t have a supertype."
  (->>
   {;; Bounds
    ;; :void           ???    ; ⊥ doesn’t have a parent as all types are supertypes of ⊥
    :nil               :any
    ;; ----
    :boolean           :any
    :keyword           :any
    :qualified-keyword :keyword
    :symbol            :any
    :qualified-symbol  :symbol
    :string            :any
    ;; Nums
    :number            :any
    :long              :number
    :double            :number
    :int               :number
    :nat-int           :int
    ;; Colls
    :coll              :any
    :seqable           :coll
    :seq               :seqable
    :sequential        :seqable
    :list              :sequential
    :vector            :sequential
    :set               :seqable
    :map               :seqable
    ;; Experiment zone
    :fn                :any
    ;; Feel free to add more, loops are forbidden.
    }
   (reduce-kv (fn [h type parent-type] (derive h type parent-type))
              (make-hierarchy))))

;; if a type is not in there , consider it a child of :any

(def predicates
  {:any               any?
   :nil               nil?
   ;; ----
   :boolean           boolean?
   :keyword           keyword?
   :qualified-keyword qualified-keyword?
   :symbol            symbol?
   :qualified-symbol  qualified-symbol?
   :string            string?
   ;; Nums
   :number            number?
   :double            double?
   :int               int?
   :nat-int           nat-int?
   ;; Colls
   :coll              coll?
   :seqable           seqable?
   :seq               seq?
   :sequential        sequential?
   :list              list?
   :vector            vector?
   :set               set?
   :map               map?
   ;; Experiment zone
   :fn                fn? ;; not `ifn?`!
   ;; Feel free to add more, but keep it injective.
   })

;; if a value is of some type but we don’t have a predicate for it, tries with `instance?`

(defn matching-types
  "Return all possible types of a given value `x` in O(logₙ) time.
  Walk the type hierarchy from top to bottom, exploring branches and collecting
  matching types."
  ([h preds x]
   (matching-types h preds :any x))
  ([h preds type x]
   (matching-types h preds type #{} x))
  ([h preds type acc x]
   (let [pred? (get predicates type)]
     (if (and pred? (pred? x)) ;; if we’ve got a matching predicate for this type
       (into (conj acc type) ;; collect this type
             ;; recurse on children types
             (mapcat #(matching-types h preds % acc x) (children h type)))
       ;; stop exploring this branch
       acc))))

(defn shrink
  "Given a set of `types`, return a set of the most precise ones."
  ([h] (fn [types] (shrink h types)))
  ([h types]
   (shrink h :any types))
  ([h type types]
   (shrink h #{} type types))
  ([h acc type types]
   (let [children (set/intersection types (descendants h type))]
     (if (empty? children)
       (conj acc type)
       (into acc (mapcat #(shrink h acc % types) children))))))

(defn node
  ([parent type]
   (node parent type {}))
  ([parent type defaults]
   (with-meta (merge {::type type} defaults)
     {:parent parent})))

(defn new-or [n]
  (let [type   (::type n)
        parent (:parent (meta n))]
    (node parent :or {:child {type n}})))

(declare merge-types)

(defn type-of [h preds v] (first (shrink h (matching-types h preds v))))

(defn merge-keys [ta tb]
  (let [{keys-a ::keys} ta
        {keys-b ::keys} tb
        keys            (set/union keys-a keys-b)
        req             (set/intersection keys-a keys-b)]
    ;; keep tb’s metas
    (merge tb {::keys keys
               :req   req
               :opt   (set/difference keys req)})))

(defn merge-types [parent a b]
  (let [ta (::type a)
        tb (::type b)]
    (match [parent ta   tb]
           [:or    :or-branches  _ ] (let [parent-b (:parent (meta b))]
                                       (merge-types parent {::type :or, :child a} {::type parent-b, :child b}))
           [_      nil nil]              nil
           [_       _  nil]              a
           [_      nil  _ ]              b
           [_      :keys :keys]          (merge-keys a b)
           [_      :or :or]           (assoc a :child (merge-with (partial merge-types parent) (:child a) (:child b)))
           [_      :or  _ ]           (merge-types parent a (new-or b))
           [_       _  :or]           (merge-types parent (new-or a) b)
           [_       _   _ ]           (cond
                                        (= a b)   a
                                        (= ta tb) (update a :child (partial merge-types parent) (:child b))
                                        :else     (merge-types parent (new-or a) (new-or b))))))

;; Expected output
;; {:type  :vector
;;  :child {:type  :or
;;          :child {:vector {:type :vector}
;;                  :set    {:type :set}}}}

;; Reductions
;; Step:
;; 1. {}
;; 2. {:type :vector}
;; 3. {:type :vector
;;     :child {:type :vector}}
;; 4  {:type :vector
;;     :child {:type :vector
;;             :child {:type :integer}}}
;; 5. {:type :vector
;;     :child {:type :or
;;             :child {:vector {:type :vector
;;                              :child {:type :integer}}
;;                     :set    {:type :set}}}}
;; 6  {:type :vector
;;     :child {:type :or
;;             :child {:vector {:type  :vector
;;                              :child {:type :integer}}
;;                     :set    {:type  :set
;;                              :child {:type :integer}}}}}

(defn merge-in
  ([h] (fn [m path v] (merge-in h m path v)))
  ([h m path v]
   (let [parent (::type (get-in m (butlast path)))]
     (update-in m path (partial merge-types parent) v))))

(defn qualified? [x]
  (some-> x namespace))

(def simple? (complement qualified?))

(def ident? (some-fn keyword? symbol?))

(defn keys? [x]
  (and (map? x)
       (every? ident? (keys x))))

(defn down [path]
  (conj path :child))

(defn qualify [qualifierf x]
  (if qualifierf
    (or (qualifierf x) x)
    x))

(defn infer
  ([v]
   (infer v {}))
  ([v opts]
   (infer *hierarchy* predicates v opts))
  ([h ps v opts]
   (infer h ps v [:child] {::type :root, :keys {}} nil (or opts {})))
  ([h ps v path cache parent opts]
   (cond
     (or
      (set? v)
      (sequential? v)) (let [seq-type (type-of h ps v)]
                         (if (empty? v)
                           (merge-in h cache path (node parent seq-type))
                           (reduce (fn [cache v]
                                     (infer h ps v (down path) cache seq-type opts))
                                   (merge-in h cache path (node parent seq-type))
                                   v)))
     (keys? v)         (let [qualify (partial qualify (:qualify opts))]
                         (reduce (fn [cache [k v]]
                                   (if (qualified? k)
                                     (infer h ps v [:keys k] cache :keys opts)
                                     (infer h ps v [:keys (qualify k)] cache :keys opts)))
                                 (merge-in h cache path (node parent :keys {::keys (->> (keys v)
                                                                                        (map qualify)
                                                                                        (filter qualified?)
                                                                                        (set))}))
                                 v))
     (map? v)          (as-> cache cache
                         (merge-in h cache path (node parent :map))
                         (infer h ps (keys v) (conj path :keys) cache :map opts)
                         (infer h ps (vals v) (conj path :vals) cache :map opts)
                         (update-in cache path #(-> % (update :keys :sequential) ;; unnest $KeySeq and $ValSeq
                                                    (update :vals :sequential))))
     :else             (merge-in h cache path (node parent (type-of h ps v))))))
