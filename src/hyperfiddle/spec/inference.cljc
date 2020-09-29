(ns hyperfiddle.spec.inference
  (:refer-clojure :exclude [derive])
  (:require [clojure.core.match :refer [match]]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]))

;;; Type Hierarchy

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
    ;; Complex types
    :inst              :any
    :uuid              :any
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
   nil                any? ; default value / not found
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
   ;; Complex types
   :inst              inst?
   :uuid              uuid?
   ;; Experiment zone
   :fn                fn? ;; not `ifn?`!
   ;; Feel free to add more, but keep it injective.
   })

;; TODO? if a value is of some type but we don’t have a predicate for it, tries with `instance?`

(defn matching-types
  "Return all possible types of a given value `x` in O(logₙ) time.
  Walk the type hierarchy from top to bottom, exploring branches and collecting
  matching types."
  ([h preds x]
   (matching-types h preds :any x))
  ([h preds type x]
   (matching-types h preds type #{} x))
  ([h preds type acc x]
   (let [pred? (get preds type)]
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

(defn type-of [h preds v] (first (shrink h (matching-types h preds v))))

(def ident? (some-fn keyword? symbol?))

(defn keys? [x]
  (and (map? x)
       (seq (keys x))
       (every? ident? (keys x))))

(defn qualify [qualifierf x]
  (if qualifierf
    (or (qualifierf x) x)
    x))

(defn qualified? [x]
  (and (keyword? x)
       (some? (namespace x))))

(defn merge-keys [a b]
  (let [{keys-a :keyset} a
        {keys-b :keyset} b
        keyset           (set/union keys-a keys-b)
        req              (set/intersection keys-a keys-b)]
    (merge b {:keyset keyset
              :req    req
              :opt    (set/difference keyset req)})))

(defn merge-types [a b]
  (let [ta (:type a)
        tb (:type b)]
    (match [ta tb]
           [nil nil] nil
           [_   nil] a
           [nil   _] b
           [:or :or] (update a :branches (partial merge-with merge-types) (:branches b))
           [:or   _] (update a :branches (partial merge-with merge-types) {tb b})
           [_   :or] (merge-types b a)
           [:keys :keys] (merge-keys a b)
           :else (cond
                   (= a   b) a
                   (= ta tb) (update a :child merge-types (:child b))
                   :else     {:type     :or
                              :branches {ta a
                                         tb b}}))))

(defn infer*
  [h ps v reg opts]
  (cond
    (or
     (set? v)
     (sequential? v)) (let [seq-type    (type-of h ps v)
                            [child reg] (reduce (fn [[a reg] x]
                                                  (let [[b reg'] (infer* h ps x reg opts)]
                                                    [(merge-types a b) (merge reg reg')]))
                                                [nil reg]
                                                v)]
                        [(cond-> {:type seq-type}
                           child (assoc :child child))
                         reg])
    (keys? v)         (let [qualify (partial qualify (:qualify opts))
                            ks      (->> (keys v) (map qualify) (filter qualified?) (set))]
                        [{:type   :keys
                          :keyset ks}
                         (reduce (fn [reg k]
                                   (let [[t reg'] (infer* h ps (get v k) reg opts)]
                                     (merge-with merge-types reg' {k t})))
                                 reg
                                 ks)])
    (map? v)          (if (seq v)
                        (let [[ks reg-a] (infer* h ps (keys v) reg opts)
                              [vs reg-b] (infer* h ps (vals v) reg opts)]
                          [{:type :map
                            :keys (:child ks)
                            :vals (:child vs)}
                           (merge reg-a reg-b)])
                        ;; empty map
                        [{:type :map} reg])
    :else             [{:type (type-of h ps v)} reg]))

(defn infer
  ([v]
   (infer v {}))
  ([v opts]
   (infer *hierarchy* predicates v opts))
  ([h ps v]
   (infer h ps v {}))
  ([h ps v opts]
   (infer* h ps v {} (or opts {}))))


(def render-type nil)
(defmulti render-type (fn [_predicates node] (:type node)) :hierarchy #'*hierarchy*)

(defmethod render-type :default [ps {:keys [type]}]
  (get ps type))

(defmethod render-type :map [ps {:keys [keys vals]}]
  `(s/map-of ~(render-type ps keys) ~(render-type ps vals)))

(defmethod render-type :coll [ps {:keys [child type]}]
  `(s/coll-of ~(render-type ps child) :kind ~(get ps type)))

(defmethod render-type :keys [ps {:keys [req opt]}]
  `(s/keys ~@(cond-> []
               (seq req) (conj :req (vec req))
               (seq opt) (conj :opt (vec opt)))))

(defmethod render-type :or [ps {:keys [branches]}]
  (let [branches (->> branches
                      (mapcat (fn [[k v]]
                                [k (render-type ps v)])))]
    `(s/or ~@branches)))

(defn render
  ([[ast registry]]
   (render *hierarchy* predicates [ast registry]))
  ([h ps [ast registry]]
   (binding [*hierarchy* h]
     (let [defs (reduce-kv (fn [acc name ast]
                             (conj acc `(s/def ~name ~(render-type ps ast))))
                           () registry)
           form (concat defs (list (render-type ps ast)))]
       (if (> (count form) 1)
         `(do ~@form)
         (first form))))))

