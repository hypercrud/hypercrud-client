(ns contrib.datomic-tx
  (:require
    [clojure.walk :as walk]
    [clojure.set :as set]
    [clojure.spec.alpha :as s]
    [contrib.data :refer [update-existing update-in-existing deep-merge dissoc-nils]]
    [contrib.datomic :refer [tempid? ref-one? ref-many? scalar-one? scalar-many? isComponent unique identity? one? many? ref?]]))


(defn- retract-entity [schema tx-data e]
  (let [{:keys [tx child-entids orphaned-parent-check]}
        (reduce (fn [acc next-stmt]
                  (if (contains? #{:db/add :db/retract} (first next-stmt))
                    (let [[op' e' a' v'] next-stmt]
                      (cond
                        (= e e') (if (and (isComponent schema a') (tempid? v'))
                                   (update acc :child-entids conj v')
                                   acc)
                        (= e v') (if (tempid? e')
                                   (update acc :orphaned-parent-check conj e')
                                   acc)
                        :else (update acc :tx conj next-stmt)))
                    (update acc :tx conj next-stmt)))
                {:tx []
                 :child-entids []
                 :orphaned-parent-check []}
                tx-data)
        tx (loop [[entid & rest] orphaned-parent-check
                  tx tx]
             (if entid
               (if (some #(and (= :db/add (first %)) (= entid (second %))) tx)
                 (recur rest tx)                            ; entid used in entity position in statements
                 (let [{:keys [tx orphaned-parent-check]}
                       (reduce (fn [acc next-stmt]
                                 (if (and (= :db/add (first next-stmt)) (= entid (last next-stmt)))
                                   (update acc :orphaned-parent-check conj (second next-stmt))
                                   (update acc :tx conj next-stmt)))
                               {:tx []
                                :orphaned-parent-check []}
                               tx)]
                   (recur (concat rest orphaned-parent-check) tx)))
               tx))]
    (loop [[entid & rest] child-entids
           tx tx]
      (if entid
        (recur rest (retract-entity schema tx entid))
        tx))))


(defn ^:export find-datom "not a good abstraction" [tx e-needle a-needle]
  (let [[[_ _ _ v]] (->> tx (filter (fn [[op e a v]]
                                      (= [op e a] [:db/add e-needle a-needle]))))]
    v))

(declare stmt->identifier, identifier->e)

(declare flatten-map-stmt)

(defn flatten-ref-stmt
  [schema e a v]
  (if (map? v)
    (let [e' (or (:db/id v)              ; {:person/name "Earnest"}
                 (str (hash v)))]
      (if (or (isComponent schema a)
              (some (partial unique schema) (keys v)))
        (concat (flatten-map-stmt schema (assoc v :db/id e'))
                [[:db/add e a e']])
        (throw (ex-info "Nested Map Transactions must either be a component, or have a unique attribute" {:attribute a}))))
    [[:db/add e a v]]))

(defn flatten-map-stmt
  "Flatten a single Datomic map-form statement into equivalent vector-form statements. Recursive. See test case."
  [schema m]
  (let [e (stmt->identifier schema m)
        e (:db/id e (:tempid e (str (hash m))))]
    (->> (seq m)                                            ; iterate as seq for compatibility with #Entity
         (filter (fn [[a v]] (not= :db/id a)))              ; db/id is virtual attribute, not a statement
         ; Don't need to suupport (id, ident, lookup-ref) because transactor will unify the tempid
         (mapcat (fn [[a v]]

                   (cond
                     (ref-one? schema a)                    ; :employee/manager
                     (flatten-ref-stmt schema e a v)

                     (ref-many? schema a)                   ; :person/siblings
                     (mapcat (partial flatten-ref-stmt schema e a) v)

                     (scalar-one? schema a)                 ; :person/name
                     [[:db/add e a v]]                      ; "Bob"

                     (scalar-many? schema a)                ; :person/liked-tags
                     (->> v                                 ; [:movies :ice-cream :clojure]
                          (mapv (fn [v]
                                  [:db/add e a v])))
                     :else (throw (ex-info "Flatten Map Statement | Attribute not defined in schema" {:schema schema :attribute a}))))))))




(defn flatten-tx
  "Normalize a Datomic transaction by flattening any map-form Datomic statements into tuple-form"
  [schema mixed-form-tx]
  (->> mixed-form-tx
       (mapcat (fn [stmt]
                 (if (map? stmt)                        ; map-form stmt expands to N vector-form stmts
                   (flatten-map-stmt schema stmt)
                   [stmt])))
       vec))

(defn filter-tx
  "run predicate on each stmt of tx, traversing map forms without altering them"
  [schema f? tx]
  (->> tx
       (flatten-tx schema)                                  ; flatten map-form stmts for targetted error messages on forbidden oeav
       (filter (fn [[o :as stmt]]
                 ; stmt can be map-form, or vector form [o e a v], or a function invocation
                 ; function calls share namespace with attributes, so whitelist approach still works
                 (f? stmt)))))

(defn remove-tx [schema f? tx]
  (filter-tx schema (complement f?) tx))

#?(:clj
   (defn expand-hf-tx
     [forms]
     (reduce
      (fn [acc [tag & args :as form]]
        (if (symbol? tag)
          (let [var (resolve tag)
                _ (assert var (str "Transaction symbol not found" tag))
                f @var
                _ (assert ifn? (str "Transaction symbol resolved to not function value"))]
                                        ; f returns a vector of datoms
            (into acc (apply f args)))
          (conj acc form)))
      []
      forms)))

(defmulti tx-meta
  (fn [schema tx]
    (if (map? tx)
      ::map
      (first tx))))

(s/def ::cardinality (s/or :one :many))
(s/def ::identifier map?)
(s/def ::inverse fn?)
(s/def ::special fn?)
(s/def ::special fn?)

(s/def ::transaction-meta
  (s/keys :req [::identifier]
          :opt [::cardinality ::inverse ::special ::conflicting?]))

(s/fdef tx-meta
  :ret ::transaction-meta)

(defn val->identifier
  [e]
  (cond
    (string? e)
    {:tempid e}

    (number? e)
    {:db/id e}

    (keyword? e)
    {:db/ident e}

    (vector? e)
    (conj {} e)))

(defn stmt->identifier
  [schema stmt]
  (cond
    (and (vector? stmt) (#{:db/add :db/retract :db/cas :db/retractEntity} (first stmt)))
    (let [[_ e a v] stmt]
      (merge {}
        (val->identifier e)

        (when (identity? schema a)
          {a v})))

    (map? stmt)
    (let [idents (into {} (filter (fn [[a v]] (identity? schema a)) stmt))
          {id :db/id tempid :tempid} stmt]
      (merge
       idents
       (when (or tempid (string? id))
         {:tempid (or tempid id)})
       (when (number? id)
         {:db/id id})
       (when (vector? id)
         (conj {} id))))))

(def identifier (comp ::identifier tx-meta))
(def cardinality (comp ::cardinality tx-meta))
(def inverse (comp ::inverse tx-meta))
(def attribute (comp ::attribute tx-meta))

(defmethod tx-meta :db/add
  [schema [f e a v :as tx]]
  {::inverse [:db/retract e a v]
   ::cardinality (if (one? schema a)
                   :one
                   :many)
   ::identifier (stmt->identifier schema tx)
   ::conflicting? (fn [[tx-fn' _ a' :as tx']]
                    (and (= :db/add tx-fn')
                         (= a a')))})

(defmethod tx-meta :db/retract
  [schema [f e a v :as tx]]
  {::inverse [:db/add e a v]
   ::cardinality (if (one? schema a)
                   :one
                   :many)
   ::identifier (stmt->identifier schema tx)
   ::conflicting? (fn [[tx-fn' _ a' :as tx']]
                    (and (= tx-fn' :db/retract)
                         (= a a')))})

(defmethod tx-meta :db/cas
  [schema [_ e a o n]]
  {::inverse [:db/cas e a n o]
   ::cardinality :one
   ::identifier (val->identifier e)
   ::conflicting? (fn [[tx-fn' _ a' :as tx']]
                    (and (#{:db/add :db/retract} tx-fn')
                         (= a a')))})

(defmethod tx-meta :db/retractEntity
  [schema [_ e]]
  (let [ident (val->identifier e)]
    {::cardinality :one
     ::identifier ident
     ::conflicting? (constantly true)}))

(defmethod tx-meta :default
  [schema tx]
  nil)

(defn identifier->e
  [schema identifier]
  (assert (not (empty? identifier)))
  (or (:db/id identifier)
      (:tempid identifier)
      (->> identifier
           (filter (fn [[a v]] (identity? schema a)))
           first)))

(defn unified-identifier
  [id0 id1]
  (let [ks (set/intersection (set (keys id0)) (set (keys id1)))
        unified-ks (filter #(= (get id0 %) (get id1 %)) ks)]
    (if (empty? unified-ks)
      nil
      (select-keys id0 unified-ks))))

(defn ideal-idx
  [identifier ideals]
  (filterv
    identity
    (mapv
      (fn [[id grp] i]
        (when (unified-identifier identifier id)
          i))
      ideals
      (range))))

(defn absorb
  [schema identifier ideal tx]
  (if (map? tx)
    (reduce
      (fn [[identifier ideal] [a v]]
        (absorb schema identifier ideal [:db/add (identifier->e schema identifier) a v]))
      [identifier ideal]
      tx)
    (let [{:keys [::inverse ::cardinality ::conflicting? ::special]
           :as meta} (tx-meta schema tx)
          cardinality (or cardinality (if conflicting? :one :many))
          conflicting? (or conflicting? (constantly false))
          ideal (or ideal #{})]
      [(merge identifier (::identifier meta))
       (if special
         (set (special ideal))
         (if (contains? ideal inverse)
           (disj ideal inverse)
           (apply
             (fn [txs]
               (if (= :one cardinality)
                 (conj
                   (into #{} (remove conflicting? txs))
                   tx)
                 (conj txs tx)))
             [ideal])))])))

(defn absorb-stmt
  [schema ideals stmt]
  (let [identifier (stmt->identifier schema stmt)]
    (if-let [idx (first (ideal-idx identifier ideals))]
      (update ideals idx (fn [[identifier ideal]] (absorb schema identifier ideal stmt)))
      (conj ideals (absorb schema identifier nil stmt)))))

(defn construct
  ([schema tx]
   (construct schema [] tx))
  ([schema ideals tx]
   (reduce (partial absorb-stmt schema) ideals tx)))

(defn invalid?
  [schema tx]
  (or (nil? tx)
      (and (map? tx) (<= (count tx) 1))
      (and (vector? tx)
           (or
             (= :db/id (nth tx 2 nil))
             (let [[txfn e a v] tx]
               (or (= e [a v])
                   (and (= :db/add txfn)
                        (many? schema a)
                        (coll? v)
                        (empty? v))))))))


(defn deconstruct-ideal
  [schema identifier ideal]
  (let [e (identifier->e schema identifier)]
    (into
      (mapv #(assoc % 1 e) (filter (fn [[tx-fn]] (= :db/add tx-fn)) ideal))
      (remove (fn [[tx-fn]] (= :db/add tx-fn)) ideal))))

(defn ideals->tx
  [schema ideals]
  (->>
    ideals
    (map (partial apply deconstruct-ideal schema))
    (reduce into [])
    (filterv (complement (partial invalid? schema)))))

(defn remove-dangling-ids
  [schema tx]
  (let [called-tempids (set (filter string? (map second tx)))]
    (vec
     (remove
      (fn [[_ e a v :as stmt]]
        (and (ref? schema a)
             (string? v)
             (not (contains? called-tempids v))))
      tx))))

(defn deconstruct
  [schema ideals]
  (->> ideals
       (ideals->tx schema)
       (remove-dangling-ids schema)))

(defn mappify-add-statements
  [schema adds]
  (let [add-groups (group-by second adds)]
    (mapv
      (fn [[id group]]
        (reduce
          (fn [acc [_ _ a v]]
            (if (ref-many? schema a)
              (update acc a (fn [x] (conj (or x #{}) v)))
              (assoc acc a v)))
          (cond
            (or (string? id) (number? id)) {:db/id id}

            (vector? id) (let [[a v] id]
                           (if (= a :db/id)
                             {:db/id v}
                             {:db/id [a v]})))
          group))
      add-groups)))

(defn mappify
  [schema tx]
  (reduce
    into
    []
    (vals (update (group-by first tx) :db/add (partial mappify-add-statements schema)))))

(defn into-tx [schema tx more-statements]
  (mappify schema (deconstruct schema (construct schema (construct schema (flatten-tx schema tx)) more-statements))))

(comment
 [[:db/add "a" :person/name "Alice"
   {:person/name "Bob"
    :person/parents [{:person/name "Cindy"}
                     {:person/name "David"}]}
   [:db/retract 42 :person/name "Ernie"]
   [:db/retract [:person/name "Ernie"] :person/name "Ernie"]
   [:db/retractEntity 43]
   [:db/retractEntity [:person/name "Fred"]]
   [:db/cas [:person/name "Gandalf"] :person/age 22 23]
   [:user.fn/foo 'x 'y 'z 'q 'r]
   [:esub.fn/sign-up-by-uuid (:db/id *user*) (java.util.UUID/fromString id)]
   [:sub.fn/ensure-sub-status (:db/id *user*)]]


  [[:db/add :db/retractEntity :hyperfiddle/whitelist-attribute true]
   [:db/add :db/cas :hyperfiddle/whitelist-attribute true]]])



;(defn ^:legacy entity-components [schema entity]
;  (mapcat (fn [[attr v]]
;            (let [{:keys [:db/cardinality :db/valueType :db/isComponent]} (get schema attr)]
;              (if isComponent
;                (case [(:db/ident valueType) (:db/ident cardinality)]
;                  [:db.type/ref :db.cardinality/one] [v]
;                  [:db.type/ref :db.cardinality/many] (vec v)
;                  []))))
;          entity))
;
;; pulled-tree->statements, respect component
;(defn ^:legacy entity-and-components->statements [schema e]
;  ; tree-seq lets us get component entities too
;  (->> (tree-seq map? #(entity-components schema %) e)
;       (mapcat entity->statements)))
