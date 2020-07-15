(ns contrib.datomic-tx
  (:require
    [clojure.walk :as walk]
    [clojure.set :as set]
    [contrib.data :refer [update-existing update-in-existing deep-merge dissoc-nils]]
    [contrib.datomic.schema :refer [ref? identity? component?] :as schema]
    [contrib.datomic :refer [tempid? ref-one? ref-many? scalar-one? scalar-many? isComponent unique]]))


(defn- retract-entity [schema tx-data e]
  (let [{:keys [tx child-entids orphaned-parent-check]}
        (reduce (fn [acc next-stmt]
                  (if (contains? #{:db/add :db/retract} (first next-stmt))
                    (let [[op' e' a' v'] next-stmt]
                      (cond
                        (= e e') (if (and (component? schema a') (tempid? v'))
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
  (let [e (or (:db/id m) (str (hash m) #_(gensym)))]                 ; gensym tempids are unstable for unit tests
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
                     :else (throw (ex-info "Flatten Map Statement | Attribute not defined as ref in schema" {:schema schema :attribute a}))))))))




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

(defn identifier->e
  [schema identifier]
  (or (:db/id identifier)
      (:tempid identifier)
      (->> identifier
           (filter (fn [[a v]] (identity? schema a)))
           first)))

(defn identifier
  [schema stmt]
  (cond
    (vector? stmt)
    (let [[_ e a v] stmt]
      (merge {}
        (cond
          (string? e)
          {:tempid e}

          (number? e)
          {:db/id e}

          (vector? e)
          (conj {} e))

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

(defmulti absorb
  (fn [schema identifier ideal stmt]
    (cond
      (map? stmt)
      :map

      (and (vector? stmt) (#{:db/add :db/retract :db/retractEntity} (first stmt)))
      (first stmt)

      :else :other)))

(defmethod absorb :map
  [schema id ideal stmt]
  (reduce
    (fn [[id ideal] [a v]]
      (absorb schema id ideal [:db/add nil a v]))
    [id ideal]
    stmt))

(defmethod absorb :db/add
  [schema id ideal [_ e a v :as tx]]
  [(merge id (identifier schema tx))
   (if (schema/many? schema a)
     (if (contains? (get ideal :-) a)
       (update-existing ideal :- dissoc a)
       (update-in ideal [:+ a]
                  (fn [coll]
                    (let [coll (or coll #{})]
                      (if (and (not (string? v)) (seqable? v) (not (map? v))
                               (not (and (vector? v) (= 2 (count v)) (schema/identity? schema (first v)))))
                        (into coll v)
                        (conj coll v))))))
     (if (contains? (get ideal :-) a)
       (if (= (get-in ideal [:- a]) v)
         (update-existing ideal :- dissoc a)
         (assoc-in ideal [:+ a] v))
       (assoc-in ideal [:+ a] v)))])

(defn retract-munge-value
  [schema v]
  (if (map? v)
    (let [e (identifier->e schema v)]
      (if (vector? e)
        (second e)
        e))
    v))

(defn- -disj
  [coll v]
  (cond
    (or (vector? coll) (list? coll) (seq? coll))
    (into (empty coll) (complement (partial = v)) coll)

    (set? coll) (disj coll v)))

(defmethod absorb :db/retract
  [schema identifier ideal [_ _ a v]]
  (let [v (retract-munge-value schema v)]
    [identifier
     (if (schema/many? schema a)
       (if (contains? (get-in ideal [:+ a]) v)
         (update-in ideal [:+ a] -disj v)
         (update-in ideal [:- a] conj v))
       (if (= v (get-in ideal [:+ a]))
         (-> ideal
             (update-existing :+ dissoc a))
         (-> ideal
             (update :- assoc a v))))]))

(defmethod absorb :db/retractEntity
  [schema identifier ideal _]
  [identifier (assoc ideal :retracted true)])

(defmethod absorb :other
  [schema identifier ideal tx]
  [identifier
   (update ideal :other #(conj (or % []) tx))])

(defn absorb-stmt
  [schema ideals stmt]
  (let [identifier (identifier schema stmt)]
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
                        (schema/many? schema a)
                        (empty? v))))))))


(defn deconstruct-ideal
  [schema identifier {adds :+ retracts :- :keys [retracted other] :as ideal}]
  (let [e (identifier->e schema identifier)]
    (if retracted
      [[:db/retractEntity e]]
      (into
       []
       (concat
        (map (fn [[a v]] [:db/add e a v]) adds)
        (->> retracts
             (mapcat
               (fn [[a v]]
                 (if (schema/many? schema a)
                   (map (fn [v] [a v]) v)
                   [[a v]])))
             (map (fn [[a v]] [:db/retract e a v])))
        other)))))

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
            (assoc acc a v))
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
  (mappify schema (deconstruct schema (construct schema (construct schema tx) more-statements))))

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
