(ns hyperfiddle.transaction
  (:require
    [clojure.set :as set]
    [contrib.datomic :refer [tempid? ref-one? ref-many? scalar-one? scalar-many? isComponent unique identity? one? many? ref?]]
    #?(:clj [datomic.api :as d])                            ; illegal require
    [hyperfiddle.api :as hf]))

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
         (conj {:db/id id} id))))))


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
                     (if (coll? v)
                       (->> v                                 ; [:movies :ice-cream :clojure]
                            (mapv (fn [v]
                                    [:db/add e a v])))
                       [[:db/add e a v]])
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
                _ (assert var (str "Transaction symbol not found " tag))
                f @var
                _ (assert ifn? (str "Transaction symbol resolved to non-function value"))]
                                        ; f returns a vector of datoms
            (into acc (apply f args)))
          (conj acc form)))
      []
      forms)))

(def identifier (comp ::hf/tx-identifier hf/tx-meta))
(def cardinality (comp ::hf/tx-cardinality hf/tx-meta))
(def inverse (comp ::hf/tx-inverse hf/tx-meta))
(defn conflicting?
  [schema tx0 tx1]
  (let [c0? (::hf/tx-conflicting? (hf/tx-meta schema tx0) (constantly false))
        c1? (::hf/tx-conflicting? (hf/tx-meta schema tx1) (constantly false))]
    (or (c0? tx1) (c1? tx0))))

(defmethod hf/tx-meta :db/add
  [schema [f e a v :as tx]]
  {::hf/tx-inverse [:db/retract e a v]
   ::hf/tx-cardinality (if (one? schema a)
                         ::hf/one
                         ::hf/many)
   ::hf/tx-identifier (stmt->identifier schema tx)
   ::hf/tx-conflicting? (fn [[tx-fn' _ a' :as tx']]
                          (and (= :db/add tx-fn')
                               (= a a')))})

(defmethod hf/tx-meta :db/retract
  [schema [f e a v :as tx]]
  {::hf/tx-inverse [:db/add e a v]
   ::hf/tx-cardinality (if (one? schema a)
                         ::hf/one
                         ::hf/many)
   ::hf/tx-identifier (stmt->identifier schema tx)
   ::hf/tx-conflicting? (fn [[tx-fn' _ a' :as tx']]
                          (and (= tx-fn' :db/retract)
                               (= a a')))})

(defmethod hf/tx-meta :db/cas
  [schema [_ e a o n]]
  {::hf/tx-inverse [:db/cas e a n o]
   ::hf/tx-cardinality ::hf/one
   ::hf/tx-identifier (val->identifier e)
   ::hf/tx-conflicting? (fn [[tx-fn' _ a' :as tx']]
                          (and (#{:db/add :db/retract} tx-fn')
                               (= a a')))})

(defmethod hf/tx-meta :db/retractEntity
  [schema [_ e]]
  (let [ident (val->identifier e)]
    {::hf/tx-cardinality ::hf/one
     ::hf/tx-identifier ident
     ::hf/tx-conflicting? (constantly true)}))

(defmethod hf/tx-meta :default
  [schema tx]
  nil)

(defn identifier->e
  [schema identifier]
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
    (let [{:keys [::hf/tx-inverse ::hf/tx-cardinality ::hf/tx-conflicting? ::hf/tx-special ::hf/tx-identifier]
           :as meta} (hf/tx-meta schema tx)
          tx-cardinality (or tx-cardinality (if tx-conflicting? ::hf/one ::hf/many))
          tx-identifier (if (map? tx-identifier) tx-identifier (val->identifier tx-identifier))
          ideal (or ideal #{})]
      [(merge identifier tx-identifier)
       (if tx-special
         (set (tx-special ideal))
         (if (contains? ideal tx-inverse)
           (disj ideal tx-inverse)
           (apply
             (fn [txs]
               (if (= ::hf/one tx-cardinality)
                 (conj
                   (into #{} (remove (partial conflicting? schema tx) txs))
                   tx)
                 (conj txs tx)))
             [ideal])))])))

(defn absorb-stmt
  [schema ideals stmt]
  (let [identifier (identifier schema stmt)
        identifier (if (map? identifier) identifier (val->identifier identifier))]
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
      (fn [[tx-fn e a v :as stmt]]
        (and (#{:db/add :db/retract} tx-fn)
             (ref? schema a)
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
            (if (many? schema a)
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



; Wrapper for listening to transactions
; Likely to be replaced with an event stream in api
; so use sparringly

#?(:clj
    (defn datom->stmt
      "Convert a Datomic datom into a vector to be sent down to the client as a tx to be applied."
      [conn {:keys [e a v added] :as datom}]
      [(if added :db/add :db/retract) e (d/ident (d/db conn) a) v]))

#?(:clj
   (do
     (defprotocol ITransactionPublisher
       (subscribe [_ f])
       (unsubscribe [_ f])
       (close! [_]))

     (defn- ->tx-publisher*
       [conn]
       (let [subs (atom #{})
             queue (d/tx-report-queue conn)
             is-done (atom false)
             pub (reify ITransactionPublisher
                   (subscribe [_ f] (swap! subs conj f))
                   (unsubscribe [_ f] (swap! subs disj f))
                   (close! [_] (reset! is-done true)))]

         (.start
          (java.lang.Thread.
           (fn []
             (let [tx (.take queue)]
               (doseq [sub @subs]
                 (sub tx)))
             (when-not @is-done (recur)))))

         pub))

     (def ->tx-publisher (memoize ->tx-publisher*))))
