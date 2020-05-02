(ns hyperfiddle.security.entity-ownership
  (:require
    [contrib.datomic]
    [datomic.api :as d]
    [hyperfiddle.security :as security]
    [loom.alg :as alg]
    [loom.graph :as graph]))


(defn normalize-id [db ident]
  (if (contrib.datomic/tempid? ident)
    ident
    (or (d/entid db ident) ident)))

(defn process-ref-v [db inject-tempids-map' identity-ids component-ids
                     acc-map e a v]
  (if (map? v)
    ; nested map statement
    (let [child-acc-map (inject-tempids-map' v)
          v (:statement child-acc-map)]
      [v (-> acc-map
             (update :attr-ids into (:attr-ids child-acc-map))
             (update :identity-graph graph/graph (:identity-graph child-acc-map))
             (update :component-edges into (:component-edges child-acc-map))
             (cond->
               (contains? identity-ids a) (update :identity-graph graph/add-edges [e [a (:db/id v)]])
               (contains? component-ids a) (update :component-edges conj [e (:db/id v)])))])

    ; or just an id or lookup ref
    (let [v' (normalize-id db v)]
      [v (cond-> acc-map
           (contains? identity-ids a) (update :identity-graph graph/add-edges [e [a v']])
           (contains? component-ids a) (update :component-edges conj [e v']))])))

(defn inject-tempids-map [db stmt identity-ids component-ids ref-ids schema-ids]
  (let [top-hash (hash stmt)
        id-count (atom 0)
        inject-tempids-map' (fn inject-tempids-map' [stmt]
                              (let [[e stmt] (if-let [e (:db/id stmt)]
                                               [e stmt]
                                               (let [e (str "hyperfiddle.security." top-hash "." (swap! id-count inc))]
                                                 [e (assoc stmt :db/id e)]))
                                    e (normalize-id db e)]
                                (reduce (fn [acc-map [a v :as kv]]
                                          (let [a (normalize-id db a)]
                                            (-> (if (contains? ref-ids a)
                                                  (case (:db/cardinality (d/entity db a))
                                                    :db.cardinality/one
                                                    (let [[v acc-map] (process-ref-v db inject-tempids-map' identity-ids component-ids
                                                                                     acc-map e a v)]
                                                      (assoc-in acc-map [:statement (first kv)] v))

                                                    :db.cardinality/many
                                                    ; datomic is super lenient for card/many. v can be a map or an identity (NOT lookup-ref) or
                                                    ; a heterogeneous list of any combination of map, lookup ref, or identity
                                                    ; e.g using this statement as a test  `(d/with (db! root-uri) [{:db/id "-1" :fiddle/links 'v}])`
                                                    ; the following values for 'v are VALID:
                                                    ;   [{:db/id 17592186061399}]
                                                    ;   {:db/id 17592186061399}
                                                    ;   [17592186061399]
                                                    ;   17592186061399
                                                    ;   :db/add
                                                    ;   [{:fiddle/ident :databases}]
                                                    ;   {:fiddle/ident :databases}
                                                    ;   [[:fiddle/ident :databases]]
                                                    ;   [[[:db/ident :fiddle/ident] :databases]]  ; extra spicy double lookup ref
                                                    ; the following values are INVALID:
                                                    ;   [:fiddle/ident :databases]
                                                    ;   [[:db/ident :fiddle/ident] :databases]    ; extra spicy double lookup ref
                                                    (if (or (list? v) (vector? v) (seq? v) (set? v))
                                                      (reduce (fn [acc-map v]
                                                                (let [[v acc-map] (process-ref-v db inject-tempids-map' identity-ids component-ids
                                                                                                 acc-map e a v)]
                                                                  (update-in acc-map [:statement (first kv)] conj v)))
                                                              (assoc-in acc-map [:statement (first kv)] (empty v))
                                                              v)
                                                      (let [[v acc-map] (process-ref-v db inject-tempids-map' identity-ids component-ids
                                                                                       acc-map e a v)]
                                                        (assoc-in acc-map [:statement (first kv)] v))))

                                                  (cond-> (assoc-in acc-map [:statement (first kv)] v)
                                                    ; non-db.type/ref lookup ref
                                                    (contains? identity-ids a) (update :identity-graph graph/add-edges [e [a v]])))
                                                (cond->
                                                  (contains? schema-ids a) (update :attr-ids conj e)))))
                                        {:attr-ids #{}
                                         :identity-graph (graph/graph e)
                                         :component-edges #{}
                                         :statement {}}
                                        stmt)))]
    (inject-tempids-map' stmt)))

(defn inject-tempids [db tx]
  (let [identity-ids (into #{} (d/q '[:find [?e ...]
                                      :where
                                      [?e :db/ident]
                                      [?e :db/unique :db.unique/identity]]
                                    db))
        component-ids (into #{} (d/q '[:find [?e ...]
                                       :where
                                       [?e :db/ident]
                                       [?e :db/isComponent true]]
                                     db))
        ref-ids (into #{} (d/q '[:find [?e ...]
                                 :where
                                 [?e :db/ident]
                                 [?e :db/valueType :db.type/ref]]
                               db))
        schema-ids (->> [:db/cardinality :db/valueType]
                        (map #(d/entid db %))
                        (into #{}))
        acc (reduce (fn [{:keys [attr-ids component-edges identity-graph tx]} stmt]
                      (cond
                        (map? stmt) (let [sub (inject-tempids-map db stmt identity-ids component-ids ref-ids schema-ids)]
                                      {:attr-ids (into attr-ids (:attr-ids sub))
                                       :identity-graph (graph/graph identity-graph (:identity-graph sub))
                                       :component-edges (into component-edges (:component-edges sub))
                                       :tx (conj tx (:statement sub))})

                        (or (list? stmt) (vector? stmt))
                        (-> (let [op (:db/ident (d/pull db [:db/ident] (first stmt)))]
                              (condp contains? op
                                #{:db/add :db/retract}
                                (let [[_ e a v] stmt
                                      e (normalize-id db e)
                                      a (normalize-id db a)]
                                  {:attr-ids (cond-> attr-ids
                                               (contains? schema-ids a) (conj e))
                                   :identity-graph (if (contains? identity-ids a)
                                                     (graph/add-edges identity-graph [e [a v]])
                                                     (graph/add-nodes identity-graph e))
                                   :component-edges (if (contains? component-ids a)
                                                      (conj component-edges [e v])
                                                      component-edges)})

                                #{:db/retractEntity :db.fn/retractEntity}
                                {:attr-ids attr-ids
                                 :identity-graph (->> (second stmt)
                                                      (normalize-id db)
                                                      (graph/add-nodes identity-graph))
                                 :component-edges component-edges}

                                #{:db/cas :db.fn/cas}
                                (let [[_ e a ov nv] stmt
                                      e (normalize-id db e)
                                      a (normalize-id db a)]
                                  {:attr-ids (cond-> attr-ids
                                               (contains? schema-ids a) (conj e))
                                   :identity-graph (if (contains? identity-ids a)
                                                     (graph/add-edges identity-graph [e [a ov]] [e [a nv]])
                                                     (graph/add-nodes identity-graph e))
                                   :component-edges (if (contains? component-ids a)
                                                      (conj component-edges [e nv] [e ov])
                                                      component-edges)})
                                ; else
                                (throw (ex-info "Unrecognized operation" {:statement stmt}))))
                            (assoc :tx (conj tx stmt)))

                        :else (throw (ex-info "Unrecognized statement type" {:statement stmt}))))
                    {:attr-ids #{}
                     :identity-graph (graph/graph)
                     :component-edges #{}
                     :tx []}
                    tx)]
    {:attr-ids (:attr-ids acc)
     :component-graph (let [ig (:identity-graph acc)]
                        (->> (:component-edges acc)
                             (map (fn [[parent child]]
                                    [(set (alg/bf-traverse ig parent)) (set (alg/bf-traverse ig child))]))
                             (apply graph/digraph)))
     :identity-graph (:identity-graph acc)
     :tx-with-tempids (:tx acc)}))

(defn ->existing-eids [db eids]
  (->> eids
       (remove contrib.datomic/tempid?)
       (map (partial d/entid db))
       (remove nil?)                                        ; nil will be from new lookup refs
       (into #{})))

(defn validate-write-entity-ownership [config db tx]
  (let [supported-ops #{:db/add :db/retract :db/retractEntity :db.fn/retractEntity :db/cas :db.fn/cas}]
    (-> (->> tx
             (remove map?)
             (every? #(contains? supported-ops (first %))))
        (assert "user declared db functions not yet supported")))

  (let [{:keys [attr-ids component-graph identity-graph tx-with-tempids]} (inject-tempids db tx)]
    ; check permissions on existing entities
    (doseq [existing-eids (->> (alg/connected-components component-graph)
                               (mapcat (fn [list-of-sets-of-eids]
                                         ; only need one eid per identity
                                         (let [x (map first list-of-sets-of-eids)]
                                           (map vector x (drop 1 x)))))
                               (apply graph/add-edges identity-graph)
                               (alg/connected-components)
                               (map (partial ->existing-eids db)))
            :when (not (empty? existing-eids))]
      (when-not (apply (:can-merge? config) existing-eids)
        (throw (security/tx-validation-failure))))

    (let [db-part (fn [eids]
                    (cond
                      ; order is important, db.part/db wins when there are conflicts
                      (some #(contains? attr-ids %) eids) :db.part/db
                      (contains? (set eids) "datomic.tx") :db.part/tx
                      :else (let [[part & rest] (->> eids
                                                     (map #(normalize-id db %))
                                                     (filter integer?)
                                                     (map d/part)
                                                     (map #(d/ident db %))
                                                     distinct)]
                              (assert (empty? rest) "multiple partitions found")
                              (or part :db.part/user))))]
      (->> (alg/connected-components identity-graph)
           (mapcat (fn [eids]
                     (let [existing-eids (->existing-eids db eids)]
                       (when (empty? existing-eids)         ; only generate owner if ALL merged entities are new
                         (let [my-part (db-part eids)
                               parent-parts (->> (graph/in-edges component-graph (set eids))
                                                 (map (comp db-part first))
                                                 (distinct))]
                           (assert (#{0 1} (count parent-parts)) "Entity is component child of multiple parent entities across multiple partitions")
                           ; this entity is NOT a component child of another same partitioned new entity
                           (when (not= (first parent-parts) my-part)
                             ; doesn't matter which tempid has owner appended
                             (let [tempid (some #(when (contrib.datomic/tempid? %) %) eids)]
                               (assert tempid "no tempids or existing ids") ; this is probably a bad transaction; would fail with "Unable to resolve entity"
                               ((:generate-owner config) tempid my-part))))))))
           (into tx-with-tempids)))))

(defn build-parent-lookup [db]
  (let [component-ids (into #{} (d/q '[:find [?e ...]
                                       :where
                                       [?e :db/ident]
                                       [?e :db/isComponent true]]
                                     db))]
    (fn [e]
      (letfn [(component-parents [e]
                (let [parent-eids (->> (d/datoms db :vaet e)
                                       (filter #(contains? component-ids (.a %)))
                                       (map #(.e %)))]
                  ; todo solve infinite loops
                  (->> parent-eids
                       (mapcat (fn [eid]
                                 (let [parent-eids (component-parents eid)]
                                   (if (empty? parent-eids)
                                     [eid]
                                     parent-eids))))
                       (into #{}))))]
        (let [top-level-eids (component-parents e)]
          (assert (> 2 (count top-level-eids)) "Multiple parents found for component entity")
          (first top-level-eids))))))

(defn write-domains [hf-db subject tx]                      ; [$ domain dbname subject tx] todo
  (if (nil? subject)
    (throw (security/tx-validation-failure))
    (let [db (d/db (d/connect (str (:database/uri hf-db))))
          parent-lookup (build-parent-lookup db)
          root-subjects (-> (into #{} (:hyperfiddle/owners hf-db))
                            (conj security/root))
          owners (fn [eid]
                   (let [eid (or (parent-lookup eid) eid)
                         e (d/entity db eid)]
                     (into #{} (:hyperfiddle/owners e))))
          has-permissions? (if (contains? root-subjects subject)
                             (constantly true)
                             #(contains? % subject))
          config {:can-merge? (fn [eid & rest]
                                (let [first-owners (owners eid)]
                                  (and (has-permissions? first-owners)
                                       ; unless the perms are identical, the entities cannot be merged
                                       (every? #(= first-owners (owners %)) rest))))
                  :generate-owner (if (contains? root-subjects subject)
                                    (fn [e db-part]
                                      (when-not (contains? #{:db.part/db :db.part/tx} db-part)
                                        [[:db/add e :hyperfiddle/owners subject]]))
                                    (fn [e db-part]
                                      (when (contains? #{:db.part/db :db.part/tx} db-part)
                                        (throw (security/tx-validation-failure)))
                                      [[:db/add e :hyperfiddle/owners subject]]))}]
      (validate-write-entity-ownership config db tx))))

(defn write-entity-ownership [hf-db subject tx]
  (if (nil? subject)
    (throw (security/tx-validation-failure))
    (let [db (d/db (d/connect (str (:database/uri hf-db))))
          parent-lookup (build-parent-lookup db)
          root-subjects (-> (into #{} (:hyperfiddle/owners hf-db))
                            (conj security/root))
          owners (fn [eid]
                   (let [eid (or (parent-lookup eid) eid)
                         e (d/entity db eid)]
                     (into #{} (:hyperfiddle/owners e))))
          has-permissions? (if (contains? root-subjects subject)
                             (constantly true)
                             #(contains? % subject))
          config {:can-merge? (fn [eid & rest]
                                (let [first-owners (owners eid)]
                                  (and (has-permissions? first-owners)
                                       ; unless the perms are identical, the entities cannot be merged
                                       (every? #(= first-owners (owners %)) rest))))
                  :generate-owner (if (contains? root-subjects subject)
                                    (fn [e db-part]
                                      (when-not (contains? #{:db.part/tx} db-part)
                                        [[:db/add e :hyperfiddle/owners subject]]))
                                    (fn [e db-part]
                                      (when (contains? #{:db.part/tx} db-part)
                                        (throw (security/tx-validation-failure)))
                                      [[:db/add e :hyperfiddle/owners subject]]))}]
      (validate-write-entity-ownership config db tx))))
