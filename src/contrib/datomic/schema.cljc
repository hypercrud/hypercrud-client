(ns contrib.datomic.schema)

(defn ref?
  [schema a]
  (= :db.type/ref
     (let [unique (get-in schema [a :db/valueType])]
       (if (map? unique)
         (get unique :db/ident)
         unique))))

(defn component?
  [schema a]
  (:db/isComponent (get schema a)))

(defn identity?
  [schema a]
  (or (= :db/id a)
      (= :db.unique/identity
         (let [unique (get-in schema [a :db/unique])]
           (if (map? unique)
             (get unique :db/ident)
             unique)))))

(defn many?
  [schema a]
  (= :db.cardinality/many
     (let [cardinality (get-in schema [a :db/cardinality])]
       (if (map? cardinality)
         (get cardinality :db/ident)
         cardinality))))

(defn one?
  [schema a]
  (= :db.cardinality/one
     (let [cardinality (get-in schema [a :db/cardinality])]
       (if (map? cardinality)
         (get cardinality :db/ident)
         cardinality))))
