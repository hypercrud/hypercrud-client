(ns contrib.datomic.schema)

(defn ref?
  [schema a]
  (= :db.type/ref (get-in schema [a :db/valueType :db/ident])))

(defn component?
  [schema a]
  (:db/isComponent (get schema a)))

(defn identity?
  [schema a]
  (= :db.unique/identity (get-in schema [a :db/unique :db/ident])))

(defn many?
  [schema a]
  (= :db.cardinality/many identity (get-in schema [a :db/cardinality :db/ident])))

(defn one?
  [schema a]
  (= :db.cardinality/one identity (get-in schema [a :db/cardinality :db/ident])))
