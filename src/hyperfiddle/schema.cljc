(ns hyperfiddle.schema
  (:require
    #?(:clj [datomic.api :as d])
    [cats.core :as cats]
    [cats.monad.either :as either]
    [cats.monad.exception :as exception]
    [contrib.datomic]
    [hyperfiddle.api :as hf]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.datomic.core]
    [promesa.core :as p]))


(defn hydrate-schemas [rt pid local-basis partitions]
  (let [dbnames  (-> (hf/databases (hf/domain rt))
                     keys
                     vec)
        requests (mapv (fn [dbname] `((query-schema ~dbname) ~pid))
                       dbnames)]
    (-> (io/hydrate-requests (hf/io rt) local-basis partitions requests)
        (p/then (fn [{:keys [pulled-trees]}]
                  (->> pulled-trees                         ; add local schemas from spec here?
                       (map (fn [pulled-tree+]
                              (-> (cats/fmap #(->> % (last) (mapv first) (contrib.datomic/indexed-schema)) pulled-tree+)
                                  (either/branch exception/failure exception/success))))
                       (zipmap dbnames)))))))

(defn- query-schema [dbname]
  #?(:clj (d/q '[:find (pull ?attr [*
                                    {:db/valueType [:db/ident]
                                     :db/cardinality [:db/ident]
                                     :db/unique [:db/ident]}])
                 :where [:db.part/db :db.install/attribute ?attr]]
               (hf/get-db dbname)
               {:limit -1})))

(defn -summon-schema-out-of-band
  "Hacks for namespace hyperfiddle.security which due to bootstrapping cannot be hydrated in-band.
  See https://github.com/hyperfiddle/hyperfiddle/issues/1003"
  [domain db-name $]
  (let [result ((hyperfiddle.io.datomic.core/qf (hf/databases domain)
                                                [[db-name nil]])
                {:query '[:find (pull ?attr [*
                                             {:db/valueType [:db/ident]
                                              :db/cardinality [:db/ident]
                                              :db/unique [:db/ident]}])
                          :where [:db.part/db :db.install/attribute ?attr]]
                 :args [$]
                 :limit -1
                 #_#_:offset nil})
        result (mapv first result)]                         ; Datomic Cloud doesn't have FindColl pull syntax
    (contrib.datomic/indexed-schema result)))
