(ns hyperfiddle.directory.core
  (:require
    [cats.monad.either :as either]))


(defn fiddle-dbname+ [datomic-record]
  (if-let [fiddledb-dbid (get-in datomic-record [:domain/fiddle-database :db/id])]
    (or (some #(when (= fiddledb-dbid (get-in % [:domain.database/record :db/id]))
                 (either/right (:domain.database/name %)))
              (:domain/databases datomic-record))
        (either/left (ex-info "Invalid :domain/fiddle-database. Must use a :domain/database"
                              (select-keys datomic-record [:domain/ident]))))
    (either/left (ex-info "Invalid :domain/fiddle-database. Missing :db/id"
                          (select-keys datomic-record [:domain/ident :domain/fiddle-database])))))
