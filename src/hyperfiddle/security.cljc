(ns hyperfiddle.security
  (:require
    #?(:clj [hyperfiddle.io.datomic.core])
    #?(:clj [hyperfiddle.domain])
    #?(:clj [hyperfiddle.schema])
    [contrib.datomic-tx :refer [remove-tx]]
    [contrib.pprint :refer [pprint-str pprint-datoms-str]]
    [taoensso.timbre :as timbre]
    [hyperfiddle.domain :as domain])
  #?(:clj
     (:import
       (hypercrud.types.DbRef DbRef))))


(def root "hyperfiddle.security/root")                      ; todo uuid/real account

(defn tx-validation-failure [& {:as data-map}]
  (ex-info "user tx failed validation" (into {:hyperfiddle.io/http-status-code 403} data-map)))

(defn write-allow-anonymous [$ domain dbname subject tx]
  tx)

(defn write-authenticated-users-only [$ domain dbname subject tx]
  (if (nil? subject)
    (throw (tx-validation-failure))
    tx))

(defn write-owner-only [$ domain dbname subject tx]
  (if (-> (into #{root} (:hyperfiddle/owners (hyperfiddle.domain/database domain dbname)))
          (contains? subject))
    tx
    (throw (tx-validation-failure))))

(defn tx-forbidden-stmts [schema whitelist tx]
  (->> tx
       (remove-tx schema                                    ; handles map-form transactions and tx fns
                  (fn [[o & [e a v :as args] :as stmt]]
                    (condp some [o]
                      #{:db/add :db/retract} (contains? whitelist a) ; typical edits are secured by attribute
                      (contains? whitelist o))))))          ; transaction functions are secured by txfn name

(defn attr-whitelist-query [$]
  {:query '[:find [?ident ...] :where
            [?attr :db/ident ?ident]                        ; transactor functions have idents
            #_[:db.part/db :db.install/attribute ?attr]     ; transactor functions are not installed
            [?attr :hyperfiddle/whitelist-attribute true]]  ; If hf attr not installed, this clause throws
   :args [$]
   :limit -1
   #_#_:offset nil})

#?(:clj
   (defn whitelist [$                                       ; Already has the proper staging area applied (one prior to the tx currently being validated)
                    domain db-name]                         ; spaghetti params used to decide which Datomic native q function to call
     {:pre [$ domain db-name]}
     ; TODO this is IO for non-local datomic configurations
     (let [whitelist (try
                       ((hyperfiddle.io.datomic.core/qf (domain/databases domain)
                                                        [(let [branch nil] ; qf ignores branch
                                                           (DbRef. db-name branch))])
                        (attr-whitelist-query $))
                       (catch Exception e
                         ; datomic.impl.Exceptions$IllegalArgumentExceptionInfo:
                         ; :db.error/not-an-entity Unable to resolve entity: :hyperfiddle/whitelist-attribute
                         nil))]
       (into (set whitelist)
             (:hf/transaction-operation-whitelist (domain/database domain db-name))))))

#?(:clj
   (defn tx-operation-whitelist! [$ domain dbname subject tx]
     (let [whitelist (whitelist $ domain dbname)
           schema (hyperfiddle.schema/-summon-schema-out-of-band domain dbname $)
           forbidden-stmts (tx-forbidden-stmts schema whitelist tx)]
       (timbre/debug "forbidden statements: " forbidden-stmts)
       (if (empty? forbidden-stmts)
         tx
         (throw (let [msg (str "Forbidden statements: \n\n"
                               (pprint-datoms-str forbidden-stmts) "\n\n"
                               "are forbidden due to transaction whitelist: \n\n"
                               (pprint-str whitelist))]
                  (ex-info msg {}  #_{:hf/anomoly msg
                                      :hf/forbidden-statements forbidden-stmts
                                      :hf/transaction-whitelist whitelist})))))))
