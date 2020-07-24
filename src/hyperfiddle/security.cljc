(ns hyperfiddle.security
  (:require
    [cats.monad.either :refer [right left]]
    #?(:clj [hyperfiddle.io.datomic.core])
    #?(:clj [hyperfiddle.schema])
    [hyperfiddle.transaction :refer [remove-tx]]
    #?(:clj [contrib.pprint :refer [pprint-str pprint-datoms-str]])
    #?(:clj [taoensso.timbre :as timbre])
    [hyperfiddle.api :as hf])
  #?(:clj
     (:import
       (hypercrud.types.DbRef DbRef))))


(def root "hyperfiddle.security/root")                      ; todo uuid/real account

(defn tx-validation-failure [& {:as data-map}]
  (ex-info "user tx failed validation" (into {:hyperfiddle.io/http-status-code 403} data-map)))

#?(:clj
   (defmethod hf/process-tx :hyperfiddle.security/authenticated-users-only [$ domain dbname subject tx]
     (if (nil? subject)
       (throw (tx-validation-failure))
       tx)))

#?(:clj
   (defmethod hf/process-tx :hyperfiddle.security/owner-only [$ domain dbname subject tx]
     (if (-> (into #{root} (:hyperfiddle/owners (hf/database domain dbname)))
           (contains? subject))
       tx
       (throw (tx-validation-failure)))))

(defn tx-forbidden-stmts [schema whitelist tx]
  {:pre [(set? whitelist)]}
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
                       ((hyperfiddle.io.datomic.core/qf (hf/databases domain)
                                                        [(let [branch nil] ; qf ignores branch
                                                           (DbRef. db-name branch))])
                        (attr-whitelist-query $))
                       (catch Exception e
                         ; datomic.impl.Exceptions$IllegalArgumentExceptionInfo:
                         ; :db.error/not-an-entity Unable to resolve entity: :hyperfiddle/whitelist-attribute
                         nil))]
       (into (set whitelist)
             (:hf/transaction-operation-whitelist (hf/database domain db-name))))))

#?(:clj
   (defmethod hf/process-tx :hyperfiddle.security/tx-operation-whitelist [$ domain dbname subject tx]
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

(defn owned-by? [hf-db subject]
  (contains? (into #{} (:hyperfiddle/owners hf-db)) subject))

(defmethod hf/subject-may-transact+ :hyperfiddle.security/owner-only [hf-db subject] (if (owned-by? hf-db subject) (right) (left "Writes restricted")))
(defmethod hf/subject-may-create? :hyperfiddle.security/owner-only [ctx] (owned-by? (hf/db-record ctx) (hf/subject ctx)))
(defmethod hf/subject-may-edit-entity? :hyperfiddle.security/owner-only [ctx] (owned-by? (hf/db-record ctx) (hf/subject ctx)))

(defmethod hf/subject-may-transact+ :hyperfiddle.security/authenticated-users-only [hf-db subject] (if (some? subject) (right) (left "Please login")))
(defmethod hf/subject-may-create? :hyperfiddle.security/authenticated-users-only [ctx] (some? (hf/subject ctx)))
(defmethod hf/subject-may-edit-entity? :hyperfiddle.security/authenticated-users-only [ctx] (some? (hf/subject ctx)))

(defmethod hf/subject-may-transact+ :hyperfiddle.security/tx-operation-whitelist [hf-db subject] (right))
(defmethod hf/subject-may-create? :hyperfiddle.security/tx-operation-whitelist [ctx] false) ; ??? busted?
(defmethod hf/subject-may-edit-entity? :hyperfiddle.security/tx-operation-whitelist [ctx] true)
