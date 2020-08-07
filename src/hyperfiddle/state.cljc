(ns hyperfiddle.state
  (:require
    [clojure.spec.alpha :as s]
    [contrib.data :refer [map-values update-existing for-kv]]
    [hyperfiddle.transaction :as tx]
    [contrib.reducers :as reducers]
    [contrib.pprint :refer [pprint-str]]
    [clojure.set :refer [superset? difference intersection]]
    [hypercrud.types.Err :refer [->Err]]
    [hyperfiddle.api :as hf]
    [hyperfiddle.route]                                     ; spec validation
    [taoensso.timbre :as timbre]))


(defn- serializable-error [e]
  ; need errors to be serializable, so crapily pr-str
  (let [?message (ex-message e)]
    (cond
      (string? e) e
      ?message (assoc (->Err (str ?message)) :data (pprint-str (ex-data e)))
      :else (pr-str e))))

(defn global-basis-reducer [global-basis action & args]
  (case action
    :set-global-basis (first args)
    :set-global-user-basis (assoc global-basis :user (first args))
    global-basis))

; todo this is duplicate logic from hf.rt
(defn- get-schema+ [partitions pid dbname]
  {:pre [pid]}
  (if (get-in partitions [pid :is-branched])
    (get-in partitions [pid :schemas dbname])
    (get-schema+ partitions (get-in partitions [pid :parent-pid]) dbname)))

(defn- with [partitions pid dbname tx]
  {:pre [pid dbname]}
  (if-let [schema+ (get-schema+ partitions pid dbname)]
    (update-in partitions [pid :stage dbname] (partial tx/into-tx @schema+) tx)
    (throw (ex-info "Missing schema" {:pid pid :dbname dbname}))))

(defn- build-hydrate-id [?partition]
  ;{:pre [partition]} ; can be nil lol
  (hash (select-keys ?partition [:route :pending-route :stage :local-basis])))

(declare partition-apply-defaults)

(defn delete-partition [partitions pid]
  (let [{:keys [parent-pid partition-children]} (get partitions pid)]
    ; https://github.com/hyperfiddle/hyperfiddle/issues/1022
    ; For partition children who have no other partitions pointing to it
    ; foreach children in partition-children
    ; if the child is unused by any other partitions' partition-children, delete it

    ;(map :partition-children (dissoc partitions pid))       ; other people's children, not mine
    ;
    ;(for-kv partition-children partitions
    ;  (fn [m cpid cp]
    ;    (if-not (get partitions cpid)                       ; people other than me
    ;      (delete-partition m cpid))))

    ; Leaving the broken impl, worked around by making hf-def PIDs unique:
    (-> (reduce delete-partition partitions partition-children) ; recursively drop all children
      (dissoc pid)                                          ; drop this one
      (update-in [parent-pid :partition-children] disj pid)))) ; remove ref to me

(defn partitions-interp [partitions ?action args]
  (case ?action
    :transact!-success (let [[pid transacted-dbnames] args]
                         (assert (some? (get partitions pid)))
                         (-> partitions
                           (assoc-in [pid :hydrate-id] "hack; dont flicker while page rebuilds")
                           (update-in [pid :stage] #(apply dissoc % (vec transacted-dbnames)))))

    :create-partition (let [[parent-pid child-pid is-branched] args]
                        (assert (some? (get partitions parent-pid)))
                        (-> partitions
                          (update-in [parent-pid :partition-children] conj child-pid)
                          (assoc child-pid {:is-branched is-branched
                                            :parent-pid parent-pid})))

    :new-hydrated-partition (let [[parent-pid child-pid partition] args]
                              (assert (some? (get partitions parent-pid)))
                              (-> partitions
                                (update-in [parent-pid :partition-children] conj child-pid)
                                (assoc child-pid (dissoc partition [:partition-children]))))

    :delete-partition (let [[pid] args]
                        (assert (some? (get partitions pid)))
                        (delete-partition partitions pid))

    :partition-basis (let [[pid local-basis] args]
                       (assert (some? (get partitions pid)))
                       (assoc-in partitions [pid :local-basis] local-basis))

    :partition-route (let [[pid route] args]
                       (assert (some? (get partitions pid)) "Must create-partition before setting route") ; todo move this assertion to runtime boundaries
                       (assoc-in partitions [pid :route] route))

    :partition-route-defaults (let [[pid route] args]
                                (assert (some? (get partitions pid)) "Must create-partition before setting route") ; see :partition-route
                                (assoc-in partitions [pid :route-defaults] route))

    :stage-route (let [[pid route] args]
                   (assert (some? (get partitions pid)) "Must create-partition before setting route") ; todo move this assertion to runtime boundaries
                   (assoc-in partitions [pid :pending-route] route))

    :with (let [[pid dbname tx] args]
            (assert (some? (get partitions pid)))
            (with partitions pid dbname tx))

    :merge (let [[parent-pid pid] args]
             (assert (some? (get partitions pid)))
             (assert (some? (get partitions parent-pid)))
             (-> (reduce-kv (fn [partitions dbname tx]
                              (with partitions parent-pid dbname tx))
                   partitions
                   (get-in partitions [pid :stage]))
               (delete-partition pid)))

    :hydrate!-start (let [[pid] args]
                      ;(assert (some? (get partitions pid))) ; this pid is not yet loaded? I think this assert is good
                      ; how can you hydrate!-start if the partition was not created? That's the bug
                      (assoc-in partitions [pid :hydrate-id] (build-hydrate-id nil))
                      #_(update partitions pid
                          (fn [partition]
                            (assert partition)
                            (assoc partition :hydrate-id (build-hydrate-id partition)))))

    :hydrate!-success (let [[pid ptm tempid-lookups] args]
                        (assert (some? (get partitions pid)))
                        (update partitions pid
                          (fn [partition]
                            (-> partition
                              (dissoc :error :hydrate-id)
                              (assoc :ptm ptm
                                     :tempid-lookups tempid-lookups)))))

    :hydrate!-route-success (let [[pid new-partition] args]
                              (assert (some? (get partitions pid)))
                              (update partitions pid
                                (fn [partition]
                                  (-> (into {} partition)
                                    (dissoc :error :hydrate-id)
                                    (into (dissoc new-partition [:partition-children]))))))

    :partition-error (let [[pid error] args]
                       (assert (some? (get partitions pid)))
                       (update partitions pid
                         (fn [partition]
                           (-> partition
                             (dissoc :hydrate-id)
                             (assoc :error (serializable-error error))))))

    :open-popover (let [[pid popover-id] args]
                    (assert (some? (get partitions pid)))
                    (update-in partitions [pid :popovers] conj popover-id))

    :close-popover (let [[pid popover-id] args]
                     (assert (some? (get partitions pid)))
                     (update-in partitions [pid :popovers] disj popover-id))


    :reset-stage-branch (let [[pid v] args]
                          (assert (some? (get partitions pid)))
                          (assoc-in partitions [pid :stage] v))

    :reset-stage-db (let [[pid dbname tx] args]
                      (assert (some? (get partitions pid)))
                      (assoc-in partitions [pid :stage dbname] tx))

    (or partitions {})))

(declare partition-validate)

(defn partitions-reducer [partitions ?action & args]
  ;(if ?action (println "partitions-reducer: " ?action (first args)))
  (let [partitions2 (partitions-interp partitions ?action args)]
    (->> partitions2
      (map (fn [[pid p]]
             (let [p' (partition-apply-defaults p)]
               [pid (partition-validate ?action pid p')])))
      (into {}))))

(defn partition-validate [?action pid updated-p]
  #_(println "pid: " pid " :partition-children: " (:partition-children updated-p) " partitions: " (keys partitions2))
  #_(let [p (set (keys partitions2))
          c (set (:partition-children updated-p))]
      (if-not (superset? p c)
        (println "partition children not in parent: " (difference c p))
        #_(println "partition children are in parent." #_(difference p c))))
  (when-not (or (:parent-pid updated-p) (:is-branched updated-p))
    (throw (ex-info "Every partition must have a parent or be branched" {:pid pid :action ?action})))
  (when (and (not (:is-branched updated-p)) (seq (:stage updated-p)))
    (throw (ex-info "Cannot stage to unbranched partition" {:pid pid :action ?action})))
  updated-p)

(defn partition-apply-defaults [p]
  ; This doesn't even work right
  (reduce-kv update p
    {:hydrate-id identity
     :popovers #(or % #{})

     :is-branched boolean
     :partition-children #(or % #{})
     :parent-pid identity

     ; data needed to hydrate a partition
     :route identity
     :pending-route identity
     :stage (fn [multi-color-tx]
              (->> multi-color-tx
                (remove (fn [[dbname tx]] (empty? tx)))
                (into {})))
     :local-basis identity

     ; response data of hydrating a partition
     :attr-renderers identity
     :error identity
     :ptm identity
     :schemas identity
     :tempid-lookups identity}))

(defn auto-transact-reducer [auto-tx action & args]
  (case action
    :set-auto-transact (first args)
    :update-auto-transact (let [[dbname auto-tx] args]
                            (assoc auto-tx dbname auto-tx))
    (or auto-tx {})))

(defn user-id-reducer [user-id action & args]
  (case action
    :set-user-id (first args)                               ; dead action?
    user-id))

(def reducer-map {:hyperfiddle.runtime/global-basis global-basis-reducer
                  :hyperfiddle.runtime/partitions partitions-reducer
                  :hyperfiddle.runtime/auto-transact auto-transact-reducer
                  :hyperfiddle.runtime/user-id user-id-reducer})

(def root-reducer (reducers/combine-reducers reducer-map))

(defn dispatch! [rt action]
  ;; Using js/console.debug with Chrome DevTools uses ~5ms instead of ~200ms for
  ;; big dispatch events. An optimized logging strategy should be discussed.
  ;; - JS/JVM specific optimizations
  ;; - unified runtime-specific logging configuration / flags
  #_#?(:clj (timbre/debug "dispatch!" action)
     :cljs (js/console.debug "dispatch!" action))
  (reducers/dispatch! (hf/state rt) root-reducer action))

(defn initialize [v] (root-reducer v nil))
