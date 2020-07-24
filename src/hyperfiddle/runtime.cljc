(ns hyperfiddle.runtime
  (:require
    [cats.core :refer [mlet]]
    [cats.labs.promise]
    [clojure.set :as set]
    [contrib.data :as data :refer [seq->>] :refer-macros [seq->>]]
    [contrib.datomic]
    [hyperfiddle.transaction :as tx]
    [contrib.reactive :as r]
    [hypercrud.types.DbRef :refer [->DbRef]]
    [hyperfiddle.api :as hf]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.route :as route]
    [hyperfiddle.state :as state]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(defn ^:deprecated state
  ([rt]
   (timbre/warn "runtime/state is no longer supported")
   (hf/state rt))
  ([rt path]
   (timbre/warn "runtime/state is no longer supported")
   (r/cursor (hf/state rt) path)))

(defn- state-ref [rt path] (r/cursor (hf/state rt) path))

(defn get-user-id [rt] @(state-ref rt [::user-id]))

(defn get-global-basis [rt] @(state-ref rt [::global-basis]))

(defn get-auto-transact [rt dbname] @(state-ref rt [::auto-transact dbname]))

(defn set-auto-transact [rt dbname auto-tx]
  {:pre [(domain/valid-dbname? (hf/domain rt) dbname)]}
  (state/dispatch! rt [:update-auto-transact dbname auto-tx]))

(defn get-partition [rt pid] @(state-ref rt [::partitions pid]))

(defn any-partition-loading? [rt]
  @(r/fmap->> (state-ref rt [::partitions])
              (some (comp not nil? :hydrate-id val))))

(defn create-partition
  "Idempotent. If a partition exists for the given partition id, nothing happens.
  Returns nil and immediately"
  ([rt pid child-pid]
   (create-partition rt pid child-pid false))
  ([rt pid child-pid is-branched]
   {:pre [pid child-pid]}
   (when @(r/fmap nil? (state-ref rt [::partitions child-pid]))
     (state/dispatch! rt [:create-partition pid child-pid is-branched]))
   nil))

(defn delete-partition [rt pid]
  {:pre [pid]}
  (state/dispatch! rt [:delete-partition pid]))

(defn branch-partition
  [rt pid]
  {:pre [pid]}
  (state/dispatch! rt [:branch-partition pid]))

(defn branched?
  "Returns false when a partition shares its parent's staging area"
  [rt pid]
  {:pre [pid]}
  (boolean @(state-ref rt [::partitions pid :is-branched])))

(defn parent-pid [rt pid]
  {:pre [pid]}
  @(state-ref rt [::partitions pid :parent-pid]))

(defn get-branch-pid
  "Given a partition-id, what is the most immediate partition that has a branched stage.
  Will never return nil"
  [rt pid]
  {:pre [pid]}
  (if (branched? rt pid)
    pid
    (get-branch-pid rt (parent-pid rt pid))))

(defn get-branch-parent-pid
  "Given a partition-id,  immediate partition that has a branched stage.
  Returns nil if there is no parent with a branch"
  [rt pid]
  {:pre [pid]}
  (let [branch-pid (get-branch-pid rt pid)
        parent (parent-pid rt branch-pid)]
    (when parent
      (get-branch-pid rt parent))))

(defn child-pids
  "Immediate children
  Returns a coll of pids"
  [rt pid]
  {:pre [pid]}
  @(state-ref rt [::partitions pid :partition-children]))

(defn descendant-pids
  "All children, and their children...
  Returns a coll of pids"
  [rt pid]
  {:pre [pid]}
  (->> (child-pids rt pid)
       (mapcat (partial descendant-pids rt))
       (cons pid)))

(defn get-error [rt pid]
  {:pre [pid]}
  @(state-ref rt [::partitions pid :error]))

(defn set-error [rt pid e]
  {:pre [pid]}
  (state/dispatch! rt [:partition-error pid e]))

(defn loading?
  [rt pid]
  {:pre [pid]}
  (some? @(state-ref rt [::partitions pid :hydrate-id])))

(defn db
  [rt pid dbname]
  {:pre [pid]}
  (cond
    #_#_(not (branched? rt pid)) (db rt (parent-pid rt pid) dbname)
    #_#_(domain/valid-dbname? (domain rt) dbname) (throw (ex-info "Invalid dbname" {:dbname dbname}))
    :else (->DbRef dbname pid)))

(defn get-schemas
  "Returns a map[k,v]
  k :: dbname
  v :: nil or cats.monad.exception[contrib.datomic.Schema]"
  ; todo this needs work
  [rt pid]
  {:pre [pid]
   :post [(some? %)]}
  (or
    ; When hydrating a popover directly, this partition has the schemas, not the parent. Todo untangle this
    @(state-ref rt [::partitions pid :schemas])             ; hack
    (get-schemas rt (parent-pid rt pid))
    #_@(state-ref rt [::partitions (get-branch-pid rt pid) :schemas]))) ; code path as designed

(defn get-schema+
  "Returns nil or cats.monad.exception[contrib.datomic.Schema]"
  ; todo this needs work
  [rt pid dbname]
  {:pre [pid]}
  (get (get-schemas rt pid) dbname))

(defn get-attr-renderer
  [rt pid ident]
  @(state-ref rt [::partitions (get-branch-pid rt pid) :attr-renderers ident]))

(defn get-local-basis [rt pid]
  {:pre [pid]}
  @(state-ref rt [::partitions (get-branch-pid rt pid) :local-basis]))

(defn get-route [rt pid]
  {:pre [pid]
   #_#_:post [%]}
  (doto
    @(state-ref rt [::partitions pid :route])
    #_(assert (str "route not found for pid: " pid))))

(defn get-route-defaults [rt pid]
  {:pre [pid]}
  @(state-ref rt [::partitions pid :route-defaults]))

(defn get-pending-route [rt pid]
  {:pre [pid]}
  @(state-ref rt [::partitions pid :pending-route]))

(defn- refresh-global-basis [rt root-pid]
  (-> (io/global-basis (hf/io rt))
      (p/then (fn [global-basis] (state/dispatch! rt [:set-global-basis global-basis])))
      (p/catch (fn [e]
                 (timbre/error e)
                 (set-error rt root-pid e)
                 (throw e)))))

(defn- refresh-partition-basis [rt pid]
  (let [{:keys [::global-basis ::partitions]} @(hf/state rt)
        route (or (get-in partitions [pid :pending-route])
                  (get-in partitions [pid :route]))]
    (-> (io/local-basis (hf/io rt) global-basis route)
        (p/then (fn [local-basis]
                  (state/dispatch! rt [:partition-basis pid local-basis])))
        (p/catch (fn [error]
                   (set-error rt pid error)
                   (throw error))))))

(defn- process-children [rt pid new-partitions]
  (let [local-children (set (child-pids rt pid))
        new-children (set (get-in new-partitions [pid :partition-children]))]
    (reduce (fn [acc child-pid]
              (cond
                (and (new-children child-pid)
                     (local-children child-pid))
                (if (= (get-in new-partitions [child-pid :route])
                      (get-route rt child-pid))             ; This is sometimes nil, and that's a problem
                  (merge-with                               ; stuff in ptm and recurse for child-pid's children
                    into
                    (update acc :actions conj [:hydrate!-route-success child-pid (get new-partitions child-pid)])
                    (process-children rt child-pid new-partitions))
                  (update acc :rehydrate-pids conj child-pid)) ; the route has changed

                (new-children child-pid)
                (merge-with                                 ; stuff in ptm and recurse for child-pid's children
                  into
                  (update acc :actions conj [:new-hydrated-partition pid child-pid (get new-partitions child-pid)])
                  (process-children rt child-pid new-partitions))

                (local-children child-pid)
                (if false                                   ; todo, branched? branched-popover?
                  (update acc :rehydrate-pids conj child-pid)
                  (update acc :actions conj [:delete-partition child-pid]))))

            {:actions []
             :rehydrate-pids []}
            (set/union local-children new-children))))

(defn- accumulate-parent-partitions [rt pid]
  (loop [curr-pid pid
         acc {pid (assoc (get-partition rt pid) :partition-children #{})}]
    (if-let [ppid (parent-pid rt curr-pid)]
      (let [p (-> (get-partition rt ppid)
                  (assoc :partition-children #{curr-pid}))]
        (recur ppid (assoc acc ppid p)))
      acc)))

(defn- hydrate-partition [rt pid]
  (let [{:keys [hydrate-id] :as p} (get-partition rt pid)
        route (or (:pending-route p) (:route p))
        partitions (->> (or (->> (descendant-pids rt pid)
                                 (filter #(branched? rt %))
                                 seq)
                            [pid])
                        (map #(accumulate-parent-partitions rt %))
                        (reduce (fn [as bs]
                                  (reduce-kv (fn [acc pid p]
                                               (if (contains? acc pid)
                                                 (update-in acc [pid :partition-children] set/join (:partition-children p))
                                                 (assoc acc pid p)))
                                             as bs)))
                        (data/map-values #(select-keys % [:is-branched :partition-children :parent-pid :stage])))]
    (-> (io/hydrate-route (hf/io rt) (get-local-basis rt pid) route pid partitions)
        (p/catch (fn [e]
                   (timbre/info pid hydrate-id @(state-ref rt [::partitions pid :hydrate-id]))
                   (timbre/error e)
                   (if (= hydrate-id @(state-ref rt [::partitions pid :hydrate-id]))
                     (set-error rt pid e)
                     (timbre/info (str "Ignoring response for " hydrate-id)))
                   (throw e)))
        (p/then (fn [new-partitions]
                  (if (= hydrate-id @(state-ref rt [::partitions pid :hydrate-id]))
                    (let [{:keys [actions rehydrate-pids]} (process-children rt pid new-partitions)
                          ; if rehydrated-pids is not already locally seen, then the state is already corrupted
                          actions (into [:batch             ; Order does not matter; the final state change is atomic
                                         [:hydrate!-route-success pid (get new-partitions pid)]]
                                        (concat actions
                                                (map (fn [pid] [:hydrate!-start pid]) rehydrate-pids)
                                                (when-not (parent-pid rt pid)
                                                  [[:set-global-user-basis (get-in new-partitions [pid :local-basis])]])))]
                      #_(println "rehydrate-pids " rehydrate-pids)
                      (state/dispatch! rt actions)
                      (-> (map #(hydrate-partition rt %) rehydrate-pids) ; recursion
                          (p/all)
                          (p/then (constantly nil))))
                    (timbre/info (str "Ignoring response for " hydrate-id))))))))

(def LEVEL-NONE 0)
(def LEVEL-GLOBAL-BASIS 1)
(def LEVEL-LOCAL-BASIS 2)
(def LEVEL-HYDRATE-PAGE 3)

(defn bootstrap-data
  "Returns a promise"
  [rt pid init-level & {:keys [hydrate-page?] :or {hydrate-page? (constantly true)}}]
  (cond-> (p/resolved nil)
    (< init-level LEVEL-GLOBAL-BASIS) (p/then (fn [_] (refresh-global-basis rt pid)))
    (< init-level LEVEL-LOCAL-BASIS) (p/then (fn [_] (refresh-partition-basis rt pid)))
    (< init-level LEVEL-HYDRATE-PAGE) (p/then (fn [_]
                                                (if (hydrate-page?)
                                                  (hydrate-partition rt pid)
                                                  (p/resolved nil))))))

(defn get-stage
  ([rt pid]
   {:pre [pid]}
   @(state-ref rt [::partitions (get-branch-pid rt pid) :stage]))
  ([rt pid dbname]
   {:pre [pid]}
   @(state-ref rt [::partitions (get-branch-pid rt pid) :stage dbname])))

(defn set-stage
  "Stages tx to the given dbname (overwrites existing tx).  This will rehydrate the given branch.
  Returns a promise"
  [rt pid dbname tx]
  (cond
    (not (branched? rt pid)) (set-stage rt (get-branch-pid rt pid) dbname tx)
    (not (domain/valid-dbname? (hf/domain rt) dbname)) (p/rejected (ex-info "Unable to stage to an invalid db" {:dbname dbname}))
    (get-auto-transact rt dbname) (p/rejected (ex-info "Unable to stage to a db with auto-transact on." {:dbname dbname}))
    :else (when (not= tx (get-stage rt pid dbname))
            (state/dispatch! rt [:batch
                                 [:reset-stage-db pid dbname tx]
                                 [:hydrate!-start pid]])
            (hydrate-partition rt pid))))

(defn get-tempid-lookup!
  "Returns the tempid lookup for the given branch and dbname.
  May throw an exception (e.g. if the branch has an invalid stage)"
  [rt pid dbname]
  (some-> @(state-ref rt [::partitions (get-branch-pid rt pid) :tempid-lookups dbname])
          deref))

(defn id->tempid! [rt pid dbname id]
  ; todo what about if the tempid is on a higher branch in the uri?
  (if (contrib.datomic/tempid? id)
    id
    (let [id->tempid (get-tempid-lookup! rt pid dbname)]
      (get id->tempid id id))))

(defn tempid->id! [rt pid dbname tempid]
  ; todo what about if the tempid is on a higher branch in the uri?
  (if (contrib.datomic/tempid? tempid)
    (let [tempid->id (-> (get-tempid-lookup! rt pid dbname)
                         (set/map-invert))]
      (get tempid->id tempid tempid))
    tempid))

(defn- transact-impl [rt pid tx-groups & post-tx]
  {:pre [(branched? rt pid)]}
  (mlet [{:keys [tempid->id]} (io/transact! (hf/io rt) tx-groups)
         :let [route (route/invert-route (get-route rt pid) (fn [dbname id] (get-in tempid->id [dbname id] id)))
               _ (state/dispatch! rt (into [:batch [:transact!-success pid (keys tx-groups)]] post-tx))]
         _ (refresh-global-basis rt pid)]
    ; todo we want to overwrite our current browser location with this new url
    ; currently this new route breaks the back button
    ; todo should just call foundation/bootstrap-data
    (hf/set-route rt pid route true)))

; todo do something when child branches exist and are not nil: hyperfiddle/hyperfiddle#99
; can only transact one branch
(defn transact
  "Transacts the staging area of this partition (if the partition is unbranched, transact the parent branch).
  If specified, the transaction can be restricted to a single database.
  Returns a promise"
  ([rt pid]
   (let [pid (get-branch-pid rt pid)]
     (transact-impl rt pid (get-stage rt pid))))
  ([rt pid dbname]
   (if (domain/valid-dbname? (hf/domain rt) dbname)
     (let [pid (get-branch-pid rt pid)]
       (transact-impl rt pid {dbname (get-stage rt pid dbname)}))
     (p/rejected (ex-info "Unable to transact to an invalid db" {:dbname dbname})))))

(defn- update-to-tempids! [rt pid dbname tx]                ; cljs!
  (let [schema @(get-schema+ rt pid dbname)
        id->tempid (get-tempid-lookup! rt pid dbname)]
    (map (fn [stmt]
           (cond
             (map? stmt)
             stmt

             ; e.g. [:db/retract e a v] [:db/retractEntity e] (user/f v)
             (or (vector? stmt) (list? stmt))
             (hf/stmt-id->tempid id->tempid schema stmt)

             () (throw (ex-info (str `update-to-tempids! " no match") {:stmt stmt}))))
         tx)))

(defn with-tx
  "Stage tx to the given dbname (appends to existing tx).  This will rehydrate the given branch.  This may or may not immediately transact.
  Returns a promise"
  ([ctx tx]
   (with-tx (:runtime ctx) (:partition-id ctx) (hf/dbname ctx) tx))
  ([rt pid dbname tx]
   (cond
     (not (branched? rt pid)) (with-tx rt (get-branch-pid rt pid) dbname tx)
     (not (domain/valid-dbname? (hf/domain rt) dbname)) (p/rejected (ex-info "Unable to stage to an invalid db" {:dbname dbname}))
     (empty? tx) (do (timbre/warn "No tx provided")
                     (p/resolved nil))
     :else (do
             (let [tx (update-to-tempids! rt pid dbname tx)]
               (state/dispatch! rt [:with pid dbname tx]))
             (if (and (nil? (parent-pid rt pid)) (get-auto-transact rt dbname))
               (transact pid dbname)
               (do
                 (state/dispatch! rt [:hydrate!-start pid])
                 (hydrate-partition rt pid)))))))

(defn commit-branch
  "Commit the branch to its parent.  This will rehydrate the parent branch.  This may or may not immediately transact.
  Returns a promise"
  [rt pid tx-groups]                                        ; todo rewrite in terms of with-tx
  (cond
    (not (branched? rt pid)) (commit-branch rt (get-branch-pid rt pid) tx-groups)
    (not (domain/valid-dbnames? (hf/domain rt) (keys tx-groups))) (p/rejected (ex-info "Unable to commit to an invalid db" {:invalid-dbnames (remove #(domain/valid-dbname? (hf/domain rt) %) (keys tx-groups))}))
    :else (do
            (let [with-actions (->> tx-groups
                                    (remove (fn [[dbname tx]] (empty? tx)))
                                    (mapv (fn [[dbname tx]]
                                            (let [tx (update-to-tempids! rt pid dbname tx)]
                                              [:with pid dbname tx]))))]
              ; should the tx fn not be withd? if the transact! fails, do we want to run it again?
              (state/dispatch! rt (into [:batch] with-actions)))
            (let [ppid (get-branch-parent-pid rt pid)       ; if not branched, there is no parent-pid
                  tx-groups (->> (get-stage rt pid)
                                 (filter (fn [[dbname tx]] (and (get-auto-transact rt dbname) (not (empty? tx)))))
                                 (into {}))]
              (if (and (nil? (parent-pid rt pid)) (not (empty? tx-groups)))
                ; todo what if transact throws?
                (transact-impl rt pid tx-groups [:merge ppid pid])
                (do
                  (state/dispatch! rt [:batch
                                       [:merge ppid pid]
                                       [:hydrate!-start ppid]])
                  (hydrate-partition rt ppid)))))))

(defn open-popover [rt pid popover-pid]
  (state/dispatch! rt [:open-popover pid popover-pid]))

(defn close-popover [rt pid popover-pid]
  (state/dispatch! rt [:close-popover pid popover-pid]))

(defn popover-is-open? [rt pid popover-pid]
  (some? @(state-ref rt [::partitions pid :popovers popover-pid])))
