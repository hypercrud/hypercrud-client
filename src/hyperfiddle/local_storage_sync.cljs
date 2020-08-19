(ns hyperfiddle.local-storage-sync
  (:require
    [contrib.component :as component]
    [contrib.document :refer [document-location!]]
    [contrib.local-storage :as local-storage]
    [hyperfiddle.io.basis :as basis]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.state :as state]
    [taoensso.timbre :as timbre]
    [hyperfiddle.api :as hf]))


(def running-ls-schema-version 5)

(def ls-migrations
  {2 (fn [ls-state]                                         ; v3 removed non-nil stage from localstorage
       (assert (= 2 (:version ls-state)))
       (-> (select-keys ls-state [::runtime/auto-transact
                                  ::runtime/global-basis
                                  ::runtime/user-id
                                  :stage])
           (update :stage get nil)
           (assoc :version 3)))
   3 (fn [ls-state]                                         ; v4 add :last-modified
       (assert (= 3 (:version ls-state)))
       (assoc ls-state :version 4 :last-modified 0))        ; just use the epoch
   4 (fn [ls-state]                                         ; v5 migrate uris to dbnames
       (assert (= 4 (:version ls-state)))
       ; just pave anything with a uri
       (-> ls-state
           (dissoc ::runtime/auto-transact ::runtime/global-basis :stage)
           (assoc :version 5)))
   5 (fn [ls-state]                                         ; v6 global-basis/domain enhancement
       (assert (= 5 (:version ls-state)))
       (-> ls-state
           (dissoc ::runtime/global-basis)
           (assoc :version 6)))
   })

(defn- state->local-storage [pid state-val]
  (-> (select-keys state-val [::runtime/auto-transact
                              ::runtime/global-basis
                              ::runtime/user-id])
      (cond->
        (not (get-in state-val [::runtime/partitions pid :parent-pid]))
        (assoc ::runtime/global-basis (::runtime/global-basis state-val)))
      (assoc :version running-ls-schema-version
             :stage (get-in state-val [::runtime/partitions pid :stage]))))

(defn- local-storage-state-watcher [pid ls-key k r o n]
  (let [o (state->local-storage pid o)
        n (state->local-storage pid n)]
    (when-not (= o n)
      ; only sync if the LS values have changed
      (let [ls-state (try (local-storage/get-item ls-key) (catch :default e nil))]
        ; implied trust that the localstorage listener has already synced other tabs with this one
        (when-not (= ls-state n)
          (local-storage/set-item! ls-key (assoc n :last-modified (.now js/Date))))))))

(defn watch-key [branch-id] (keyword (str (hash branch-id)) "local-storage"))

(defn- update-state [rt pid ls-key new-value different-basis]
  (let [{:keys [::runtime/auto-transact ::runtime/global-basis ::runtime/user-id :stage :version]} new-value
        different-user (not= user-id (runtime/get-user-id rt))
        different-stage (not= stage (runtime/get-stage rt pid))
        different-autotx (some (fn [[dbname auto-tx]] (not= auto-tx (runtime/get-auto-transact rt dbname))) auto-transact)
        init-level (cond
                     different-user runtime/LEVEL-NONE
                     different-basis runtime/LEVEL-GLOBAL-BASIS
                     different-stage runtime/LEVEL-LOCAL-BASIS
                     :else runtime/LEVEL-HYDRATE-PAGE)
        action (cond-> [:batch]
                 different-user (conj [:set-user-id user-id])
                 different-basis (conj [:set-global-basis global-basis])
                 different-stage (conj [:reset-stage-branch pid stage])
                 different-autotx (conj [:set-auto-transact auto-transact]))]
    (remove-watch (hf/state rt) (watch-key pid))
    ; this dispatch! is syncronous, so we can safely, temporarily stop the localstorage sync
    ; this is desired so an inactive tab does NOT
    (state/dispatch! rt action)
    (add-watch (hf/state rt) (watch-key pid) (partial local-storage-state-watcher pid ls-key))
    (try
      (let [changing-route (when-not (runtime/parent-pid rt pid)
                             (let [existing-route (runtime/get-route rt pid)
                                   route (hf/url-decode (hf/domain rt) (document-location!))]
                               (when-not (= existing-route route)
                                 (state/dispatch! rt [:stage-route pid route])
                                 true)))]
        (runtime/bootstrap-data rt pid init-level :hydrate-page? (constantly (or different-stage changing-route))))
      (catch js/Error e
        (runtime/set-error rt pid e)))))

(defn- local-storage-event-action [rt pid ls-key new-value]
  (let [{:keys [::runtime/auto-transact ::runtime/global-basis ::runtime/user-id :stage :version]} new-value]
    (if (not= running-ls-schema-version version)
      (let [error "Hyperfiddle has been updated, please refresh!"]
        (when (not= (runtime/get-error rt pid) error)
          ; turn off localstorage sync, this tab is dead
          (remove-watch (hf/state rt) (watch-key pid))
          (runtime/set-error rt pid error)))
      (let [action (fn [different-basis] (update-state rt pid ls-key new-value different-basis))
            init-gb (runtime/get-global-basis rt)]
        (cond
          (runtime/parent-pid rt pid) (action false)        ; this event listener is not on the root-branch, ignore basis differences
          (identical? init-gb global-basis) (action false)  ; do nothing with basis
          :else (case (compare (get-in init-gb [:domain :t]) (get-in global-basis [:domain :t])) ; compare domain first, if different, the user keys might be different which is ok
                  1 (action false)                          ; should not happen, but if poorly behaving tabs could write a stale value to local storage, ignore it
                  -1 (if (not= (get-in init-gb [:domain :hash]) (get-in global-basis [:domain :hash]))
                       (do
                         ; compared to this tab, the event was fired from a tab using a domain in the future, force a hard refresh
                         ; todo this branch needs improvement. Currently there is no facility to reload the domain record, but there could be
                         (remove-watch (hf/state rt) (watch-key pid))
                         (runtime/set-error rt pid (ex-info "Stale domain. Please reload this page to continue."
                                                            {:current (:domain init-gb) :local-storage-event (:domain global-basis)})))
                       (case (basis/compare-uri-maps (:user init-gb) (:user global-basis))
                         -1 (action true)                   ; refresh global-basis
                         1 (action false)                   ; should not happen, but if poorly behaving tabs could write a stale value to local storage, ignore it
                         0 (action false)                   ; do nothing with basis
                         ))
                  0 (if (not= (get-in init-gb [:domain :hash]) (get-in global-basis [:domain :hash]))
                      (do
                        ; stop the show; there is nothing that can be done
                        (remove-watch (hf/state rt) (watch-key pid))
                        (runtime/set-error rt pid (ex-info "Domain bases not comparable. t cannot be the same when hash is different"
                                                           {:current (:domain init-gb) :local-storage-event (:domain global-basis)})))
                      (case (basis/compare-uri-maps (:user init-gb) (:user global-basis))
                        -1 (action true)                    ; refresh global-basis
                        1 (action false)                    ; should not happen, but if poorly behaving tabs could write a stale value to local storage, ignore it
                        0 (action false)                    ; do nothing with basis
                        ))))))))

(defn- init-auto-tx [ls ssr]
  (reduce-kv (fn [acc k ssr-v]
               (let [ls-v (get ls k)]
                 (assoc acc k (cond
                                (false? ssr-v) false        ; ssr false trumps local storage
                                (some? ls-v) ls-v           ; user may have opted in to auto-tx
                                :else ssr-v                 ; trust ssr
                                ))))
             {}
             ssr))

(defn- init-state [domain initial-state ls-state & [global-basis]]
  (-> (select-keys ls-state [::runtime/auto-transact
                             ::runtime/user-id
                             :last-modified
                             :stage
                             :version])
      (update :stage select-keys (keys (hf/databases domain))) ; drop any stage no longer applicable to domain ; todo show an error page when dropping tx? user might lose work
      (update ::runtime/auto-transact init-auto-tx (::runtime/auto-transact initial-state))
      (assoc ::runtime/user-id (::runtime/user-id initial-state)) ; ssr always win (it has access to cookies)
      (cond->
        global-basis (assoc ::runtime/global-basis global-basis))))

(defrecord LocalStorageSync [rt pid ls-key event-listener]
  component/Lifecycle
  (start [component]
    (let [initial-state @(hf/state rt)
          ls-state (try (local-storage/get-item ls-key) (catch :default e {}))
          new-ls-state (loop [ls-state ls-state]
                         (if (= running-ls-schema-version (:version ls-state))
                           (if-not (runtime/parent-pid rt pid)
                             (init-state (hf/domain rt) initial-state ls-state)
                             (let [global-basis (let [init-gb (::runtime/global-basis initial-state)
                                                      ls-gb (get ls-state [::runtime/global-basis])]
                                                  (cond
                                                    (identical? init-gb ls-gb) init-gb
                                                    (nil? init-gb) ls-gb
                                                    (nil? ls-gb) init-gb
                                                    :else (case (compare (get-in init-gb [:domain :t]) (get-in ls-gb [:domain :t]))
                                                            ; compare domain first, if different, the user keys might be different which is ok
                                                            -1 ls-gb
                                                            1 init-gb
                                                            0 (if (not= (get-in init-gb [:domain :hash]) (get-in ls-gb [:domain :hash]))
                                                                (throw (ex-info "Domain bases not comparable. t cannot be the same when hash is different"
                                                                                {:server (:domain init-gb) :local-storage (:domain ls-gb)}))
                                                                (case (basis/compare-uri-maps (:user init-gb) (:user ls-gb))
                                                                  -1 ls-gb
                                                                  0 init-gb
                                                                  1 init-gb)))))]
                               (init-state (hf/domain rt) initial-state ls-state global-basis)))
                           (if-let [migrate (get ls-migrations (:version ls-state))]
                             (recur (migrate ls-state))
                             (do
                               (when-not (nil? ls-state) (timbre/error "Unable to migrate local-storage: " ls-state))
                               (-> (state->local-storage pid initial-state)
                                   (assoc :last-modified (.now js/Date))
                                   (update ::runtime/auto-transact #(init-auto-tx nil %)))))))]
      (when-not (= ls-state new-ls-state)
        (local-storage/set-item! ls-key new-ls-state))
      ; todo, we should be dispatching
      (reset! (hf/state rt)
              (-> (merge initial-state (dissoc new-ls-state :stage :version :last-modified))
                  (assoc-in [::runtime/partitions pid :stage] (:stage new-ls-state)))))

    (let [event-listener (fn [e]
                           (when (local-storage/same-key? ls-key e)
                             (local-storage-event-action rt pid ls-key (local-storage/event-new-value e))))]
      (.addEventListener js/window "storage" event-listener)
      (add-watch (hf/state rt) (watch-key pid) (partial local-storage-state-watcher pid ls-key))
      (assoc component :event-listener event-listener)))

  (stop [component]
    (.removeEventListener js/window "storage" event-listener)
    (remove-watch (hf/state rt) (watch-key pid))
    component))
