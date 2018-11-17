(ns hyperfiddle.actions
  (:require
    [cats.core :refer [mlet]]
    [cats.labs.promise]
    [cats.monad.either :as either]
    [clojure.spec.alpha :as s]
    [contrib.datomic-tx :as tx]
    [contrib.uri :refer [->URI]]
    [hypercrud.browser.routing :as routing]
    [hypercrud.client.core :as hc]
    [hypercrud.types.EntityRequest :refer [->EntityRequest]]
    [hypercrud.types.Err :as Err]
    [hypercrud.util.branch :as branch]
    [hyperfiddle.io.hydrate-requests :refer [hydrate-all-or-nothing!]]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security.client :as security]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(defn set-display-mode [display-mode]
  [:set-display-mode display-mode])

; batch doesn't make sense with thunks (can be sync or async dispatches in a thunk),
; user beware
(defn batch [& action-list] (cons :batch action-list))

(defn hydrate-partition [rt branch dispatch! get-state]
  (let [{:keys [hydrate-id]} (get-in (get-state) [::runtime/partitions branch])]
    (-> (runtime/hydrate-route rt branch)
        (p/then (fn [{:keys [local-basis ptm tempid-lookups]}]
                  (if (= hydrate-id (get-in (get-state) [::runtime/partitions branch :hydrate-id]))
                    (dispatch! [:hydrate!-route-success branch ptm tempid-lookups local-basis]) ; todo domain & user are now potentially out of sync
                    (timbre/info (str "Ignoring response for " hydrate-id)))))
        (p/catch (fn [error]
                   (if (= hydrate-id (get-in (get-state) [::runtime/partitions branch :hydrate-id]))
                     (dispatch! [:partition-error branch error])
                     (timbre/info (str "Ignoring response for " hydrate-id)))
                   (throw error))))))

(defn refresh-global-basis [rt on-finally dispatch! get-state] ; on-finally is a crude hack
  (-> (runtime/global-basis rt)
      (p/then (fn [global-basis]
                (dispatch! (apply batch [:set-global-basis global-basis] on-finally))))
      (p/catch (fn [error]
                 (dispatch! (apply batch [:set-error error] on-finally))
                 (throw error)))))

(defn refresh-partition-basis [rt branch dispatch! get-state]
  (-> (runtime/local-basis rt branch)
      (p/then (fn [local-basis]
                (dispatch! [:partition-basis branch local-basis])))
      (p/catch (fn [error]
                 (dispatch! [:partition-error branch error])
                 (throw error)))))

(defn hydrate-partition-schema [rt branch dispatch! get-state]
  (-> (runtime/hydrate-schemas rt branch)
      (p/then (fn [schema]
                (dispatch! [:partition-schema branch schema])))
      (p/catch (fn [error]
                 (dispatch! [:partition-error branch error])
                 (throw error)))))

(defn refresh-domain [rt dispatch! get-state]
  (-> (runtime/domain rt)
      (p/then (fn [domain]
                (dispatch! [::runtime/set-domain domain])))
      (p/catch (fn [error]
                 (dispatch! [:set-error error])
                 (throw error)))))

(defn refresh-user [rt dispatch! get-state]
  (let [users-uri (->URI "datomic:free://datomic:4334/hyperfiddle-users") ; todo inject
        beta-uri (->URI "datomic:free://datomic:4334/~dustin.getz@hyperfiddle.net+beta") ; todo inject
        tank? (= "tank" @(runtime/state rt [::runtime/domain :domain/ident])) ; todo this should be modeled on the domain/project
        active-ide? (:active-ide? (runtime/host-env rt))
        user-uris (cond-> #{}
                    tank? (conj beta-uri)
                    active-ide? (conj users-uri))
        basis (->> (if-let [global-basis @(runtime/state rt [::runtime/global-basis])]
                     (merge (:ide global-basis) (:user global-basis))
                     (->> @(runtime/state rt [::runtime/partitions])
                          (some (fn [[_ partition]]
                                  (->> (:local-basis partition)
                                       (filter (fn [[k _]] (contains? user-uris k)))
                                       seq)))))
                   (filter (comp user-uris first))
                   (into {}))
        stage nil
        requests (when-let [user-id @(runtime/state rt [::runtime/user-id])]
                   (cond-> []
                     tank?
                     (conj (->EntityRequest [:user/user-id user-id] (hc/db rt beta-uri nil) [:hfnet.beta/accepted-on :hfnet.beta/archived]))

                     active-ide?
                     (conj (->EntityRequest [:user/user-id user-id] (hc/db rt users-uri nil) [:hyperfiddle.ide/parinfer]))))]
    (-> (if (empty? requests)
          (p/resolved nil)
          (hydrate-all-or-nothing! rt basis stage requests))
        (p/then (fn [responses]
                  (dispatch! [:set-user (apply merge {} responses)])))
        (p/catch (fn [error]
                   (dispatch! [:set-error error])
                   (throw error))))))

(defn add-partition [rt route branch branch-aux & on-start]
  {:pre [(s/valid? :hyperfiddle/route route)]}
  (fn [dispatch! get-state]
    (dispatch! (apply batch (conj (vec on-start) [:add-partition branch route branch-aux])))
    (-> (refresh-partition-basis rt branch dispatch! get-state)
        (p/then #(dispatch! [:hydrate!-start branch]))
        (p/then #(hydrate-partition-schema rt branch dispatch! get-state))
        (p/then #(hydrate-partition rt branch dispatch! get-state)))))

(defn discard-partition [branch]
  [:discard-partition branch])

(defn close-popover [branch popover-id]
  [:close-popover branch popover-id])

(defn set-route [rt route branch keep-popovers? force dispatch! get-state]
  {:pre [(s/valid? :hyperfiddle/route route)]}
  (assert (nil? branch) "Non-nil branches currently unsupported")
  (let [current-route (get-in (get-state) [::runtime/partitions branch :route])]
    (if (and (not force) (route/compare-routes route current-route) (not= route current-route))
      (dispatch! [:partition-route branch route])           ; just update state without re-hydrating
      ; currently branches only have relationships to parents, need to be able to find all children from a parent
      ; this would allow us to discard/close ourself and our children
      ; for now we are always nil branch, so blast everything
      (let [actions (->> (::runtime/partitions (get-state))
                         (mapcat (fn [[ident partition]]
                                   (conj (if (and (= branch ident) keep-popovers?)
                                           (vector)
                                           (mapv (partial close-popover ident) (:popovers partition)))
                                         (if (= branch ident)
                                           [:partition-route ident route] ; dont blast nil stage
                                           (discard-partition ident))))))]
        (dispatch! (apply batch actions))
        ; should just call foundation/bootstrap-data
        (-> (refresh-partition-basis rt branch dispatch! get-state)
            (p/then #(dispatch! [:hydrate!-start branch]))
            (p/then #(hydrate-partition-schema rt branch dispatch! get-state))
            (p/then #(hydrate-partition rt branch dispatch! get-state)))))))

(defn update-to-tempids [get-state branch uri tx]
  (let [{:keys [tempid-lookups schemas]} (get-in (get-state) [::runtime/partitions branch])
        schema (get schemas uri)
        id->tempid (some-> (get tempid-lookups uri)
                           (either/branch #(throw (ex-info % {})) identity))]
    (map (partial tx/stmt-id->tempid id->tempid schema) tx)))

(defn transact! [rt tx-groups dispatch! get-state & {:keys [route post-tx]}]
  (dispatch! [:transact!-start])
  (-> (runtime/transact! rt tx-groups)
      (p/catch (fn [e]
                 #?(:cljs
                    (let [message (cond
                                    (string? e) e
                                    (Err/Err? e) (:msg e)
                                    (map? e) (:message e)
                                    :else (ex-message e))]
                      (js/alert message)))
                 (dispatch! [:transact!-failure e])
                 (throw e)))
      (p/then (fn [{:keys [tempid->id]}]
                ; todo should just call foundation/bootstrap-data
                (mlet [:let [on-finally (into [[:transact!-success (keys tx-groups)]] post-tx)]
                       _ (refresh-global-basis rt on-finally dispatch! get-state)
                       _ (refresh-domain rt dispatch! get-state)
                       _ (refresh-user rt dispatch! get-state)
                       :let [current-route (get-in (get-state) [::runtime/partitions nil :route])]]
                  (either/branch
                    (or (some-> route route/validate-route+) ; arbitrary user input, need to validate
                        (either/right current-route))
                    (fn [e]
                      (dispatch! [:set-error e])
                      (p/rejected e))
                    (fn [route]
                      (let [invert-id (fn [temp-id uri] (get-in tempid->id [uri temp-id] temp-id))
                            route' (routing/invert-route (::runtime/domain (get-state)) route invert-id)
                            keep-popovers? (or (nil? route) (route/compare-routes route current-route))]
                        ; todo we want to overwrite our current browser location with this new url
                        ; currently this new route breaks the back button
                        (set-route rt route' nil keep-popovers? true dispatch! get-state)))))))))

(defn should-transact!? [uri get-state]
  (and (get-in (get-state) [::runtime/auto-transact uri])
       (let [hf-db (domain/uri->hfdb uri (::runtime/domain (get-state))) ; todo this needs sourced from context domain for topnav
             user-id (get-in (get-state) [::runtime/user-id])
             user (get-in (get-state) [::runtime/user])]
         (either/branch
           (security/subject-can-transact? hf-db user-id user)
           #(do
              (timbre/error %)
              false)
           identity))))

(defn with-groups [rt branch tx-groups & {:keys [route post-tx]}]
  {:pre [(not-any? nil? (keys tx-groups))]}
  (fn [dispatch! get-state]
    (let [tx-groups (->> tx-groups
                         (remove (fn [[uri tx]] (empty? tx)))
                         (map (fn [[uri tx]] [uri (update-to-tempids get-state branch uri tx)]))
                         (into {}))
          transact-uris (->> (keys tx-groups)
                             (filter (fn [uri] (and (nil? branch) (should-transact!? uri get-state)))))
          transact-groups (select-keys tx-groups transact-uris)
          with-actions (->> (apply dissoc tx-groups transact-uris)
                            (mapv (fn [[uri tx]] [:with branch uri tx])))]
      (if (not (empty? transact-groups))
        ; todo what if transact throws?
        (transact! rt transact-groups dispatch! get-state
                   :post-tx (let [clear-uris (->> (keys transact-groups)
                                                  (map (fn [uri] [:reset-stage-uri branch uri nil]))
                                                  vec)]
                              (concat clear-uris            ; clear the uris that were transacted
                                      with-actions))
                   :route route)
        (either/branch
          (or (some-> route route/validate-route+) (either/right nil)) ; arbitrary user input, need to validate
          (fn [e]
            (dispatch! (apply batch (conj with-actions [:set-error e]))))
          (fn [route]
            (let [actions (cond-> with-actions
                            ; what about local-basis? why not specify branch?
                            route (conj [:partition-route nil route]))]
              (dispatch! (apply batch (conj actions [:hydrate!-start branch])))
              (hydrate-partition rt branch dispatch! get-state))))))))

(defn open-popover [branch popover-id]
  [:open-popover branch popover-id])

(defn stage-popover [rt branch swap-fn-async & on-start]    ; todo rewrite in terms of with-groups
  (fn [dispatch! get-state]
    (p/then (swap-fn-async)
            (fn [{:keys [tx app-route]}]
              (let [with-actions (mapv (fn [[uri tx]]
                                         (let [tx (update-to-tempids get-state branch uri tx)]
                                           [:with branch uri tx]))
                                       tx)
                    parent-branch (branch/decode-parent-branch branch)]
                ; should the tx fn not be withd? if the transact! fails, do we want to run it again?
                (dispatch! (apply batch (concat with-actions on-start)))
                ;(with-groups rt invert-route parent-branch tx-groups :route app-route :post-tx nil)
                (let [tx-groups (->> (get-in (get-state) [::runtime/partitions branch :stage])
                                     (filter (fn [[uri tx]] (and (should-transact!? uri get-state) (not (empty? tx)))))
                                     (into {}))]
                  (if (and (nil? parent-branch) (not (empty? tx-groups)))
                    ; todo what if transact throws?
                    (transact! rt tx-groups dispatch! get-state
                               :post-tx (let [clear-uris (->> (keys tx-groups)
                                                              (map (fn [uri] [:reset-stage-uri branch uri nil]))
                                                              vec)]
                                          (concat clear-uris ; clear the uris that were transacted
                                                  [[:merge branch] ; merge the untransacted uris up
                                                   (discard-partition branch)])) ; clean up the partition
                               :route app-route)
                    (either/branch
                      (or (some-> app-route route/validate-route+) (either/right nil)) ; arbitrary user input, need to validate
                      (fn [e]
                        (dispatch! (batch [:merge branch]
                                          [:set-error e]
                                          (discard-partition branch))))
                      (fn [app-route]
                        (let [actions [[:merge branch]
                                       (when app-route
                                         ; what about local-basis? why not specify branch?
                                         [:partition-route nil app-route])
                                       (discard-partition branch)]]
                          (dispatch! (apply batch (conj actions [:hydrate!-start parent-branch])))
                          (hydrate-partition rt parent-branch dispatch! get-state)))))))))))

(defn reset-stage-uri [rt branch uri tx]
  (fn [dispatch! get-state]
    ; check if auto-tx is OFF first?
    (when (not= tx (get-in (get-state) [::runtime/partitions branch :stage uri]))
      (dispatch! (batch [:reset-stage-uri branch uri tx] [:hydrate!-start nil]))
      (hydrate-partition rt nil dispatch! get-state))))

(defn manual-transact-uri! [rt nil-branch-aux uri]
  (fn [dispatch! get-state]
    ; todo do something when child branches exist and are not nil: hyperfiddle/hyperfiddle#99
    ; can only transact one branch
    (let [tx-groups (-> (get-in (get-state) [::runtime/partitions nil :stage])
                        (select-keys [uri]))]
      (transact! rt tx-groups dispatch! get-state :post-tx [[:reset-stage-uri nil uri]]))))
