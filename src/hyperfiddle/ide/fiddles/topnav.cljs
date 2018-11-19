(ns hyperfiddle.ide.fiddles.topnav
  (:require
    [cats.monad.either :as either]
    [clojure.string :as string]
    [contrib.reactive :as r]
    [contrib.ui :refer [easy-checkbox]]
    [contrib.ui.tooltip :refer [tooltip]]
    [hypercrud.browser.context :as context]
    [hyperfiddle.actions :as actions]
    [hyperfiddle.data]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security.client :as security]
    [hyperfiddle.ui :as ui :refer [markdown]]
    [hyperfiddle.ui.controls :refer [radio-group]]))


(defn any-loading? [peer]
  (some (comp not nil? :hydrate-id val) @(runtime/state peer [::runtime/partitions])))

(defn loading-spinner [ctx & [?class]]
  (if @(r/track any-loading? (:peer ctx))
    [:div.display-inline-flex [re-com.core/throbber]]))

(defn renderer [val ctx props]
  (let [target-route (context/target-route ctx)]
    [:div props
     [:div.left-nav
      [tooltip {:label "Home"} [:a {:href "/"} @(runtime/state (:peer ctx) [::runtime/domain :domain/ident])]]
      (let [fiddle-ident (first target-route)
            ; domain editor doesn't target a fiddle at all, and some fiddles are named by uuid
            fiddle-name (if (keyword? fiddle-ident) (name fiddle-ident))]
        [tooltip {:label fiddle-name}
         [:span fiddle-name]])
      (let [props {:tooltip [nil "Fiddles in this domain"]
                   :iframe-as-popover true}]
        [ui/link :hf/iframe :fiddle-shortcuts ctx "index" props])]

     [:div.right-nav {:key "right-nav"}                     ; CAREFUL; this key prevents popover flickering
      [loading-spinner ctx]
      (let [tooltip [:div {:style {:text-align "left"}}
                     [markdown
                      (->> @(runtime/state (:peer ctx) [::runtime/domain :domain/databases])
                           (reduce (fn [acc {:keys [:domain.database/name :domain.database/record]}]
                                     (if (contains? acc (:database/uri record))
                                       (update acc (:database/uri record) conj name)
                                       (assoc acc (:database/uri record) [name])))
                                   {@(runtime/state (:peer ctx) [::runtime/domain :domain/fiddle-database :database/uri]) ["Source"]})
                           (map (fn [[uri dbnames]]
                                  (let [prefix (if @(runtime/state (:peer ctx) [::runtime/auto-transact uri])
                                                 "- [x] "
                                                 "- [ ] ")]
                                    (str prefix (string/join "/" dbnames) " (" uri ")"))))
                           (string/join "\n")
                           (str "##### Auto-transact:\n\n"))
                      {:hyperfiddle.ui.markdown-extensions/unp true}]]
            props {:tooltip [nil tooltip]
                   :class (when-not @(r/fmap empty? (runtime/state (:peer ctx) [::runtime/partitions nil :stage]))
                            "stage-dirty")
                   :iframe-as-popover true}]
        [ui/link :hf/iframe :stage ctx "stage" props])
      (ui/link :hf/new :new-fiddle ctx "new" (let [disabled? (not (security/can-create? ctx)) ; we explicitly know the context here is $
                                                   anonymous? (nil? @(runtime/state (:peer ctx) [::runtime/user-id]))]
                                               {:disabled disabled?
                                                :tooltip (cond
                                                           (and anonymous? disabled?) [:warning "Please login"]
                                                           disabled? [:warning "Writes restricted"])
                                                :hyperfiddle.ui.popover/redirect (fn [popover-data]
                                                                                   [(:fiddle/ident popover-data)])}))
      [tooltip {:label "Environment administration"} (ui/link :hf/rel :domain ctx "env")]
      (if @(runtime/state (:peer ctx) [::runtime/user-id])
        (if-let [{:keys [:hypercrud.browser/data]} (hyperfiddle.data/browse ctx :hf/iframe :account)]
          (let [props {:tooltip [nil @(r/fmap :user/email data)]
                       :iframe-as-popover true}]
            [ui/link :hf/iframe :account ctx @(r/fmap :user/name data) props]))
        [:a {:href (foundation/stateless-login-url ctx)} "login"])]]))

(defn ^:export qe-picker-control [val ctx props]            ; not topnav
  (let [props (assoc props
                :class "qe"
                :options [{:value :query :label "query"}
                          {:value :entity :label "pull"}
                          {:value :blank :label "static"}])]
    [radio-group val ctx props]))

(letfn [(toggle-auto-transact! [ctx selected-uri]
          (runtime/dispatch! (:peer ctx) [:toggle-auto-transact @selected-uri]))]
  (defn ^:export stage-ui-buttons [selected-uri stage ctx]
    (let [writes-allowed?+ (let [hf-db (domain/uri->hfdb @selected-uri @(runtime/state (:peer ctx) [::runtime/domain]))
                                 subject @(runtime/state (:peer ctx) [::runtime/user-id])
                                 user @(runtime/state (:peer ctx) [::runtime/user])]
                             (security/subject-can-transact? hf-db subject user))
          anonymous? (nil? @(runtime/state (:peer ctx) [::runtime/user-id]))]
      [:<>
       [tooltip (either/branch
                  writes-allowed?+
                  (fn [e] {:status :warning :label "Misconfigured db security"})
                  (fn [writes-allowed?]
                    (cond (and (not writes-allowed?) anonymous?) {:status :warning :label "Please login"}
                          (not writes-allowed?) {:status :warning :label "Writes restricted"}
                          (empty? @stage) {:status :warning :label "no changes"})))
        (let [disabled? (either/branch
                          writes-allowed?+
                          (constantly true)
                          (fn [writes-allowed?] (or (not writes-allowed?) (empty? @stage))))]
          [:button {:disabled disabled?
                    :style (if disabled? {:pointer-events "none"})
                    :on-click (fn []
                                (let [nil-branch-aux {:hyperfiddle.ide/foo "page"}
                                      action (actions/manual-transact-uri! (:peer ctx) nil-branch-aux @selected-uri)]
                                  (runtime/dispatch! (:peer ctx) action)))}
           "transact!"])]
       " "
       [tooltip (either/branch
                  writes-allowed?+
                  (fn [e] {:status :warning :label "Misconfigured db security"})
                  (fn [writes-allowed?]
                    (cond (and anonymous? (not writes-allowed?)) {:status :warning :label "Please login"}
                          (not writes-allowed?) {:status :warning :label "Writes restricted"}
                          (not (empty? @stage)) {:status :warning :label "please transact! all changes first"})))
        (let [is-disabled (either/branch
                            writes-allowed?+
                            (constantly true)
                            (fn [writes-allowed?]
                              (or (not writes-allowed?) (not (empty? @stage)))))
              is-auto-transact @(runtime/state (:peer ctx) [::runtime/auto-transact @selected-uri])]
          [easy-checkbox {:disabled is-disabled
                          :style (if is-disabled {:pointer-events "none"})
                          :checked (boolean is-auto-transact)
                          :on-change (r/partial toggle-auto-transact! ctx selected-uri)}
           "auto-transact"])]])))
