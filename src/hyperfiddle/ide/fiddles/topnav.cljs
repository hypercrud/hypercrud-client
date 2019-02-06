(ns hyperfiddle.ide.fiddles.topnav
  (:require
    [clojure.string :as string]
    [contrib.reactive :as r]
    [contrib.ui.tooltip :refer [tooltip]]
    [hyperfiddle.data]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security.client :as security]
    [hyperfiddle.ui :as ui]))


(defn any-loading? [peer]
  (some (comp not nil? :hydrate-id val) @(runtime/state peer [::runtime/partitions])))

(defn loading-spinner [ctx & [?class]]
  (if @(r/track any-loading? (:peer ctx))
    [:div.display-inline-flex [re-com.core/throbber]]))

(defn renderer' [value ctx props]
  [:div props
   [:div.left-nav
    [tooltip {:label "Home"} [:a {:href "/"} (:app-domain-ident (runtime/domain (:peer ctx)))]]
    (let [props {:tooltip [nil "Fiddles in this domain"]
                 :iframe-as-popover true}]
      [ui/link :fiddle-shortcuts ctx "index" props])
    [:span (let [[fiddle-ident :as route] @(runtime/state (:peer ctx) [::runtime/partitions nil :route])]
             (cond
               (= fiddle-ident :hyperfiddle.ide/edit) (let [[_ [user-fiddle-ident]] route]
                                                        (str "edit: "
                                                             (if (and (coll? user-fiddle-ident) (= :fiddle/ident (first user-fiddle-ident)))
                                                               (let [user-fiddle-ident (second user-fiddle-ident)]
                                                                 (if (keyword? user-fiddle-ident)
                                                                   (name user-fiddle-ident)
                                                                   user-fiddle-ident))
                                                               user-fiddle-ident)))
               (keyword? fiddle-ident) (name fiddle-ident)
               :else fiddle-ident))]]

   [:div.right-nav {:key "right-nav"}                       ; CAREFUL; this key prevents popover flickering
    [loading-spinner ctx]
    (let [tooltip [:div {:style {:text-align "left"}}
                   [ui/markdown
                    (->> (runtime/domain (:peer ctx))
                         domain/databases
                         keys
                         sort
                         (map (fn [dbname]
                                (let [prefix (if @(runtime/state (:peer ctx) [::runtime/auto-transact dbname])
                                               "- [x] "
                                               "- [ ] ")]
                                  (str prefix dbname))))
                         (string/join "\n")
                         (str "##### Auto-transact:\n\n"))
                    {:hyperfiddle.ui.markdown-extensions/unp true}]]
          props {:tooltip [nil tooltip]
                 :class (when-not @(r/fmap empty? (runtime/state (:peer ctx) [::runtime/partitions nil :stage]))
                          "stage-dirty")
                 :iframe-as-popover true}]
      [ui/link :stage ctx "stage" props])
    (ui/link :new-fiddle ctx "new" (let [disabled? (not (security/can-create? ctx)) ; we explicitly know the context here is $
                                         anonymous? (nil? @(runtime/state (:peer ctx) [::runtime/user-id]))]
                                     {:disabled disabled?
                                      :tooltip (cond
                                                 (and anonymous? disabled?) [:warning "Please login"]
                                                 disabled? [:warning "Writes restricted"])
                                      :hyperfiddle.ui.popover/redirect (fn [popover-data]
                                                                         [(:fiddle/ident popover-data)])}))
    [tooltip {:label "Environment administration"} (ui/link :domain ctx "env")]
    (if @(runtime/state (:peer ctx) [::runtime/user-id])
      (if-let [{:keys [:hypercrud.browser/data]} (hyperfiddle.data/browse ctx :account)]
        (let [props {:tooltip [nil @(r/fmap :user/email data)]
                     :iframe-as-popover true}]
          [ui/link :account ctx @(r/fmap :user/name data) props]))
      [:a {:href (hyperfiddle.ide/stateless-login-url ctx)} "login"])]])

(defn hack-login-renderer [value ctx props]
  [:div props
   [:div.left-nav
    [tooltip {:label "Home"} [:a (domain/ident (runtime/domain (:peer ctx)))]]]
   [:div.right-nav {:key "right-nav"}                       ; CAREFUL; this key prevents popover flickering
    [loading-spinner ctx]]])

(defn renderer [value ctx props]
  (let [f (if (= :hyperfiddle.ide/please-login (first @(runtime/state (:peer ctx) [::runtime/partitions nil :route])))
            hack-login-renderer
            renderer')]
    [f value ctx props]))

(def ^:deprecated stage-ui-buttons hyperfiddle.ui.staging/stage-ui-buttons)
