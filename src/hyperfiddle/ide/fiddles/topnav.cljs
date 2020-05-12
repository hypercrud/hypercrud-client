(ns hyperfiddle.ide.fiddles.topnav
  (:require
    [contrib.reactive :as r]
    [contrib.ui.tooltip :refer [tooltip]]
    [hyperfiddle.api :as hf]
    [hyperfiddle.data]
    [hyperfiddle.foundation :as foundation]
    [hyperfiddle.ide.directory :as ide-directory]
    [hyperfiddle.ide.routing :as ide-routing]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui :as ui]
    [hyperfiddle.ui.error :as ui-error]))




(defn loading-spinner [ctx & [?class]]
  (if (runtime/any-partition-loading? (:runtime ctx))
    [:div.display-inline-flex [re-com.core/throbber]]))

(defmethod hf/tx ::new-fiddle-tx #_#{:hyperfiddle.ide/topnav-new
                                     :hyperfiddle.ide/new-fiddle}
  [ctx [e a v] props]
  [[:db/add v :fiddle/type :query]])

(defn topnav-new-wrapper-render [_ ctx props]
  ; iframe wrapper for naked qfind color tag
  [ui/link :hyperfiddle.ide/new-fiddle ctx "new"
   (let [disabled? (-> ctx
                       ; KAH: no idea on the appropriate way to inject $
                       ; hack so hf/subject-may-create? can call context/dbname and get "$"
                       (assoc :hypercrud.browser/element (r/pure {:source {:symbol '$}})) ; we explicitly know the context here is $
                     hf/subject-may-create? not)
         anonymous? (nil? (runtime/get-user-id (:runtime ctx)))]
     {:disabled disabled?
      :tooltip (cond
                 (and anonymous? disabled?) [:warning "Please login"]
                 disabled? [:warning "Writes restricted"])
      :hyperfiddle.ui.popover/redirect (fn [popover-data]
                                         (ide-routing/preview-route->ide-route {::route/fiddle (:fiddle/ident popover-data)}))})])

(defn route->fiddle-label [{:keys [::route/fiddle] :as route}]
  (cond
    (= fiddle :hyperfiddle.ide/edit) (let [user-fiddle-ident (get-in route [::route/datomic-args 0])]
                                       (str #_"edit: "
                                         (if (and (coll? user-fiddle-ident) (= :fiddle/ident (first user-fiddle-ident)))
                                           (let [user-fiddle-ident (second user-fiddle-ident)]
                                             (if (keyword? user-fiddle-ident)
                                               (name user-fiddle-ident)
                                               user-fiddle-ident))
                                           user-fiddle-ident)))
    (keyword? fiddle) (name fiddle)
    (string? fiddle) fiddle
    :else (pr-str fiddle)))

(defn renderer' [ctx props left-child right-child]
  [:div props
   [:div.left-nav
    [tooltip {:label "Home"} [:a {:href "/"}
                              (or (-> (hf/domain (:runtime ctx)) ::ide-directory/app-domain-ident) "Home")]]
    (let [props {:tooltip [nil "Fiddles in this domain"]
                 :iframe-as-popover true}]
      [ui/link :hyperfiddle.ide/entry-point-fiddles ctx "index" props])
    left-child]

   [:div.right-nav {:key "right-nav"}                       ; CAREFUL; this key prevents popover flickering
    [loading-spinner ctx]
    right-child
    [hyperfiddle.ui/link :hyperfiddle.ide/topnav-new
     ctx nil {:hyperfiddle.ui/error-render-custom ui-error/error-inline
              :user-renderer topnav-new-wrapper-render}]
    [tooltip {:label "Environment administration"} (ui/link :hyperfiddle.ide/env ctx "env")]
    (when (-> (hf/domain (:runtime ctx)) (hf/database "$users"))
      (if (hf/subject ctx)
        [ui/link :hyperfiddle.ide/account ctx]
        [:a {:href (hyperfiddle.ide/stateless-login-url ctx)} "login"]))]])

(defn hack-login-renderer [ctx props]
  [:div props
   [:div.left-nav
    [tooltip {:label "Home"} [:a (or (-> (hf/domain (:runtime ctx)) ::ide-directory/app-domain-ident) "Home")]]
    [:span (route->fiddle-label (runtime/get-route (:runtime ctx) foundation/root-pid))]]
   [:div.right-nav {:key "right-nav"}                       ; CAREFUL; this key prevents popover flickering
    [loading-spinner ctx]]])

(defn renderer [_ ctx {:keys [::left-child ::right-child] :as props}]
  (let [f (if (= :hyperfiddle.ide/please-login (::route/fiddle (runtime/get-route (:runtime ctx) foundation/root-pid)))
            hack-login-renderer
            renderer')
        left-child (or left-child [:span (route->fiddle-label (runtime/get-route (:runtime ctx) foundation/root-pid))])]
    [f ctx (dissoc props ::left-child ::right-child)
     left-child right-child]))
