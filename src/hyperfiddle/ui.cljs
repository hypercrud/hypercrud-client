(ns hyperfiddle.ui
  (:require-macros [hyperfiddle.ui :refer [-build-fiddle]])
  (:require
    [cats.core :as cats :refer [fmap mlet >>=]]
    [cats.monad.either :as either :refer [branch]]
    [clojure.core.match :refer [match match*]]
    [clojure.spec.alpha :as s]
    [clojure.string :as string]
    [contrib.ct :refer [unwrap]]
    [contrib.css :refer [css css-slugify]]
    [contrib.data :refer [map-keys map-values take-to]]
    [contrib.pprint :refer [pprint-str]]
    [contrib.reactive :as r]
    [contrib.string :refer [blank->nil]]
    [contrib.ui]
    [contrib.ui.safe-render :refer [user-portal]]
    [contrib.ui.tooltip :refer [tooltip tooltip-props]]
    [contrib.validation]
    [datascript.parser :refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]]
    [hypercrud.browser.context :as context]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.field :as field]
    [hypercrud.browser.link :as link]
    [hypercrud.browser.routing :as routing]
    [hypercrud.client.peer :as peer]
    [hypercrud.types.Err :as Err]
    [hypercrud.types.ThinEntity :refer [->ThinEntity]]
    [hypercrud.ui.connection-color :refer [connection-color]]
    [hypercrud.ui.error :as ui-error]
    [hypercrud.ui.stale :as stale]
    [hyperfiddle.data :as data]
    [hyperfiddle.fiddle :as fiddle]
    [hyperfiddle.ide.console-links :refer [inject-console-links system-link?]]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.security.client :as security]
    [hyperfiddle.tempid :refer [smart-entity-identifier consistent-relation-key stable-relation-key]]
    [hyperfiddle.ui.api]
    [hyperfiddle.ui.controls :as controls :refer [label-with-docs dbid-label magic-new]]
    [hyperfiddle.ui.docstring :refer [semantic-docstring]]
    [hyperfiddle.ui.popover :refer [effect-cmp popover-cmp]]
    [hyperfiddle.ui.select$]
    [hyperfiddle.ui.sort :as sort]
    [hyperfiddle.ui.util :as ui-util :refer [eval-renderer-comp writable-entity?]]
    [hypercrud.transit :as transit]
    [reagent.core :as reagent]))


(defn attr-renderer-control [val ctx & [props]]
  ; The only way to stabilize this is for this type signature to become a react class.
  (when-let [user-f (-> @(r/cursor (:hypercrud.browser/attr-renderers ctx) [(last (:hypercrud.browser/path ctx))])
                        blank->nil)]
    [user-portal (ui-error/error-comp ctx) nil
     ; ?user-f is stable due to memoizing eval (and only due to this)
     [eval-renderer-comp user-f val ctx props]]))

(declare result)
(declare pull)
(declare field)
(declare hyper-control)
(declare link)
(declare ui-from-link)
(declare fiddle-api)
(declare fiddle-xray)

(defn entity-links-iframe [val ctx & [props]]
  (->> (r/fmap->> (data/select-all-r ctx :hf/iframe)
                  (remove (r/comp (r/partial data/deps-over-satisfied? ctx) link/read-path :link/path)))
       (r/unsequence (r/partial stable-relation-key ctx))
       (map (fn [[rv k]]
              ^{:key k}
              [ui-from-link rv ctx props]))
       (into [:<>])))

(defn control "this is a function, which returns component"
  [val ctx & [props]]                                       ; returns Func[(ref, props, ctx) => DOM]
  (let [segment (last (:hypercrud.browser/path ctx))
        attr @(context/hydrate-attribute ctx segment)
        type (or (some-> attr :db/valueType :db/ident name keyword) (context/segment-type-2 segment)) ; can include :element ? :aggregate, :entity
        cardinality (some-> attr :db/cardinality :db/ident name keyword)]
    (match* [type cardinality]
      [:element _] controls/string                          ; FindRel-Variable
      #_#_[:aggregate _] controls/string                    ; entity, aggregate, what else?
      #_#_[:entity _] entity-links
      [:attribute _] (r/constantly (str "no schema for attr: " segment))

      [:splat _] magic-new
      [:boolean :one] controls/boolean
      [:keyword :one] controls/keyword
      [:string :one] controls/string
      [:long :one] controls/long
      [:instant :one] controls/instant
      [:ref :one] controls/ref
      [:ref :many] controls/edn-many                        ; multi select
      [_ :one] controls/edn
      [_ :many] controls/edn-many)))

(defn ^:export hyper-control [val ctx & [props]]
  {:post [%]}
  (or (attr-renderer-control val ctx props)
      (let [-field @(:hypercrud.browser/field ctx)]
        (cond                                               ; Duplicate options test to avoid circular dependency in controls/ref
          (:options props) [(control val ctx props) val ctx props]
          (field/identity-segment? -field) [controls/id-or-ident val ctx props]
          (field/children-identity-only? -field) [(control val ctx props) val ctx props]
          (seq (::field/children -field)) (let [ctx (dissoc ctx ::layout)]
                                            [:div           ; wrapper div: https://github.com/hyperfiddle/hyperfiddle/issues/541
                                             [pull field val ctx props]
                                             [field [] ctx entity-links-iframe (assoc props :label-fn (r/constantly nil #_[:div "nested pull iframes"]))]])
          :else [(control val ctx props) val ctx props]))))

(defn ^:export hyper-label [_ ctx & [props]]
  (let [field @(:hypercrud.browser/field ctx)
        level (::field/level field)
        segment (last (:hypercrud.browser/path ctx))
        segment-type (context/segment-type-2 segment)       ; :element means :relation? no, naked. Relation is ortho
        has-child-fields @(r/fmap-> (:hypercrud.browser/field ctx) ::field/children nil? not)]
    (cond
      (field/identity-segment? field) (dbid-label _ ctx props)
      :else (match* [level segment-type segment has-child-fields]
              [nil :splat _ _] (label-with-docs (::field/label field) (semantic-docstring ctx "Free-hand attribute entry") props)
              [nil :attribute _ _] (label-with-docs (::field/label field) (semantic-docstring ctx) props)
              [nil :element _ true] (label-with-docs "*relation*" (semantic-docstring ctx) props)
              [nil :element _ false] (label-with-docs (::field/label field) (semantic-docstring ctx) props)
              [nil :naked-or-element _ _] (label-with-docs "*relation*" (semantic-docstring ctx) props) ; Schema new attr, and fiddle-links new link - needs to be split
              [:relation :naked-or-element _ _] (label-with-docs "*relation*" (semantic-docstring ctx) props)
              [:tuple :naked-or-element _ _] (label-with-docs "*tuple*" (semantic-docstring ctx) props)))))

(defn auto-link-css [link]
  (->> (:link/class link)
       (interpose " ")
       (apply str)))

(defn ^:export semantic-css [ctx]
  ; Include the fiddle level ident css.
  ; Semantic css needs to be prefixed with - to avoid collisions. todo
  (->> (:hypercrud.browser/path ctx)
       (concat
         ["hyperfiddle"
          (context/dbname ctx)                              ; color
          (name (context/segment-type-2 (last (:hypercrud.browser/path ctx))))
          (->> (:hypercrud.browser/path ctx)                ; legacy unique selector for each location
               (map css-slugify)
               (string/join "/"))
          (->> (:hypercrud.browser/path ctx)                ; actually generate a unique selector for each location
               (cons :hypercrud.browser/path)               ; need to prefix the path with something to differentiate between attr and single attr paths
               (map css-slugify)
               (string/join "/"))]
         (when (context/attribute-segment? (last (:hypercrud.browser/path ctx)))
           (let [attr-ident (last (:hypercrud.browser/path ctx))
                 attr (context/hydrate-attribute ctx attr-ident)]
             [@(r/cursor attr [:db/valueType :db/ident])
              @(r/cursor (:hypercrud.browser/attr-renderers ctx) [attr-ident])  #_label/fqn->name
              @(r/cursor attr [:db/cardinality :db/ident])
              (some-> @(r/cursor attr [:db/isComponent]) (if :component))])))
       (map css-slugify)))

(defn ^:export value "Relation level value renderer. Works in forms and lists but not tables (which need head/body structure).
User renderers should not be exposed to the reaction."
  ; Path should be optional, for disambiguation only. Naked is an error
  [relative-path ctx ?f & [props]]                          ; ?f :: (ref, props, ctx) => DOM
  (let [ctx (context/focus ctx relative-path)
        props (update props :class css (semantic-css ctx))]
    [(or ?f hyper-control) @(:hypercrud.browser/data ctx) ctx props]))

(defn ^:export anchor [ctx props & children]
  (let [props (-> props
                  (update :class css "hyperfiddle")
                  (dissoc :route)
                  (assoc :href (some->> (:route props) (hyperfiddle.foundation/route-encode (:peer ctx)))))]
    (into [:a props] children)))

(letfn [(auto-ui-css-class [ctx]                            ; semantic css
          (css (let [ident @(r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/ident])]
                 ["hyperfiddle"
                  (css-slugify (some-> ident namespace))
                  (css-slugify ident)])))
        (build-wrapped-render-expr-str [user-str] (str "(fn [val ctx props]\n" user-str ")"))
        (ui-comp [ctx & [props]]                            ; user-renderer comes through here
          (let [fiddle (:hypercrud.browser/fiddle ctx)
                value @(:hypercrud.browser/data ctx)
                ctx (if-let [spec (s/get-spec (:fiddle/ident @fiddle))]
                      (assoc ctx :hypercrud.browser/validation-hints (contrib.validation/validate spec value (partial data/row-keyfn ctx)))
                      ctx)
                props (update props :class css (auto-ui-css-class ctx))
                view-props (select-keys props [:class :initial-tab :on-click #_:disabled]) ; https://github.com/hyperfiddle/hyperfiddle/issues/698
                display-mode @(:hypercrud.ui/display-mode ctx)
                error-props (-> (select-keys props [:class :on-click])
                                (update :class css "hyperfiddle-error"))]
            ^{:key (str display-mode)}
            [user-portal (ui-error/error-comp ctx) error-props
             (case display-mode
               :hypercrud.browser.browser-ui/user (if-let [user-renderer (:user-renderer props)]
                                                    [user-renderer value ctx view-props]
                                                    [:<>
                                                     [ui-util/eval-cljs-ns fiddle]
                                                     [eval-renderer-comp
                                                      (build-wrapped-render-expr-str (:fiddle/renderer @fiddle))
                                                      value ctx view-props
                                                      ; If userland crashes, reactions don't take hold, we need to reset here.
                                                      ; Cheaper to pass this as a prop than to hash everything
                                                      ; Userland will never see this param as it isn't specified in the wrapped render expr.
                                                      @fiddle
                                                      ]])
               :hypercrud.browser.browser-ui/xray [fiddle-xray value ctx view-props]
               :hypercrud.browser.browser-ui/api [fiddle-api value ctx view-props])]))
        (fiddle-css-renderer [s] [:style {:dangerouslySetInnerHTML {:__html @s}}])
        (src-mode [route ctx]
          (mlet [ctx (-> (context/clean ctx)
                         (routing/route+ route))
                 request @(r/apply-inner-r (r/track base/meta-request-for-fiddle ctx))
                 :let [fiddle (let [fiddle {:fiddle/type :entity
                                            :fiddle/pull-database "$"}]
                                ; turns out we dont need fiddle for much if we already know the request
                                (r/track fiddle/apply-defaults fiddle))]
                 ctx (-> (context/source-mode ctx)
                         (context/clean)
                         (routing/route+ [nil [(->ThinEntity "$" [:fiddle/ident @(r/fmap first (:hypercrud.browser/route ctx))])]]))]
            (base/process-results fiddle request ctx)))]
  (defn ^:export iframe [ctx {:keys [route hf-live] :as props}] ; :: [route ctx & [?f props]]
    (let [click-fn (or (:hyperfiddle.ui/iframe-on-click ctx) (constantly nil)) ; parent ctx receives click event, not child frame
          either-v (or (some-> @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :error]) either/left)
                       (if hf-live
                         (src-mode route ctx)
                         (base/data-from-route route ctx)))
          error-comp (ui-error/error-comp ctx)
          props (dissoc props :route)]
      [stale/loading (stale/can-be-loading? ctx) either-v
       (fn [e]
         (reagent/after-render
           (fn []
             (when (and (exists? js/Sentry)                 ; todo hide behind interface on runtime
                        (peer/loading? e)
                        (not @(stale/can-be-loading? ctx)))
               (.withScope js/Sentry (fn [scope]
                                       (.setExtra scope "ex-data" (clj->js (ex-data e)))
                                       (.setExtra scope "route" (pr-str route))
                                       (.setExtra scope "global-basis" (->> @(runtime/state (:peer ctx) [::runtime/global-basis])
                                                                            (map-values #(map-keys str %))
                                                                            (clj->js)))
                                       (.setExtra scope "branch-ident" (clj->js (:branch ctx)))
                                       (.setExtra scope "branch-state" (-> @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx)])
                                                                           (select-keys [:route :local-basis :ptm])
                                                                           (update :ptm keys)
                                                                           (transit/encode)))
                                       (.captureMessage js/Sentry (str (cond
                                                                         (Err/Err? e) (:msg e)
                                                                         (map? e) (:message e)
                                                                         (string? e) e
                                                                         :else (ex-message e)))))))))
         [error-comp e {:class (css "hyperfiddle-error" (:class props) "ui")
                        :on-click (r/partial click-fn route)}])
       (fn [ctx]                                            ; fresh clean ctx
         [:<>
          [ui-comp ctx (-> (update props :class css "ui")
                           (assoc :on-click (r/partial click-fn @(:hypercrud.browser/route ctx))))]
          [fiddle-css-renderer (r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])]])
       (fn [ctx]
         [:<>
          [ui-comp ctx (-> (update props :class css "hyperfiddle-loading" "ui")
                           ; use the stale ctx's route, otherwise alt clicking while loading could take you to the new route, which is jarring
                           (assoc :on-click (r/partial click-fn @(:hypercrud.browser/route ctx))))]
          [fiddle-css-renderer (r/cursor (:hypercrud.browser/fiddle ctx) [:fiddle/css])]])])))

(letfn [(prompt [link-ref ?label]
          (or ?label (->> (conj (set @(r/fmap :link/class link-ref))
                                @(r/fmap :link/rel link-ref))
                          (interpose " ") (apply str) blank->nil)))
        (link-tooltip [{:keys [:link/rel :link/class]} ?route]
          (if ?route
            (let [[_ args] ?route]
              (->> (concat class [rel] args) (map pr-str) (interpose " ") (apply str)))))
        (validated-route-tooltip-props [r+?route link-ref ctx props] ; link is for validation
          ; this is a fine place to eval, put error message in the tooltip prop
          ; each prop might have special rules about his default, for example :visible is default true, does this get handled here?
          (let [+route (>>= @r+?route #(routing/validated-route+ (:link/fiddle @link-ref) % ctx))
                errors (->> [+route] (filter either/left?) (map cats/extract) (into #{}))
                ?route (unwrap (constantly nil) +route)]
            (-> props
                (assoc :route ?route)
                (update :tooltip (fn [existing-tooltip]
                                   (if-not (empty? errors)
                                     [:warning (pprint-str errors)]
                                     (if (:hyperfiddle.ui/debug-tooltips ctx)
                                       [nil (link-tooltip @link-ref ?route)]
                                       existing-tooltip))))
                (update :class css (when-not (empty? errors) "invalid")))))
        (disabled? [link-ref ctx]
          (case @(r/fmap :link/rel link-ref)
            :hf/new nil #_(not @(r/track security/can-create? ctx))
            :hf/remove (not @(r/track security/writable-entity? ctx))
            :hf/affix (not @(r/track security/writable-entity? (:hypercrud.browser/parent ctx)))
            :hf/detach (not @(r/track security/writable-entity? (:hypercrud.browser/parent ctx)))
            :hf/self nil
            :hf/iframe nil
            :hf/rel nil
            ; else we don't know the semantics, just nil out
            nil))]
  (defn ui-from-link [link-ref ctx & [props ?label]]
    {:pre [link-ref]}
    (let [visual-ctx ctx
          ctx (context/refocus ctx (link/read-path @(r/fmap :link/path link-ref)))
          error-comp (ui-error/error-comp ctx)
          +args @(r/fmap->> link-ref (routing/build-args+ ctx))
          r+?route (r/fmap->> link-ref (routing/build-route' +args ctx)) ; need to re-focus from the top
          eav [(some-> ctx :hypercrud.browser/parent hypercrud.browser.context/id) ; Todo move into refocus. Also might not have one, txfn understands this
               (last (:hypercrud.browser/path ctx))         ; todo chop off FE todo
               (->> +args (unwrap (constantly nil)) first :db/id)]                               ; Todo for :hf/new, the focused data is now eschewed in favor of this new tempid
          style {:color nil #_(connection-color ctx (cond (system-link? (:db/id @link-ref)) 60 :else 40))}
          props (update props :style #(or % style))
          has-tx-fn @(r/fmap-> link-ref :link/tx-fn blank->nil boolean)
          is-iframe @(r/fmap-> link-ref :link/rel (= :hf/iframe))]
      (cond
        (and has-tx-fn @(r/fmap-> link-ref :link/fiddle nil?))
        (let [props (-> props
                        (update :class css "hyperfiddle")
                        (update :disabled #(or % (disabled? link-ref ctx))))]
          [effect-cmp link-ref eav ctx props @(r/track prompt link-ref ?label)])

        (or has-tx-fn (and is-iframe (:iframe-as-popover props)))
        (let [props (-> @(r/track validated-route-tooltip-props r+?route link-ref ctx props)
                        (dissoc :iframe-as-popover)
                        (update :class css "hyperfiddle")   ; should this be in popover-cmp? what is this class? – unify with semantic css
                        (update :disabled #(or % (disabled? link-ref ctx))))
              label @(r/track prompt link-ref ?label)]
          [popover-cmp link-ref eav ctx visual-ctx props label])

        is-iframe
        [stale/loading (stale/can-be-loading? ctx) (fmap #(route/assoc-frag % (:frag props)) @r+?route) ; what is this frag noise?
         (fn [e] [error-comp e])
         (fn [route]
           (let [iframe (or (::custom-iframe props) iframe)]
             [iframe ctx (-> props                          ; flagged - :class
                             (assoc :route route)
                             (dissoc props ::custom-iframe)
                             (update :class css (css-slugify @(r/fmap auto-link-css link-ref))))]))]

        :else (let [props @(r/track validated-route-tooltip-props r+?route link-ref ctx props)]
                [tooltip (tooltip-props (:tooltip props))
                 (let [props (dissoc props :tooltip)]
                   ; what about class - flagged
                   ; No eav, the route is the same info
                   [anchor ctx props @(r/track prompt link-ref ?label)])])
        ))))

(defn ^:export link "Relation level link renderer. Works in forms and lists but not tables." ; this is dumb, use a field renderer
  [rel class ctx & [?label props]]                          ; path should be optional, for disambiguation only. Naked can be hard-linked in markdown?
  (either/branch
    (data/select+ ctx rel class)
    #(vector :span %)
    (fn [link-ref]
      [ui-from-link link-ref ctx props ?label])))

(defn ^:export browse "Relation level browse. Works in forms and lists but not tables."
  [rel class ctx & [?user-renderer props]]
  {:pre [ctx]}
  (let [props (if ?user-renderer
                (assoc props :user-renderer ?user-renderer)
                props)]
    (either/branch
      (data/select+ ctx rel class)
      #(vector :span %)
      (fn [link-ref]
        [ui-from-link link-ref ctx props]))))

(defn form-field "Form fields are label AND value. Table fields are label OR value."
  [relative-path ctx Body Head props]
  (with-meta
    [:div {:class (css "field" (:class props))
           :style {:border-color (connection-color ctx)}}
     [Head nil (dissoc ctx :hypercrud.browser/data) props]
     (let [props (update props :disabled #(or % (not @(r/track writable-entity? ctx))))
           props (update props :class css (if (:disabled props) "disabled"))]
       [Body @(:hypercrud.browser/data ctx) ctx props])]
    (when (= '* (last relative-path))                       ; :hypercrud.browser/path
      ; guard against crashes for nil data
      {:key @(r/fmap-> (or (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/data])
                           (r/track identity nil))
                       keys hash)})))

(defn table-field "Form fields are label AND value. Table fields are label OR value."
  [relative-path ctx Body Head props]                       ; Body :: (val props ctx) => DOM, invoked as component
  ; Presence of data to detect head vs body? Kind of dumb
  (case (if (:hypercrud.browser/data ctx) :body :head)      ; this is ugly and not uniform with form-field NOTE: NO DEREF ON THIS NIL CHECK
    :head [:th {:class (css "field" (:class props))         ; hoist
                :style {:background-color (connection-color ctx)}}
           [Head nil ctx (-> props
                             (update :class css (when (sort/sortable? ctx) "sortable") (some-> (sort/sort-direction relative-path ctx) name))
                             (assoc :on-click (r/partial sort/toggle-sort! relative-path ctx)))]]
    ; Field omits [] but table does not, because we use it to specifically draw repeating anchors with a field renderer.
    :body [:td {:class (css "field" (:class props) (if (context/invalid? ctx) "invalid"))
                :style {:border-color (connection-color ctx)}}
           (let [props (update props :disabled #(or % (not @(r/track writable-entity? ctx))))
                 props (update props :class css (if (:disabled props) "disabled"))]
             [Body @(:hypercrud.browser/data ctx) ctx props])]))

(defn ^:export field "Works in a form or table context. Draws label and/or value."
  [relative-path ctx & [?f props]]
  (let [ctx (context/focus ctx relative-path)
        Body (or ?f hyper-control)
        Head (or (:label-fn props) hyper-label)
        props (dissoc props :label-fn)
        props (update props :class css (semantic-css ctx))
        is-magic-new (= '* (last relative-path))]
    (case (:hyperfiddle.ui/layout ctx)
      :hyperfiddle.ui.layout/table (when-not is-magic-new
                                     [table-field relative-path ctx Body Head props])
      [form-field relative-path ctx Body Head props])))

(defn ^:export table "Semantic table; columns driven externally" ; this is just a widget
  [columns ctx & [props]]
  (let [sort-col (r/atom (::sort/initial-sort props))]
    (fn [columns ctx & [props]]
      (let [props (dissoc props ::sort/initial-sort)
            ctx (assoc ctx ::sort/sort-col sort-col
                           ::layout :hyperfiddle.ui.layout/table)]
        [:table (update props :class (fnil css "hyperfiddle") "unp") ; fnil case is iframe root (not a field :many)
         [:thead (->> (columns (dissoc ctx :hypercrud.browser/data) props) (into [:tr]))] ; strict
         (->> (r/fmap-> (:hypercrud.browser/data ctx)
                        (sort/sort-fn sort-col))
              (r/unsequence (r/partial data/row-keyfn ctx))
              (map (fn [[row k]]
                     (->> (columns (context/row ctx row k) props)
                          (into ^{:key k} [:tr]))))         ; strict
              (into [:tbody]))]))))

(defn hint [val {:keys [hypercrud.browser/fiddle] :as ctx} props]
  (if (and (-> (:fiddle/type @fiddle) (= :entity))
           (nil? (smart-entity-identifier ctx val)))
    [:div.alert.alert-warning "Warning: invalid route (d/pull requires an entity argument). To add a tempid entity to the URL, click here: "
     [:a {:href "~entity('$','tempid')"} [:code "~entity('$','tempid')"]] "."]))

(defn form "Not an abstraction." [fields val ctx & [props]]
  (into [:<> {:key (str (data/row-keyfn ctx val))}]
        (fields (assoc ctx ::layout :hyperfiddle.ui.layout/block))))

(defn columns [m-field relative-path field ctx & [props]]
  (concat
    (->> @(r/fmap->> m-field ::field/children (map ::field/path-segment))
         (map (fn [child-segment]
                (let [relative-path (conj relative-path child-segment)]
                  ^{:key (str relative-path)}
                  [field relative-path ctx hyper-control props]))))
    (when-let [f (condp = @(r/fmap ::field/element-type m-field)
                   Variable hyper-control
                   Aggregate hyper-control
                   Pull nil #_entity-links
                   ; else nested pulls
                   nil)]
      [^{:key (str relative-path)} [field relative-path ctx f props]])))

(defn columns-relation-product [field ctx & [props]]
  (->> (r/fmap ::field/children (:hypercrud.browser/field ctx))
       (r/unsequence ::field/path-segment)
       (mapcat (fn [[m-field segment]]
                 (columns m-field [segment] field ctx props)))))

(defn pull "handles any datomic result that isn't a relation, recursively"
  [field val ctx & [props]]
  (let [cardinality @(r/fmap ::field/cardinality (:hypercrud.browser/field ctx))]
    (match* [cardinality]
      [:db.cardinality/one] [form (r/partial columns (:hypercrud.browser/field ctx) [] field) val ctx props]
      [:db.cardinality/many] [table (r/partial columns (:hypercrud.browser/field ctx) [] field) ctx props])))

(defn ^:export result "Default result renderer. Invoked as fn, returns seq-hiccup, hiccup or
nil. call site must wrap with a Reagent component"          ; is this just hyper-control ?
  [val ctx & [props]]
  [:<>
   (hint val ctx props)
   (let [type @(r/fmap :fiddle/type (:hypercrud.browser/fiddle ctx))
         level @(r/fmap ::field/level (:hypercrud.browser/field ctx))]
     (match* [type level]
       [:blank _] nil
       [:query :relation] [table (r/partial columns-relation-product field) ctx props]
       [:query :tuple] [form (r/partial columns-relation-product field) val ctx props]
       [_ _] (pull field val ctx props)))])

(def ^:dynamic markdown)                                    ; this should be hf-contrib or something

(def ^:export fiddle (-build-fiddle))

(defn ^:export fiddle-xray [val ctx & [props]]
  (let [ctx (inject-console-links ctx)]
    [:div (select-keys props [:class])
     [:h3 (pr-str @(:hypercrud.browser/route ctx))]
     (result val ctx {})
     [field [] ctx entity-links-iframe (assoc props :label-fn (r/constantly nil))]]))

(letfn [(render-edn [data]
          (let [edn-str (pprint-str data 160)]
            [contrib.ui/code {:value edn-str :read-only true}]))]
  (defn ^:export fiddle-api [val ctx & [props]]
    (let [data (hyperfiddle.ui.api/api-data ctx)]
      [:div.hyperfiddle.display-mode-api.container-fluid (select-keys props [:class])
       [:h3
        [:dl
         [:dt "route"] [:dd (pr-str @(:hypercrud.browser/route ctx))]]]
       (render-edn (get data @(:hypercrud.browser/route ctx)))
       (->> (dissoc data @(:hypercrud.browser/route ctx))
            (map (fn [[route result]]
                   ^{:key (str (hash route))}
                   [:div
                    [:dl [:dt "route"] [:dd (pr-str route)]]
                    (render-edn result)])))])))

(defn ^:export img [val ctx & [props]]
  [:img (merge props {:src val})])
