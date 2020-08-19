(ns hyperfiddle.ui
  (:require
    [cats.core :as cats :refer [>>= fmap]]
    [cats.monad.either :as either]
    [cljs.spec.alpha :as s]
    [clojure.core.match :refer [match*]]
    [clojure.string :as string]
    [contrib.css :refer [css css-slugify]]
    [contrib.data :refer [unqualify orf]]
    [contrib.hfrecom]
    [contrib.pprint :refer [pprint-str]]
    [contrib.reactive :as r]
    [contrib.string :refer [blank->nil]]
    [contrib.ui]
    [contrib.ui.safe-render :refer [user-portal]]
    [contrib.ui.tooltip :refer [tooltip tooltip-props]]
    [contrib.eval :as eval]
    [cuerdas.core :as str]
    [datascript.parser :refer [FindRel FindColl FindTuple FindScalar]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hyperfiddle.api :as hf]
    [hyperfiddle.data :as data]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.spec :as spec]
    [hyperfiddle.ui.controls :as controls :refer [identity-label ref-label element-label]]
    [hyperfiddle.ui.error :as ui-error]
    [hyperfiddle.ui.iframe :as iframe]
    [hyperfiddle.ui.popover :refer [effect-cmp popover-cmp]]
    [hyperfiddle.ui.sort :as sort]
    [hyperfiddle.ui.stale :as stale]
    [hyperfiddle.ui.util :as ui-utils]
    [spec-coerce.alpha]))

(declare result)
(declare pull)
(declare field)
(declare hyper-control)
(declare link)
(declare ui-from-link)
(declare fiddle-api)
(declare fiddle-xray)

(defn entity-links-iframe [ctx & [props]]
  ; Remove links that are also available at our ancestors.
  ; http://hyperfiddle.hyperfiddle.site/:hyperfiddle.ide!domain/~entity('$domains',(:domain!ident,'hyperfiddle'))
  ; http://tank.hyperfiddle.site/:tutorial.race!submission/~entity('$',(:dustingetz.reg!email,'bob@example.com'))
  ; http://tank.hyperfiddle.site/:dustingetz.seattle!communities/
  ; http://tank.hyperfiddle.site/:seattle!neighborhoods/
  (if (or (= (context/depth ctx) 0)
          (and (> (context/depth ctx) 0)
               (let [[_ a _] (context/eav ctx)]
                 (not (contrib.datomic/ref-one? @(:hypercrud.browser/schema ctx) a)))))
    [:<>
     ; keep flip flopping. is it links-here, or in links-in-dimension ??
     ; http://domains.hyperfiddle.site/:domains!edit/~entity('$domains',(:domain!ident,'dustingetzcom'))
     ; http://tank.hyperfiddle.site/:demo.seattle!neighborhood/~entity('$',(:neighborhood!name,'Georgetown'))
     (for [[k r-link] (hyperfiddle.data/spread-links-here ctx :hf/iframe)]
       ^{:key k}
       [ui-from-link r-link ctx props])]))

(defn render-ref [ctx props]
  (let [children (context/children ctx)]
    (cond
      (and (seq children)
           (= 1 (count children))
           (every? (partial context/identity? ctx) children))
      ; This if statement is needed to flatten test case: http://hyperfiddle.hyperfiddle.site/:databases/
      ; Dude i have no idea why this if statement is necessary
      (if (context/attr? ctx :db.cardinality/many)
        [controls/ref-many (context/data ctx) ctx props]
        (let [[a] children                                  ; pick one, best if there is only one
              ctx (context/focus ctx [a])]
          (hf/render ctx props)))

      (seq children)
      (let [ctx (dissoc ctx ::layout)]
        [:<>
         [:div                                              ; wrapper div: https://github.com/hyperfiddle/hyperfiddle/issues/541
          [pull (context/data ctx) ctx props]]              ; account for hf/new at parent ref e.g. :community/neighborhood
         [entity-links-iframe ctx props]])

      (context/attr? ctx :db.cardinality/one)
      [controls/ref (context/data ctx) ctx props]

      (context/attr? ctx :db.cardinality/many)
      [controls/ref-many (context/data ctx) ctx props])))

(defmethod hf/render #{:db.type/ref :db.cardinality/one} [ctx props] (render-ref ctx props))
(defmethod hf/render #{:db.type/ref :db.cardinality/many} [ctx props] (render-ref ctx props))

(defmethod hf/render #{:hf/variable} [ctx props]
  [controls/string (context/data ctx) ctx props])

(defmethod hf/render #{:hf/aggregate} [ctx props]
  [controls/string (context/data ctx) ctx props])

(defmethod hf/render #{:db.unique/identity} [ctx props]
  [controls/identity-control (context/data ctx) ctx props])

(defmethod hf/render #{:db.type/boolean :db.cardinality/one} [ctx props]
  [controls/boolean (context/data ctx) ctx props])

(defmethod hf/render #{:db.type/keyword :db.cardinality/one} [ctx props]
  [controls/keyword (context/data ctx) ctx props])

(defmethod hf/render #{:db.type/string :db.cardinality/one} [ctx props]
  [controls/string (context/data ctx) ctx props])

(defmethod hf/render #{:db.type/symbol :db.cardinality/one} [ctx props]
  [controls/string (context/data ctx) ctx props])

(defmethod hf/render #{:db.type/long :db.cardinality/one} [ctx props]
  [controls/long (context/data ctx) ctx props])

(defmethod hf/render #{:db.type/instant :db.cardinality/one} [ctx props]
  [controls/instant (context/data ctx) ctx props])

(defmethod hf/render :default [ctx props]
  (cond
    (context/attr? ctx :db.cardinality/one)
    [controls/edn (context/data ctx) ctx props]

    (context/attr? ctx :db.cardinality/many)
    [controls/edn-many (context/data ctx) ctx props]

    :else
    [:pre (pr-str "render: no match for eav: " (context/eav ctx))]))

(defn ^:export hyper-control
  [ctx & [props]]
  {:post [%]}
  (hf/render ctx props))

(defn ^:export hyper-label [ctx & [props]]                ; props has sort :on-click
  (let [?element (:hypercrud.browser/element ctx)
        element-type (if ?element (contrib.datomic/parser-type @?element))
        i (:hypercrud.browser/element-index ctx)
        [_ a _] (context/eav ctx)
        attr (and a (context/attr ctx a))]
    (match* [i (unqualify element-type) a attr]             ; has-child-fields @(r/fmap-> (:hypercrud.browser/field ctx) ::field/children nil? not)
      [_ nil _ _] nil                                       ; e.g. !field[js/user.hyperblog-post-link]()
      [_ :pull :db/id _] (identity-label _ ctx props)
      [_ :pull :db/ident _] (identity-label _ ctx props)
      [_ :pull _ {:db/unique :db.unique/identity}] (identity-label _ ctx props)
      [i :pull nil _] (element-label _ ctx props)
      [_ :pull aa _] (ref-label _ ctx props)
      [_ :variable _ _] (element-label _ ctx props)
      [_ :aggregate _ _] (element-label _ ctx props))))


(defn ^:export semantic-css "Works at all levels: attr, element and fiddle."
  [ctx]
  ; Include the fiddle level ident css.
  ; Semantic css needs to be prefixed with - to avoid collisions. todo
  (let [[_ a _] @(:hypercrud.browser/eav ctx)]
    (->> (concat
           ["hyperfiddle"
            (context/dbname ctx)                            ; color
            (name (context/segment-type-2 a))               ; false :attribute on fiddle-ident fixme
            (string/join "/" (:hypercrud.browser/pull-path ctx)) ; legacy unique selector for each location
            (->> (:hypercrud.browser/pull-path ctx)         ; actually generate a unique selector for each location
                 (cons "-hypercrud-browser-path")           ; path prefix differentiates between attr and single attr paths
                 (string/join "/"))]
           (when (> (hypercrud.browser.context/pull-depth ctx) 0) ; differentiate pull from fiddle-ident
             [(contrib.datomic/valueType @(:hypercrud.browser/schema ctx) a)
              (contrib.datomic/cardinality @(:hypercrud.browser/schema ctx) a) ; probably have result here
              (if (contrib.datomic/isComponent @(:hypercrud.browser/schema ctx) a) :component)])
           (:hypercrud.browser/pull-path ctx))              ; pullpath is legacy, remove from css todo
         (map css-slugify)
         (apply css))))

(defn writable-entity? [ctx]
  ; If we have a component that's empty, we need to be able to write to it
  ; If the db/id was not pulled, we cannot write through to the entity
  (or
    (and (context/component? ctx)
         (writable-entity? (:hypercrud.browser/parent ctx)))
    (boolean (hf/e ctx))                                    ; you have some sort of entity identity (db/id, lookup ref, etc)
    ))

(defn- redirect-to-route?
  "State if changes to the currently focused attr should go to the route"
  [ctx]
  (let [spec (spec/spec ctx)
        attr (hf/a ctx)]
    (spec/arg? spec attr)))

(defn- disabled?
  "To be used with `clojure.core/update`."
  [disabled? ctx]
  (cond
    disabled?                true ; specified by user
    (redirect-to-route? ctx) false
    :else                    (or (not @(r/track writable-entity? ctx)) ; either the entity is readonly due to the fiddle query
                                 (not @(r/track hf/subject-may-edit-entity? (:hypercrud.browser/parent ctx)))
                                 (not @(r/track hf/subject-may-edit-attr? ctx)) ; or you failed security
                                 )))

(defn- value-props [props ctx]
  (let [r-validation-hints (r/track context/validation-hints-here ctx)]
    (as-> props props
          (update props :disabled disabled? ctx)
          (assoc props ::hf/invalid-messages r-validation-hints) ; why would this be null
          (assoc props ::hf/is-invalid (boolean (seq @r-validation-hints)))
          (update props :class css (if (:disabled props) "disabled")))))

(defn- maybe-redirect-to-route
  "Redirect view changes to route if applicable."
  [ctx]
  (if (redirect-to-route? ctx)
    (assoc ctx ::hf/view-change! ui-utils/with-entity-change-route!)
    ctx))

(defn ^:export value "Relation level value renderer. Works in forms and lists but not tables (which need head/body structure).
User renderers should not be exposed to the reaction."
  ; Path should be optional, for disambiguation only. Naked is an error
  [relative-path ctx ?f & [props]]                          ; ?f :: (ref, props, ctx) => DOM
  (let [ctx (context/focus ctx relative-path)
        props (-> (update props :class css (semantic-css ctx))
                  (value-props ctx))]
    [(or ?f hyper-control) ctx props]))

(defn ^:export anchor [ctx props & children]
  (let [route (route/legacy-route-adapter (:route props))   ; todo migrate fiddles and remove
        props (-> props
                  (update :class css "hyperfiddle")
                  (dissoc :route)
                  (assoc :href (some->> route (hf/url-encode (hf/domain (:runtime ctx)))))
                  (select-keys [:class :href :style]))]
    (into [:a props] children)))

(defn validated-route-tooltip-props [+?route link-ref ctx props] ; link is for validation
  ; this is a fine place to eval, put error message in the tooltip prop
  ; each prop might have special rules about his default, for example :visible is default true,
  ; does this get handled here?
  (-> +?route
      (>>= (fn [?route]
             ; todo should still allow routing even if these checks fail, all that matters if it can be encoded to an url
             ; We specifically hydrate this deep just so we can validate anchors like this.
             (let [?fiddle (:link/fiddle @link-ref)]
               (case (:fiddle/type ?fiddle)
                 :query (->>
                          ; just check if a request can be built
                          (base/request-for-fiddle+ (:runtime ctx) (:partition-id ctx) ?route ?fiddle)
                          (cats/fmap (constantly ?route)))
                 :entity (let [[$1 :as params] (::route/datomic-args ?route)]
                           ; todo just run base/request-for-fiddle+
                           (if (not= nil $1)                ; todo check conn
                             (either/right ?route)
                             (either/left {:message "malformed entity param" :data {:params params}})))
                 (either/right ?route)))))
      (either/branch
        (fn [e]
          (-> props
              (assoc :route nil
                     :tooltip [:warning (pprint-str #{e})])
              (update :class css "invalid")))
        (fn [route]
          (cond-> (assoc props :route route)
            (:hyperfiddle.ui/debug-tooltips ctx)
            (assoc :tooltip [nil (when route                ; Show how it routed. The rest is obvious from data mode
                                   (->> (concat #_[fiddle-ident] (::route/datomic-args route))
                                        (map pr-str) (interpose " ") (apply str)))]))))))

(letfn [(prompt [link ctx ?label]
          (or ?label
              (->> (set (:link/class link))
                   (remove #(= "hf" (namespace %)))         ; "iframe" is not a good name
                   (map name)
                   (interpose " ")
                   (apply str)
                   blank->nil)
              (some-> link :link/fiddle :fiddle/ident name)
              ; todo link/tx-fn should be a kw already, @(r/cursor link-ref [:link/tx-fn])
              (some-> (context/link-tx (assoc ctx :hypercrud.browser/link (r/pure link))) name)))]
  (defn ui-from-link [link-ref ctx & [props ?label]]
    {:pre [link-ref (r/reactive? link-ref)]}
    ; Once this link changes, pretty much everything below here needs to recompute
    ; Reactions are unlikely to be faster than render-tree-pruning.
    (let [has-tx-fn (boolean (context/link-tx (assoc (select-keys ctx [:runtime :hypercrud.browser/fiddle])
                                                :hypercrud.browser/link link-ref))) ; todo link/tx-fn should be a kw already, @(r/cursor link-ref [:link/tx-fn])
          is-iframe (some #{:hf/iframe} @(r/cursor link-ref [:link/class]))]
      (cond
        (and has-tx-fn (nil? @(r/cursor link-ref [:link/fiddle])))
        [effect-cmp ctx link-ref props @(r/fmap-> link-ref (prompt (select-keys ctx [:runtime :hypercrud.browser/fiddle]) ?label))]

        (or has-tx-fn (and is-iframe (:iframe-as-popover props)))
        (let [props (dissoc props :iframe-as-popover)]
          [popover-cmp ctx link-ref props @(r/fmap-> link-ref (prompt (select-keys ctx [:runtime :hypercrud.browser/fiddle]) ?label))])

        is-iframe
        (let [+route-and-ctx (context/refocus-build-route-and-occlude+ ctx link-ref) ; Can fail if formula dependency isn't satisfied
              +route (fmap second +route-and-ctx)
              link-ctx (either/branch
                         +route-and-ctx
                         (constantly nil)                   ; how can this safely be nil
                         first)]
          [stale/loading (r/track runtime/loading? (:runtime ctx) (:partition-id ctx)) ; was just ctx before
           +route
           (fn [e] [(ui-error/error-comp ctx) e])
           (fn [route]
             (let [props (update props :class css (->> (context/link-class link-ctx) ; flagged - :class
                                                       (string/join " ")
                                                       css-slugify))
                   partition-id (context/build-pid-from-link ctx link-ctx route)]
               [iframe/iframe-cmp (context/set-partition link-ctx partition-id) props]))])

        :else (let [+route-and-ctx (context/refocus-build-route-and-occlude+ ctx link-ref) ; Can fail if formula dependency isn't satisfied
                    +route (fmap second +route-and-ctx)
                    link-ctx (either/branch
                               +route-and-ctx
                               (constantly nil)             ; how can this safely be nil
                               first)
                    props (validated-route-tooltip-props +route link-ref link-ctx props)]
                [tooltip (tooltip-props (:tooltip props))
                 (let [props (dissoc props :tooltip)]
                   ; what about class - flagged
                   ; No eav, the route is the same info
                   [anchor link-ctx props @(r/fmap-> link-ref (prompt (select-keys ctx [:runtime :hypercrud.browser/fiddle]) ?label))])])))))

(defn ^:export link "Render a link. :hf/iframes have managed loading component."
  [corcs ctx & [?label props]]
  (either/branch
    (data/select+ ctx corcs)
    #(vector :span %)
    (fn [link-ref]
      [ui-from-link link-ref ctx props ?label])))

(defn ^:export browse "Relation level browse. Works in forms and lists but not tables." ; Unused?
  [corcs ctx & [?user-renderer props]]
  {:pre [ctx]}
  (let [props (if ?user-renderer
                (assoc props :user-renderer ?user-renderer)
                props)]
    (either/branch
      (data/select+ ctx corcs)
      #(vector :span %)
      (fn [link-ref]
        [ui-from-link link-ref ctx props]))))

(defn form-field "Form fields are label AND value. Table fields are label OR value.
  EAV is already set."
  [ctx Body Head props]
  (let [ctx (maybe-redirect-to-route ctx)]
    [:div {:class (css "field" (:class props))
           :style {:border-color (if (redirect-to-route? ctx)
                                   "lightgrey" ; fields writting to the route are grey
                                   (domain/database-color (hf/domain (:runtime ctx)) (context/dbname ctx)))}}
     [Head ctx props]                                   ; suppress ?v in head even if defined
     [Body ctx (value-props props ctx)]]))

(defn table-field "Form fields are label AND value. Table fields are label OR value."
  [ctx Body Head props]                                     ; Body :: (val props ctx) => DOM, invoked as component
  (case (if (:hypercrud.browser/head-sentinel ctx) :head :body) ; broken
    :head [:th {:class (css "field" (:class props))         ; hoist
                :style {:background-color (domain/database-color (hf/domain (:runtime ctx)) (context/dbname ctx))}}
           [Head ctx (-> props
                         (update :class css (when (sort/sortable? ctx) "sortable")
                                 (if-let [[p ?d] (sort/sort-directive ctx)]
                                   (if ?d
                                     (name ?d))))
                         (assoc :on-click (r/partial sort/toggle-sort! ctx)))]]
    ; Field omits [] but table does not, because we use it to specifically draw repeating anchors with a field renderer.
    :body [:td {:class (css "field" (:class props))
                :style {:border-color (domain/database-color (hf/domain (:runtime ctx)) (context/dbname ctx))}}
           [Body ctx (value-props props ctx)]]))

(defn ^:export field "Works in a form or table context. Draws label and/or value."
  [relative-path ctx & [?f props]]
  {:pre [ctx]}
  (let [ctx (context/focus ctx relative-path)               ; Handle element and attr
        Body (or ?f hyper-control)
        Head (or (:label-fn props) hyper-label)
        props (dissoc props :label-fn)
        props (update props :class css (semantic-css ctx))]


    ; Make this go away. Since field isn't an abstraction, these don't need to share code anymore?
    ; What about multimethod field overload?
    (case (:hyperfiddle.ui/layout ctx)                      ; check cardinality
      :hyperfiddle.ui.layout/table
      ^{:key (str relative-path)}
      [table-field ctx Body Head props]

      ^{:key (str relative-path)}                           ; could be a product, does eav as key work for that?
      [form-field ctx Body Head props])))

(defn row
  [ctx props & cs]
  (let [props (select-keys props [:keys])]
    (into [:tr props] cs)))

(defn rows
  [ctx {:keys [columns row]
        :or {columns columns, row row}
        :as props}
   rows]
  (for [[ix [?k ctx]] rows]
    (into [row ctx (merge props {:key (or ?k ix)})] (columns ctx))))

(defn ^:export table "Semantic table; columns driven externally" ; this is just a widget
  [ctx & [{:keys [rows row columns headers colgroup]
           :or {rows rows, row row, columns columns, headers columns, colgroup (fn [& _] nil)}
           :as props}]]
  {:pre [(s/assert :hypercrud/context ctx)]}
  ; Need backwards compat arity
  (let [sort-col (r/atom (::sort/initial-sort props))
        page-size (if (> 0 hf/browser-query-limit) 20 hf/browser-query-limit)
        page (r/atom 0)]
    (fn [ctx & [props]]
      {:pre [(s/assert :hypercrud/context ctx)]}
      (let [props (update props :class (fnil css "hyperfiddle") "unp") ; fnil case is iframe root (not a field :many)
            ctx (assoc ctx ::sort/sort-col sort-col
                           :hypercrud.browser/head-sentinel true ; hacks - this is coordination with the context, how to fix?
                           ::layout :hyperfiddle.ui.layout/table)]
        [:<>
         [:table (select-keys props [:class :style])
          [:thead (into [:tr] (headers ctx))]
                                        ; filter? Group-by? You can't. This is data driven. Shape your data in the peer.
          [colgroup]
          (let [rows (rows ctx props
                           (->> (hypercrud.browser.context/spread-rows
                                  ctx
                                  #(sort/sort-fn % sort-col)
                                  #(->> % (drop (* @page page-size)) (take page-size)))
                           ; Duplicate rows https://github.com/hyperfiddle/hyperfiddle/issues/298
                                (map-indexed vector)))]
            (if (empty? rows)
              [:tbody
               [:tr
                [:td {:colSpan (count cols)
                      :class   "info-box_notice"}
                 "No match found"]]]
              (into [:tbody] rows)))]
         (let [n (count @(:hypercrud.browser/result ctx))]
           (cond
             (> n page-size)
             [contrib.hfrecom/anchor-tabs
              :model page
              :tabs (for [i (range (/ n page-size))]
                      {:id i :href "#" :label (str (inc i))})
              :on-change (r/partial reset! page)]
             (< n page-size) nil
             (= n 0) [:div "no results"]))]))))

(defn- path-fn [v form]
  (cond
    (seqable? form) (->> (map-indexed (fn [idx form] [idx form]) (vec form))
                         (some (fn [[idx form]]
                                 (when-let [path (path-fn v form)]
                                   (comp path #(get % idx) vec)))))
    (= v form) identity))

(defn route-input
  "Example:
      [route-input ctx {:as :foo :valmap name :on-change (fn [ov nv] (keyword nv))}]"
  [{:keys [:hypercrud.browser/route] :as ctx} & [{:keys [as valmap on-change]
                                                  :or {valmap identity
                                                       on-change (fn [_ nv] nv)}
                                                  :as props}]]
  {:pre [as]}
  (let [val (some-> (get @route as) valmap)
        handler (fn [val]
                  (if val                                   ; remove nil keys from route
                    (hf/swap-route! ctx assoc as val)
                    (hf/swap-route! ctx dissoc as)))
        props (assoc props :value val
                           :on-change (comp handler on-change))]
    [contrib.ui/debounced props contrib.ui/text]))

(defn needle-input [unfilled-where ctx & [input-props]]
  [route-input ctx
   (assoc input-props
     :as ::route/where
     :on-change (fn [_ needle]
                  (when-not (empty? needle)
                    (route/fill-where unfilled-where (if-let [spec (:hf/where-spec input-props)]
                                                       (spec-coerce.alpha/coerce spec needle)
                                                       needle))))

     :valmap (fn [where]
               (when-let [f (path-fn '% unfilled-where)]
                 (let [filled-where where]
                   (when-let [needle (f filled-where)]
                     (when (= (route/fill-where unfilled-where needle) filled-where)
                       needle))))))])

(defn needle-input2 [ctx props]
  {:pre [(s/assert :hf/where props)]}
  (let [unfilled-where (:hf/where props)
        props (merge {:placeholder (:html/placeholder props (pr-str unfilled-where))
                      :class "form-control"}
                     (select-keys props [:hf/where :hf/where-spec]))]
    [needle-input unfilled-where ctx props]))

(defn hint [val {:keys [hypercrud.browser/fiddle] :as ctx} props]
  (case @(r/fmap :fiddle/type fiddle)
    :entity (when (empty? val)
              [:div.hyperfiddle.alert.alert-warning "Warning: invalid route (d/pull requires an entity argument). To add a tempid entity to the URL, click here: "
               [:a {:href "~entity('$','tempid')"} [:code "~entity('$','tempid')"]] "."])
    :query (when (= hf/browser-query-limit (count val))
             [:div.hyperfiddle.alert.alert-warning
              (str/format "Warning: Query resultset has been truncated to %s records."
                          hf/browser-query-limit)])
    :eval nil
    ))

(defn form "Not an abstraction." [columns val ctx & [props]]
  {:pre [(s/assert :hypercrud/context ctx)]}
  (let [ctx (assoc ctx ::layout :hyperfiddle.ui.layout/block)] ; make this go away
    (into
      [:<> {:key (str (context/row-key ctx val context/entity-viewkey))}]
      (columns ctx))))

(defn columns [ctx & [props]]
  {:pre [(s/assert :hypercrud/context ctx)]}
  #_(cons (field relpath ctx nil props))
  (doall
    (for [[k _] (hypercrud.browser.context/spread-attributes ctx)]
      [field [k] ctx nil props])))

(defn pull "handles any datomic result that isn't a relation, recursively"
  [val ctx & [props]]
  {:pre [(s/assert :hypercrud/context ctx)]}
  (let [[_ a _] (context/eav ctx)]
    ; detect top and do fiddle-level here, instead of in columns?
    ; have result here to infer cardinality when schema absent
    (match* [(contrib.datomic/cardinality @(:hypercrud.browser/schema ctx) a)] ; this is parent - not focused?
      [:db.cardinality/one] [:<>
                             (controls/hf-new val ctx)
                             (controls/hf-remove val ctx)
                             [form columns val ctx props]]

      ; http://hyperfiddle.hyperfiddle.site/:hyperfiddle.ide!domain/~entity('$domains',(:domain!ident,'hyperfiddle'))
      [:db.cardinality/many] [:<>
                              #_(controls/hf-new val ctx)   ; table db/id will handle this
                              [table ctx (assoc props :columns columns)]]
      [_] [:pre (pr-str a)])))

(defn table-column-product "We collapse the top layer of pull into a cartesian product.
  What we really want is grouped table headers to clarify what is going on here."
  [ctx props]
  ; http://hyperfiddle.hyperfiddle.site/:database!options-list/
  #_(cons (field [] ctx props))
  (->> (for [[i {el :hypercrud.browser/element :as ctx-e}] (hypercrud.browser.context/spread-elements ctx)]
         #_(cons (field [i] ctx props))
         (case (unqualify (contrib.datomic/parser-type @el))
           :variable [[field [i] ctx props]]
           :aggregate [[field [i] ctx props]]
           :pull (for [[a ctx-a] (hypercrud.browser.context/spread-attributes ctx-e)]
                   [field [i a] ctx props])))
       (mapcat identity)                                    ; Don't flatten the hiccup
       doall))

(defn search-defaults [ctx & [props]]
  (let [ctx (context/derive-for-search-defaults ctx)]
    [:div {:class "hyperfiddle search-defaults"
           :style {:margin-bottom "1rem"}}
     [result (hf/data ctx) ctx props]]))

(defn ^:export result "Default result renderer. Invoked as fn, returns seq-hiccup, hiccup or
nil. call site must wrap with a Reagent component"          ; is this just hyper-control ?
  [val ctx & [props]]
  {:pre [(not= val clojure.core/val)]}                      ; check for busted call while we migrate away from this param
  [:<>
   (when (some? (:hypercrud.browser/route-defaults ctx))
     [search-defaults ctx props])
   (doall
     (for [[k ctx] (hypercrud.browser.context/spread-result ctx)]
       [:<> {:key k}
        (hint val ctx props)
        (when-let [unfilled-where (:hf/where props)]
          [needle-input2 ctx props])
        (let [qtype (type @(:hypercrud.browser/qfind ctx))] ; i think we need to make up a qfind for this case
          (if-not qtype
            [table ctx (update props :columns (orf table-column-product))]
            (condp some [qtype]                             ; spread-rows
              #{FindRel FindColl} [table ctx (update props :columns (orf table-column-product))] ; identical result?
              #{FindTuple FindScalar} [form table-column-product val ctx props])))]))])

;(defmethod render :hf/blank [ctx props]
;  [iframe-field-default val ctx props])
;
;(defmethod render :hf/find-rel [ctx props]
;  [table table-column-product ctx props])
;
;(defmethod render :hf/find-coll [ctx props]
;  [table table-column-product ctx props])
;
;(defmethod render :hf/find-tuple [ctx props]
;  [form table-column-product val ctx props])
;
;(defmethod render :hf/find-scalar [ctx props]
;  [:<>
;   (hint val ctx props)
;   [form table-column-product val ctx props]])

; to be manually kept in sync with hf.fiddle/default-renderer-str
(defn ^:export fiddle [val ctx props]
  (let [{:keys [:hypercrud.browser/fiddle]} ctx]
    [:div.container-fluid props
     [:h1 (str (:fiddle/ident @fiddle))]
     [hyperfiddle.ui/result val ctx {}]]))

(defn ^:export fiddle-xray [val ctx & [props]]
  [:<>
   [:div (select-keys props [:class :on-click])
    [result val ctx {}]]
   [entity-links-iframe ctx props]])

(letfn [(render-edn [data]
          [contrib.ui/async-code {:value data :read-only true}])]
  (defn ^:export fiddle-api [_ {rt             :runtime
                                route-defaults :hypercrud.browser/route-defaults
                                :as            ctx} & [props]]
    [:div.hyperfiddle.display-mode-api (select-keys props [:class])
     (render-edn (some-> (:hypercrud.browser/result ctx) deref))
     (when-let [iframes (->> (disj (set (runtime/descendant-pids rt (:partition-id ctx))) (:partition-id ctx))
                             (map (fn [pid]
                                    ^{:key (str pid)}
                                    [:<>
                                     [:dt
                                      [iframe/route-editor (iframe/get-route rt pid false) (iframe/get-route rt pid true) (r/partial iframe/set-route rt pid)]]
                                     [:dd
                                      (let [ctx (context/set-partition (context/map->Context {:runtime rt}) pid)]
                                        [iframe/stale-browse ctx
                                         (fn [ctx e] [ui-error/error-block e])
                                         (fn [ctx] (render-edn (some-> (:hypercrud.browser/result ctx) deref)))])]]))
                             (doall)
                             seq)]
       [:div.container-fluid.iframes
        [:h4 "iframes"]
        (into [:dl] iframes)])]))

(defn ^:export img [val ctx & [props]]
  [:img (merge props {:src val})])
