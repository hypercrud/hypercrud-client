(ns hyperfiddle.ui
  (:require
    [cats.core :as cats :refer [>>= fmap mlet return]]
    [cats.monad.either :as either]
    [cljs.spec.alpha :as s]
    [clojure.core.match :refer [match match*]]
    [clojure.string :as string]
    [contrib.css :refer [css css-slugify]]
    [contrib.data :refer [unqualify]]
    [contrib.eval :as eval]
    [contrib.pprint :refer [pprint-str]]
    [contrib.reactive :as r]
    [contrib.hfrecom]
    [contrib.string :refer [blank->nil]]
    [contrib.ui]
    [contrib.ui.safe-render :refer [user-portal]]
    [contrib.ui.tooltip :refer [tooltip tooltip-props]]
    [contrib.validation]
    [cuerdas.core :as str]
    [datascript.parser :refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hyperfiddle.api :as hf]
    [hyperfiddle.data :as data]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.controls :as controls :refer [label-with-docs identity-label ref-label element-label magic-new]]
    [hyperfiddle.ui.docstring :refer [semantic-docstring]]
    [hyperfiddle.ui.error :as ui-error]
    [hyperfiddle.ui.iframe :as iframe]
    [hyperfiddle.ui.popover :refer [effect-cmp popover-cmp]]
    [hyperfiddle.ui.select$]
    [hyperfiddle.ui.sort :as sort]
    [hyperfiddle.ui.stale :as stale]
    [hyperfiddle.ui.util :refer [writable-entity?]]
    [taoensso.timbre :as timbre]))


(let [memoized-eval-expr-str!+ (memoize eval/eval-expr-str!+) ; don't actually need to safely eval, just want to memoize exceptions)
      ; defer eval until render cycle inside userportal
      eval-renderer-comp (fn [fiddle-renderer-str & args]
                           (either/branch
                             (memoized-eval-expr-str!+ fiddle-renderer-str)
                             (fn [e] (throw e))
                             (fn [f] (into [f] args))))]
  (defn attr-renderer-control [val ctx & [props]]
    ; The only way to stabilize this is for this type signature to become a react class.
    (when-let [user-f (->> (second (context/eav ctx))
                           (runtime/attribute-renderer (:peer ctx) (:branch ctx))
                           blank->nil)]
      [user-portal (ui-error/error-comp ctx) nil
       ; ?user-f is stable due to memoizing eval (and only due to this)
       [eval-renderer-comp user-f val ctx props]])))

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
      (:options props) [hyperfiddle.ui.select$/select nil ctx props] ; legacy, use attr renderers for this

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

(defn ^:export hyper-control "Val is for userland field renderers, our builtin controls use ctx and ignore val."
  [val ctx & [props]]
  {:post [%]}
  (or (attr-renderer-control val ctx props)                 ; compat
      (hf/render ctx props)))

(defn ^:export hyper-label [_ ctx & [props]]                ; props has sort :on-click
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
              (contrib.datomic/cardinality @(:hypercrud.browser/schema ctx) a)
              (if (contrib.datomic/isComponent @(:hypercrud.browser/schema ctx) a) :component)])
           (:hypercrud.browser/pull-path ctx))              ; pullpath is legacy, remove from css todo
         (map css-slugify)
         (apply css))))

(defn- value-props [props ctx]
  (as-> props props
    (update props :disabled #(or % (not @(r/track writable-entity? ctx))))
    (cond-> props (context/leaf-invalid? ctx) (assoc :is-invalid true))
    (update props :class css (if (:disabled props) "disabled"))))

(defn ^:export value "Relation level value renderer. Works in forms and lists but not tables (which need head/body structure).
User renderers should not be exposed to the reaction."
  ; Path should be optional, for disambiguation only. Naked is an error
  [relative-path ctx ?f & [props]]                          ; ?f :: (ref, props, ctx) => DOM
  (let [ctx (context/focus ctx relative-path)
        props (-> (update props :class css (semantic-css ctx))
                  (value-props ctx))]
    [(or ?f hyper-control) (hypercrud.browser.context/data ctx) ctx props]))

(defn ^:export anchor [ctx props & children]
  (let [route (route/legacy-route-adapter (:route props))   ; todo migrate fiddles and remove
        props (-> props
                  (update :class css "hyperfiddle")
                  (dissoc :route)
                  (assoc :href (some->> route (domain/url-encode (runtime/domain (:peer ctx)))))
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
                          (base/request-for-fiddle+ (:peer ctx) (:branch ctx) ?route ?fiddle)
                          (cats/fmap (constantly ?route)))
                 :entity (let [[$1 :as params] (::route/datomic-args ?route)]
                           ; todo just run base/request-for-fiddle+
                           (if (not= nil $1)                ; todo check conn
                             (either/right ?route)
                             (either/left {:message "malformed entity param" :data {:params params}})))
                 ; nil means :blank
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

(letfn [(prompt [link ?label]
          (or ?label
              (->> (set (:link/class link))
                   (remove #(= "hf" (namespace %)))         ; "iframe" is not a good name
                   (map name)
                   (interpose " ")
                   (apply str)
                   blank->nil)
              (some-> link :link/fiddle :fiddle/ident name)
              ; todo link/tx-fn should be a kw already, @(r/cursor link-ref [:link/tx-fn])
              (some-> (context/link-tx {:hypercrud.browser/link (r/pure link)}) name)))]
  (defn ui-from-link [link-ref ctx & [props ?label]]
    {:pre [link-ref (r/reactive? link-ref)]}
    ; Once this link changes, pretty much everything below here needs to recompute
    ; Reactions are unlikely to be faster than render-tree-pruning.
    (let [has-tx-fn (boolean (context/link-tx {:hypercrud.browser/link link-ref})) ; todo link/tx-fn should be a kw already, @(r/cursor link-ref [:link/tx-fn])
          is-iframe (some #{:hf/iframe} @(r/cursor link-ref [:link/class]))]
      (cond
        (and has-tx-fn (nil? @(r/cursor link-ref [:link/fiddle])))
        [effect-cmp ctx link-ref props @(r/fmap-> link-ref (prompt ?label))]

        (or has-tx-fn (and is-iframe (:iframe-as-popover props)))
        (let [props (dissoc props :iframe-as-popover)]
          [popover-cmp ctx link-ref props @(r/fmap-> link-ref (prompt ?label))])

        is-iframe
        (let [+route-and-ctx (context/refocus-build-route-and-occlude+ ctx link-ref) ; Can fail if formula dependency isn't satisfied
              +route (fmap second +route-and-ctx)
              link-ctx (either/branch
                         +route-and-ctx
                         (constantly nil)                   ; how can this safely be nil
                         first)]
          [stale/loading (r/track runtime/branch-is-loading? (:peer ctx) (:branch ctx)) ; was just ctx before
           +route
           (fn [e] [(ui-error/error-comp ctx) e])
           (fn [route]
             (let [props (update props :class css (->> (context/link-class link-ctx) ; flagged - :class
                                                       (string/join " ")
                                                       css-slugify))]
               [iframe/iframe-cmp route link-ctx props]))])

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
                   [anchor link-ctx props @(r/fmap-> link-ref (prompt ?label))])])))))

(defn ^:export link "Render a link. :hf/iframes have managed loading component."
  [corcs ctx & [?label props]]
  (either/branch
    (data/select+ ctx corcs)
    #(vector :span %)
    (fn [link-ref]
      [ui-from-link link-ref ctx props ?label])))

(defn ^:export browse "Relation level browse. Works in forms and lists but not tables."
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
  (let []
    [:div {:class (css "field" (:class props))
           :style {:border-color (domain/database-color (runtime/domain (:peer ctx)) (context/dbname ctx))}}
     [Head nil ctx props]                                   ; suppress ?v in head even if defined
     [Body (hypercrud.browser.context/data ctx) ctx (value-props props ctx)]]))

(defn table-field "Form fields are label AND value. Table fields are label OR value."
  [ctx Body Head props]                                     ; Body :: (val props ctx) => DOM, invoked as component
  (case (if (:hypercrud.browser/head-sentinel ctx) :head :body) ; broken
    :head [:th {:class (css "field" (:class props))         ; hoist
                :style {:background-color (domain/database-color (runtime/domain (:peer ctx)) (context/dbname ctx))}}
           [Head nil ctx (-> props
                             (update :class css (when (sort/sortable? ctx) "sortable")
                                     (if-let [[p ?d] (sort/sort-directive ctx)]
                                       (if ?d
                                         (name ?d))))
                             (assoc :on-click (r/partial sort/toggle-sort! ctx)))]]
    ; Field omits [] but table does not, because we use it to specifically draw repeating anchors with a field renderer.
    :body [:td {:class (css "field" (:class props))
                :style {:border-color (domain/database-color (runtime/domain (:peer ctx)) (context/dbname ctx))}}
           [Body (hypercrud.browser.context/data ctx) ctx (value-props props ctx)]]))

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

(defn ^:export table "Semantic table; columns driven externally" ; this is just a widget
  [columns ctx & [props]]
  {:pre [(s/assert :hypercrud/context ctx)]}
  ; Need backwards compat arity
  (let [sort-col (r/atom (::sort/initial-sort props))
        page-size (if (> 0 base/browser-query-limit) 20 base/browser-query-limit)
        page (r/atom 0)]
    (fn [columns ctx & [props]]
      {:pre [(s/assert :hypercrud/context ctx)]}
      (let [props (update props :class (fnil css "hyperfiddle") "unp") ; fnil case is iframe root (not a field :many)
            ctx (assoc ctx ::sort/sort-col sort-col
                           :hypercrud.browser/head-sentinel true ; hacks - this is coordination with the context, how to fix?
                           ::layout :hyperfiddle.ui.layout/table)]
        [:<>
         [:table (select-keys props [:class :style])
          [:thead (into [:tr] (columns ctx))]
          ; filter? Group-by? You can't. This is data driven. Shape your data in the peer.
          (into [:tbody] (for [[ix [?k ctx]] (->> (hypercrud.browser.context/spread-rows
                                                    ctx
                                                    #(sort/sort-fn % sort-col)
                                                    #(->> % (drop (* @page page-size)) (take page-size)))
                                                  ; Duplicate rows https://github.com/hyperfiddle/hyperfiddle/issues/298
                                                  (map-indexed vector))]
                           ; columns keys should work out, field sets it on inside
                           #_[:tr {:key (str ?k)} (columns ctx)]
                           (let [k (or ?k ix)
                                 cs (columns ctx)]
                             (into [:tr {:key (str k)}] cs))))]
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

(defn needle-input [unfilled-where {:keys [:hypercrud.browser/route ::-set-route] :as ctx} & [input-props]]
  [contrib.ui/debounced
   (assoc input-props
     :value (when-let [f (path-fn '% unfilled-where)]
              (let [filled-where (::route/where @route)]
                (when-let [needle (f filled-where)]
                  (when (= (route/fill-where unfilled-where needle) filled-where)
                    needle))))
     :on-change (fn [_ needle]
                  (-> (if (empty? needle)
                        (dissoc @route ::route/where)
                        (assoc @route ::route/where (route/fill-where unfilled-where needle)))
                      -set-route)))
   contrib.ui/text])

(defn hint [val {:keys [hypercrud.browser/fiddle] :as ctx} props]
  (case @(r/fmap :fiddle/type fiddle)
    :entity (when (empty? val)
              [:div.alert.alert-warning "Warning: invalid route (d/pull requires an entity argument). To add a tempid entity to the URL, click here: "
               [:a {:href "~entity('$','tempid')"} [:code "~entity('$','tempid')"]] "."])
    :query (when (= base/browser-query-limit (count val))
             [:div.alert.alert-warning (str/format "Warning: Query resultset has been truncated to %s records." base/browser-query-limit)])
    :blank nil
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
    (match* [(contrib.datomic/cardinality @(:hypercrud.browser/schema ctx) a)] ; this is parent - not focused?
      [:db.cardinality/one] [:<>
                             (controls/hf-new val ctx)
                             (controls/hf-remove val ctx)
                             [form columns val ctx props]]

      ; http://hyperfiddle.hyperfiddle.site/:hyperfiddle.ide!domain/~entity('$domains',(:domain!ident,'hyperfiddle'))
      [:db.cardinality/many] [:<>
                              #_(controls/hf-new val ctx)   ; table db/id will handle this
                              [table columns ctx props]]
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

(defn ^:export result "Default result renderer. Invoked as fn, returns seq-hiccup, hiccup or
nil. call site must wrap with a Reagent component"          ; is this just hyper-control ?
  [val ctx & [props]]
  [:<>
   (doall
     (for [[k ctx] (hypercrud.browser.context/spread-result ctx)]
       [:<> {:key k}
        (hint val ctx props)
        (when-let [unfilled-where (:hf/where props)]
          [needle-input unfilled-where ctx {:placeholder "filter" :class "form-control"}])
        (condp some [(type @(:hypercrud.browser/qfind ctx))] ; spread-rows

          #{FindRel FindColl}
          [table table-column-product ctx props]            ; identical result?

          #{FindTuple FindScalar}
          [form table-column-product val ctx props])]))])

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

(def ^:dynamic markdown)                                    ; this should be hf-contrib or something

; to be manually kept in sync with hf.fiddle/default-renderer-str
(defn ^:export fiddle [val ctx props]
  (let [{:keys [:hypercrud.browser/fiddle]} ctx]
    [:div.container-fluid props
     [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]
     [hyperfiddle.ui/result val ctx {}]]))

(defn ^:export fiddle-xray [val ctx & [props]]
  [:<>
   [:div (select-keys props [:class :on-click])
    [result val ctx {}]]
   [entity-links-iframe ctx props]])

(letfn [(render-edn [data]
          (let [edn-str (pprint-str data 160)]
            [contrib.ui/code {:value edn-str :read-only true}]))]
  (defn ^:export fiddle-api [_ ctx & [props]]
    [:div.hyperfiddle.display-mode-api (select-keys props [:class])
     (render-edn (some-> (:hypercrud.browser/result ctx) deref))
     (when-let [iframes (->> (hyperfiddle.data/select-many ctx #{:hf/iframe}) ; this omits deep dependent iframes fixme
                             (map (fn [link]
                                    (either/branch
                                      (mlet [link-ctx (context/refocus-to-link+ ctx (r/pure link))
                                             args (context/build-args+ link-ctx link)
                                             route (context/build-route+ args link-ctx)]
                                        (return [route link-ctx]))
                                      (fn [e] (timbre/warn e))
                                      identity)))
                             (cons (when @(r/fmap :fiddle/hydrate-result-as-fiddle (:hypercrud.browser/fiddle ctx))
                                     (let [[inner-fiddle & inner-args] (::route/datomic-args @(:hypercrud.browser/route ctx))
                                           route (cond-> {:hyperfiddle.route/fiddle inner-fiddle}
                                                   (seq inner-args) (assoc :hyperfiddle.route/datomic-args (vec inner-args)))]
                                       [route ctx])))
                             (remove nil?)
                             (map (fn [[route ctx]]
                                    ^{:key (str (hash route))}
                                    [iframe/managed-route-editor-state
                                     (fn [route route-on-change ctx & [props]]
                                       [:<>
                                        [:dt [iframe/route-editor route route-on-change]]
                                        [:dd [iframe/stale-browse route ctx
                                              (fn [e] [ui-error/error-block e])
                                              (fn [ctx] (render-edn (some-> (:hypercrud.browser/result ctx) deref)))]]])
                                     route ctx {}]))
                             seq)]
       [:div.container-fluid.iframes
        [:h4 "iframes"]
        (into [:dl] iframes)])]))

(defn ^:export img [val ctx & [props]]
  [:img (merge props {:src val})])

(extend-type context/Context
  hf/UI
  (display-mode [ctx]
    (or (some-> (:hyperfiddle.ui/display-mode ctx) deref)
        :hypercrud.browser.browser-ui/user))
  (display-mode? [ctx k]
    (= k (unqualify (hf/display-mode ctx)))))
