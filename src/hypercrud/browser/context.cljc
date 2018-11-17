(ns hypercrud.browser.context
  (:require
    [cats.monad.either :as either]
    [contrib.data :refer [ancestry-common ancestry-divergence]]
    [contrib.reactive :as r]
    [hypercrud.browser.field :as field]
    [hyperfiddle.domain :as domain]
    [hyperfiddle.runtime :as runtime]))


(defn clean [ctx]
  (dissoc ctx
          :hypercrud.ui/error
          :hyperfiddle.ui/layout

          :hypercrud.browser/attr-renderers
          :hypercrud.browser/data
          :hypercrud.browser/fiddle
          :hypercrud.browser/field
          :hypercrud.browser/parent
          :hypercrud.browser/path
          :hypercrud.browser/route))

(defn source-mode [ctx]
  (-> ctx
      (assoc :hyperfiddle.ui/iframe-on-click (r/constantly nil) ; disable alt-click
             :hypercrud.ui/display-mode (r/track identity :hypercrud.browser.browser-ui/user))
      (update :hypercrud.browser/domain
              (fn [domain]
                (update (:hypercrud.browser/source-domain ctx) :domain/databases
                        (fn [dbs]
                          (->> dbs
                               (remove #(= "$" (:domain.database/name %)))
                               (cons {:domain.database/name "$"
                                      :domain.database/record (:domain/fiddle-database domain)})
                               vec)))))))

(defn attribute-segment? [path-segment]
  (or (keyword? path-segment)
      (= '* path-segment)))

(defn find-element-segment? [path-segment]
  (integer? path-segment))

(defn segment-type [segment]
  (cond
    (attribute-segment? segment) :attribute
    (find-element-segment? segment) :element
    :else :naked))

(defn segment-type-2 [segment]
  (cond
    (= '* segment) :splat
    (keyword? segment) :attribute
    (integer? segment) :element                             ; can be a variable element, an aggregate element, etc
    ; it can also be entity-[], which has implied :element, this also happens in the query [?e ...] case
    :else :naked-or-element))

(defn target-route [ctx] @(runtime/state (:peer ctx) [::runtime/partitions nil :route]))

(defn dbname [ctx] (some->> (:hypercrud.browser/field ctx) (r/fmap ::field/source-symbol) deref str))

(defn uri
  ([ctx] (uri (dbname ctx) ctx))
  ([dbname ctx] (some-> dbname (domain/dbname->uri (:hypercrud.browser/domain ctx)))))

(defn ctx->id-lookup
  ([ctx] (ctx->id-lookup (uri ctx) ctx))
  ([uri ctx]
    ; todo what about if the tempid is on a higher branch in the uri?
   (some-> @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :tempid-lookups uri])
           (either/branch #(throw (ex-info % {})) identity))))

(defn hydrate-attribute [ctx ident & ?more-path]
  (runtime/state (:peer ctx) (concat [::runtime/partitions (:branch ctx) :schemas (uri ctx)] (cons ident ?more-path))))

(defn- set-parent [ctx]
  (assoc ctx :hypercrud.browser/parent (dissoc ctx :hypercrud.browser/data)))

(defn- set-parent-data [ctx]
  (update ctx :hypercrud.browser/parent (fnil into {}) (select-keys ctx [:hypercrud.browser/data])))

(defn find-child-field [field path-segment ctx]
  (or (->> (::field/children @field)
           (filter #(= (::field/path-segment %) path-segment))
           first)
      (when (keyword? path-segment)
        (let [uri (uri (str (::field/source-symbol @field)) ctx)
              schema @(runtime/state (:peer ctx) [::runtime/partitions (:branch ctx) :schemas uri])]
          (field/summon schema (::field/source-symbol @field) path-segment)))))

(defn find-parent-field [ctx]
  (get-in ctx [:hypercrud.browser/parent :hypercrud.browser/field]))

(letfn [(focus-segment [ctx path-segment]                   ; attribute or fe segment
          #_(assert (or (not (:hypercrud.browser/data ctx)) ; head has no data, can focus without calling row
                        @(r/fmap->> (:hypercrud.browser/field ctx) ::field/cardinality (not= :db.cardinality/many)))
                    (str "Cannot focus directly from a cardinality/many (do you need a table wrap?). current path: " (:hypercrud.browser/path ctx) ", attempted segment: " path-segment))
          (let [field (r/track find-child-field (:hypercrud.browser/field ctx) path-segment ctx)
                ctx (-> ctx
                        (set-parent)
                        (update :hypercrud.browser/path conj path-segment)
                        (assoc :hypercrud.browser/field field))]
            (if-not (:hypercrud.browser/data ctx)
              ctx                                           ; head
              (-> (set-parent-data ctx)                     ; body
                  (update :hypercrud.browser/validation-hints #(for [[[p & ps] hint] % :when (= p path-segment)]
                                                                 [ps hint]))
                  (assoc :hypercrud.browser/data
                         (let [f (r/fmap ::field/get-value field)]
                           #_(assert @f (str "focusing on a non-pulled attribute: " (pr-str (:hypercrud.browser/path ctx)) "."))
                           (r/fapply f (:hypercrud.browser/data ctx))))))))]
  (defn focus "Throws if you focus a higher dimension" [ctx relative-path]
    (reduce focus-segment ctx relative-path)))

(defn row "Toggle :many into :one as we spread through the rows. k is used for filtering validation hints"
  [ctx rval & [k]]
  {:pre [(r/reactive? rval)]}
  (assert @(r/fmap->> (:hypercrud.browser/field ctx) ::field/cardinality (= :db.cardinality/many))
          (str "`context/row` is only valid on cardinality/many. current path: " (pr-str (:hypercrud.browser/path ctx))))
  (-> ctx
      (set-parent)
      (set-parent-data)
      (assoc :hypercrud.browser/data rval)
      (update :hypercrud.browser/validation-hints #(for [[[p & ps] hint] % :when (= k p)]
                                                     [ps hint]))
      (update :hypercrud.browser/field
              #(r/fmap-> % (assoc ::field/cardinality :db.cardinality/one)))))

(defn refocus "focus common ancestor" [ctx path]
  {:pre [ctx]
   :post [%]}
  ; This should run the link formula as part of focus step
  (let [current-path (:hypercrud.browser/path ctx)
        common-ancestor-path (ancestry-common current-path path)
        unwind-offset (- (count current-path) (count common-ancestor-path))
        common-ancestor-ctx ((apply comp (repeat unwind-offset :hypercrud.browser/parent)) ctx)]
    (focus common-ancestor-ctx (ancestry-divergence path current-path))))

(defn id [ctx]
  ; When looking at an attr of type ref, figure out it's identity, based on all the ways it can be pulled.
  ; What if we pulled children without identity? Then we can't answer the question (should assert this)
  (if-let [data (:hypercrud.browser/data ctx)]              ; Guard is for txfn popover call site
    (or @(contrib.reactive/cursor data [:db/ident])
        @(contrib.reactive/cursor data [:db/id])
        @data)))

(defn has-entity-identity? [ctx]
  (::field/data-has-id? @(:hypercrud.browser/field ctx))
  #_(and (context/dbname ctx) (last (:hypercrud.browser/path ctx))))

(defn invalid? [ctx]
  (->> (:hypercrud.browser/validation-hints ctx)
       (filter (comp nil? first))
       (seq)))