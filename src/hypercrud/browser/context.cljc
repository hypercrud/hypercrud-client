(ns hypercrud.browser.context
  (:require
    [cats.core :as cats :refer [mlet return >>= fmap]]
    [cats.monad.either :as either :refer [left right either?]]
    [clojure.core.match :refer [match #?(:cljs match*)]]
    [clojure.spec.alpha :as s]
    [contrib.do :refer [do-result from-result]]
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [ancestry-common ancestry-divergence unqualify keywordize map-values]]
    [contrib.datomic]
    [contrib.eval :as eval]
    [contrib.reactive :as r]
    [contrib.string :refer [blank->nil]]
    [contrib.try$ :refer [try-either]]
    [datascript.parser #?@(:cljs [:refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]])]
    [hypercrud.types.DbName :refer [#?(:cljs DbName)]]
    [hypercrud.types.ThinEntity :refer [->ThinEntity #?(:cljs ThinEntity)]]
    [hyperfiddle.api :as hf]
    [hyperfiddle.fiddle :as fiddle]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.spec :as spec]
    [hyperfiddle.spec.datomic :as spec-datomic]
    [taoensso.timbre :as timbre])
  #?(:clj
     (:import
       (datascript.parser FindRel FindColl FindTuple FindScalar Variable Aggregate Pull)
       (hypercrud.types.DbName DbName)
       (hypercrud.types.ThinEntity ThinEntity))))


; This file is coded in reactive style, which means no closures because they cause unstable references.
; All "closures" must be explicitly closed with a deftype that implements IEquiv, see helpers in contrib.reactive

(s/def :hypercrud/context
  (s/keys :opt [:hypercrud.browser/route
                :hypercrud.browser/link-index
                :hypercrud.browser/result
                :hypercrud.browser/result-enclosure
                :hypercrud.browser/result-path
                :hypercrud.browser/fiddle
                :hypercrud.browser/result-index             ; private, interpreted relative to result-path. never individually
                :hypercrud.browser/qfind
                :hypercrud.browser/eav                      ; V is scalar or nil. A is attr or fiddle-ident
                :hypercrud.browser/validation-hints
                :hypercrud.browser/element                  ; not included in EAV
                :hypercrud.browser/schema
                :hypercrud.browser/pull-enclosure
                :hypercrud.browser/pull-path                ; used only by refocus and links-in-dimension
                :hyperfiddle.ui.iframe/on-click
                :hyperfiddle.ui/display-mode
                :hyperfiddle.ui/layout
                :hyperfiddle.ui.sort/sort-col]))

(s/def :hypercrud.browser/eav r/reactive?)
(s/def :hypercrud.browser/result r/reactive?)
(s/def ::result-path-segment (s/or :element int? :attribute keyword? :relation string?
                                   :lookup-ref vector?
                                   :scalar (s/and some? (complement sequential?))
                                   ; nil is legal when entities are sparse
                                   ; https://github.com/hyperfiddle/hyperfiddle/issues/298
                                   :empty nil?))
(s/def :hypercrud.browser/result-path (s/coll-of ::result-path-segment :kind vector?))
(s/def :hypercrud.browser/element r/reactive?)
(s/def :hypercrud.browser/element-index int?)
(s/def :hypercrud.browser/schema r/reactive?)
(s/def :hypercrud.browser/pull-path (s/coll-of keyword? :kind vector?))

(defn clean [ctx]
  ; Keeps the :runtime which owns the schemas
  (dissoc ctx
    :hyperfiddle.ui/layout
    :hypercrud.browser/head-sentinel
    :hypercrud.browser/fiddle
    :hypercrud.browser/qfind
    :hypercrud.browser/result
    :hypercrud.browser/result-index
    :hypercrud.browser/result-enclosure
    :hypercrud.browser/result-path
    :hypercrud.browser/link-index
    :hypercrud.browser/eav
    :hypercrud.browser/element
    :hypercrud.browser/schema
    :hypercrud.browser/pull-enclosure
    :hypercrud.browser/pull-path
    :hypercrud.browser/parent                               ; Considered removing this for popovers to have access to parent
    :hypercrud.browser/route
    :hypercrud.browser/validation-hints))

(declare -infer-implicit-element)

(defn attribute-segment? [path-segment]
  (or (keyword? path-segment)
      (= '* path-segment)))

(defn segment-type-2 [segment]
  ; BROKEN, always attr now
  (cond
    (= '* segment) :splat
    (keyword? segment) :attribute
    (integer? segment) :element                             ; can be a variable element, an aggregate element, etc
    ; it can also be entity-[], which has implied :element, this also happens in the query [?e ...] case
    :else :naked-or-element))

(defn dbname [ctx]
  (some->
    ; Guard the implicit-element logic because this is called in tempid paths with an unbuilt context
    ; that i don't understand, this guard preserves old behavior
    ;(if (:hypercrud.browser/qfind ctx)
    ;  (-infer-implicit-element ctx)
    ;  ctx)
    ctx
    :hypercrud.browser/element deref
    :source :symbol str))

(defn underlying-tempid "ctx just needs :runtime and :partition-id" [ctx id]
  ; This muddled thinking is caused by https://github.com/hyperfiddle/hyperfiddle/issues/584
  (cond
    (contrib.datomic/tempid? id) id
    :else (when-let [dbname (dbname ctx)]
            (let [id' (runtime/id->tempid! (:runtime ctx) (:partition-id ctx) dbname id)]
              (when (contrib.datomic/tempid? id') id')))))

(defn lookup-ref [schema e-map]
  (if-let [a (contrib.datomic/find-identity-attr schema e-map)]
    [a (a e-map)]))

(defn smart-entity-identifier "key-entity except without the pr-str fallback.
  Generates the best Datomic lookup ref for a given pull. ctx needs :runtime and :partition-id"
  ; flip params for fmap->
  [ctx {:keys [:db/id :db/ident] :as e-map}]                ; v can be a ThinEntity or a pull i guess
  (s/assert map? e-map)                                     ; used to be very loose, track down these cases now.
  ; If we have a color, and a (last path), ensure it is a ref.
  ; If we have a color and [] path, it is definitely a ref.
  ; If we have no color, it is a scalar or aggregate.

  ; If we're a new tempid this branch, the lookups are no good yet, must use the db/id
  ; (NOT the tempid, the db/id will be reversed to the tempid by actions/with)
  (or (if (underlying-tempid ctx id) id)
      ident
      ; Guard tangled tempid logic
      (some-> (:hypercrud.browser/schema ctx) deref (lookup-ref e-map)) ; Entity must already be inferred for this to work
      id
      ;(if-not (coll? e-map) e-map)                        ; id-scalar
      nil))                                                   ; This is an entity but you didn't pull identity - error?
      ; Or it could be a relation. Why did this get called?


(let [f (fn [?schema+ path] (some->> ?schema+ (cats/fmap #(get-in % path))))]
  (defn hydrate-attribute! [ctx ident & ?more-path]
    (some-> @(r/fmap-> (r/track runtime/get-schema+ (:runtime ctx) (:partition-id ctx) (dbname ctx))
                       (f (cons ident ?more-path)))
            deref)))

(defn- set-parent [ctx]
  (assoc ctx :hypercrud.browser/parent ctx))

(defn entity-datakey [ctx ?v]
  (if ?v
    (smart-entity-identifier ctx ?v)))

(defn entity-viewkey "React.js view key that is stable for entire view lifecycle.
This works around Datomic issue where a tempid is hydrated for the first time and is allocated
a speculative db/id."
  [ctx {:keys [:db/id :db/ident] :as v}]
  ; https://github.com/hyperfiddle/hyperfiddle/issues/563 - Regression: Schema editor broken due to smart-id
  ; https://github.com/hyperfiddle/hyperfiddle/issues/345 - Form jank when tempid entity transitions to real entity
  (if v
    (or (underlying-tempid ctx id)                          ; Tempid is stable for entire view lifecycle
        (entity-datakey ctx v))))

(defn scalar-key "For at the top when you don't know"
  [ctx v entity-kf]
  (condp some [(type @(:hypercrud.browser/element ctx))]
    #{Aggregate Variable} v
    #{Pull} (entity-kf ctx v)))

(declare element-head)
(declare spread-elements)
(declare qfind-level?)

(defn ^:export row-key "Properly accounts for elements/schema" ; does it return nil, or a v?
  ([ctx row]
   ; The opposite should probably be the default, for userland views.
   (row-key ctx row entity-datakey))
  ([{qfind :hypercrud.browser/qfind :as ctx} row entity-kf]
   ; This keyfn is very tricky, read https://github.com/hyperfiddle/hyperfiddle/issues/341
   #_(let [qfind (contrib.datomic/qfind-collapse-findrel-1 @qfind row)])
   (cond
     (qfind-level? ctx)
     (condp some [(type @qfind)]
       #{FindColl FindScalar}
       (scalar-key (element-head ctx 0) row entity-kf)
       #{FindRel FindTuple}
       (vec                                                 ; Tupled elements have tupled keys, except FindRel-1.
         (for [[i ctx] (spread-elements ctx element-head)]  ; Data is not focused yet, must sidestep that logic
           (scalar-key ctx (get row i) entity-kf))))

     :else
     (scalar-key ctx row entity-kf))))

(def ^:export ^:legacy row-keyfn row-key)
(def ^:export ^:legacy key-row row-key)


;; stable- prefixed functions exist to provide stable references, closures
;; doesn't and would cause re-renders

(defn stable-eav-v [[?e a _] ?v']
  {:pre [#_a
         #_?v'                                              ; Never know with ?v, did they pull identity?
         ]}
  [?e a ?v'])

(defn stable-eav-av "v becomes e. In top cases, this is nil->nil; only Pulls have defined E.
  ?v' can be nil - sparse results."
  [[_ _ ?v] a' ?v']
  ;{:pre [a']} ; failurecase can be nil a'
  [?v a' ?v'])

(defn stable-eav-a [[_ _ v] a']
  ; v becomes e
  [v a' nil])

;(defprotocol EAV
;  (eav [this])
;  (e [this])
;  (a [this])
;  (v [this]))

(defn link-identifiers [link]
  (let [idents (-> (set (:link/class link))                 ; for narrowing if same fiddle in several places? legacy?

                   ; generally name links like this
                   (conj (some-> link :link/fiddle :fiddle/ident))

                   ; fast way to name a button without a fiddle
                   (conj (:link/tx-fn link))

                   ; link/a is probably not useful except for narrowing, maybe
                   ; nil path is no longer defined, use fiddle-ident instead
                   (conj (some-> link :link/path hyperfiddle.fiddle/path))

                   ; nil #{txfn linkpath fiddleident} is not meaningful
                   (disj nil))]                             ; Preserve set type
    [idents link]))

(defn depth [ctx]
  (count (:hypercrud.browser/result-path ctx)))

(defn pull-depth [ctx]
  (count (:hypercrud.browser/pull-path ctx)))

(defn qfind-level? [ctx]
  (= (pull-depth ctx) 0))

(declare data)

(defn row-key-v [ctx row]
  (or (row-key ctx row) row))

(defn entity-key-v [ctx v]
  (or (entity-datakey ctx v) v))

(defn index-result "Builds the result index based on qfind. Result index is orthogonal to sorting or order.
  All paths get a result-index, though if there is no collection, then this is just the result as no indexing is needed.
  (data ctx) is no good until after this is done.
  Works at fiddle and attribute level. Currently we do not index at element level (rather it is already indexed by vec index)"
  ([ctx]
   {:pre [(= (depth ctx) 0)]}
   (let [qfind @(:hypercrud.browser/qfind ctx)]
     (cond                                                  ; fiddle | nested attr (otherwise already spread)

       (not qfind)
       (assoc ctx :hypercrud.browser/result-index (:hypercrud.browser/result ctx))

       (= (depth ctx) 0)                                    ; fiddle level
       (let [r-ordered-result (:hypercrud.browser/result ctx)]
         (condp = (type qfind)
           FindRel
           (assoc ctx :hypercrud.browser/result-index
                      (r/fmap->> r-ordered-result (contrib.data/group-by-unique (r/partial row-key-v ctx))))

           FindColl
           (assoc ctx :hypercrud.browser/result-index
                      (r/fmap->> r-ordered-result (contrib.data/group-by-unique (r/partial row-key-v ctx))))

           FindTuple
           ; Vectors are already indexed
           (assoc ctx :hypercrud.browser/result-index r-ordered-result)

           FindScalar
           ; Scalar has no index at all
           (assoc ctx :hypercrud.browser/result-index r-ordered-result)))
       :else (assert false)
       )))
  ([ctx a]                                                  ; eav order of init issues, ::eav depends on this in :many
   {:pre [(> (pull-depth ctx) 0)]}
   (assert (not (:hypercrud.browser/head-sentinel ctx)) "this whole flag is trouble, not sure if this assert is strictly necessary")
   (assert (boolean (hf/attr ctx a)) (str "attribute " a " not in :hypercrud.browser/schema"))
   (case (contrib.datomic/cardinality @(:hypercrud.browser/schema ctx) a)

     ; For absense of schema provider, we can inspect result here
     :db.cardinality/one
     ctx

     :db.cardinality/many
     ; Group again by keyfn, we have seq and need lookup
     ; Deep update result in-place, at result-path, to index it. Don't clobber it!
     ; hang onto the set as we index for a future rowkey
     (let [set-ungrouped (r/cursor (:hypercrud.browser/result-index ctx) (:hypercrud.browser/result-path ctx))]
       (assoc ctx :hypercrud.browser/result set-ungrouped   ; replace with refined new tree
                  :hypercrud.browser/result-index
                  (r/fmap-> (:hypercrud.browser/result-index ctx)
                            (update-in (:hypercrud.browser/result-path ctx)
                                       (cond
                                         (contrib.datomic/ref? @(:hypercrud.browser/schema ctx) a)
                                         (r/partial contrib.data/group-by-unique (r/partial entity-key-v ctx))

                                         :scalar
                                         ; Sets are an index that evaluate to the key.
                                         set)))))
     nil (throw (ex-info "Invalid Schema Cardinality" {:schema (contrib.datomic/cardinality @(:hypercrud.browser/schema ctx) a) :attribute a})))))


(defn data "Works in any context and infers the right stuff"
  [{qfind :hypercrud.browser/qfind :as ctx}]
  {:pre [ctx]}
  ; TODO validate that there is a row and element if required, and throw a spec error if not.
  ;
  ; Data may be responsible for indexing the result, because it depends on the key paths.
  ; They can be db/id, db/ident, lookupref, alt lookupref. It depends what was pulled.
  ; Userland may use a non-canonical lookup ref and it should work.
  ;
  ; Result-index is precomputed to match the expected path,
  ; in some cases it is just the result (no indexing was done)
  ; The user never sees a raw index, it is just for internal lookups.
  (if qfind
    ; check head-sentinel?
    (let [ctx (-infer-implicit-element ctx)
          {:keys [:hypercrud.browser/element
                  :hypercrud.browser/result
                  :hypercrud.browser/result-index
                  :hypercrud.browser/result-path]} ctx]
      (cond
        (qfind-level? ctx)
        (if result-path
          @(r/cursor result-index result-path)
          @result)

        :else
        (let [k (last result-path)
              is-awaiting-rowkey (and (keyword? k)
                                      (= :db.cardinality/many (contrib.datomic/cardinality ; have result to infer cardinality
                                                                @(:hypercrud.browser/schema ctx) k)))]
          (if is-awaiting-rowkey
            @result                                         ; We hang on to sets as we descend
            @(r/cursor result-index result-path))))
      )))

; It's just easier to end the reaction earlier
(defn v! "returns scalar | identity | lookupref | nil."     ; never a fallback v
  [{:keys [:hypercrud.browser/element] :as ctx}]
  {:pre [element]}                                          ; infer the element above this?
  ; Sparse resultset, v can still be nil, or fully refined to a scalar

  (let [?a (last (:hypercrud.browser/pull-path ctx))
        ?v (data ctx)]
    (cond

      (nil? ?v)
      nil

      (:hypercrud.browser/head-sentinel ctx)                ; do in (data)?
      nil

      ; Attribute level first, makes element level easier
      (> (pull-depth ctx) 0)
      (if (contrib.datomic/ref? @(:hypercrud.browser/schema ctx) ?a)
        (entity-datakey ctx ?v)                             ; no fallback, this must be a real identity or nil
        ?v)

      ; Change in behavior below, it was smart-entity-identifier before
      ; which impacts formula vs view

      ; We already have an element and we already know which
      (= (pull-depth ctx) 0)                                ; element level confirmed
      (condp some [(type @(:hypercrud.browser/qfind ctx))]
        #{FindRel FindColl} (if (> (depth ctx) 0)           ; need row
                              (scalar-key ctx ?v entity-datakey))
        #{FindTuple FindScalar} (scalar-key ctx ?v entity-datakey)))))

(defn eav "Public EAV interface from userland.
  This can't over-infer in the naked iframe case, so inference is eager now"
  ; Context internals use :eav instead of this.
  [ctx]
  {:pre [(s/assert :hypercrud/context ctx)]}
  ; What can be inferred?
  ; If a row in scope, yes infer the v as entity identity
  ; If fiddle level, don't infer the row, it breaks iframe formulas
  (let [#_#_ctx (-infer-implicit-element ctx)]
    @(:hypercrud.browser/eav ctx)))                         ; this could be lazily pulled through :result

(defn e [ctx]
  (let [[e _ _] (eav ctx)]
    e))

(defn a [ctx]
  (let [[_ a _] (eav ctx)]
    a))

(defn v [ctx]
  (let [[_ _ v] (eav ctx)]
    v))

(defn attr
  ([ctx]
   (attr ctx (a ctx)))
  ([ctx a]                                                  ; explicit arity useful for inspecting children
   (some-> (:hypercrud.browser/schema ctx) deref (contrib.datomic/attr a))))

(defn attr?
  ([ctx corcs]
   (attr? ctx (a ctx) corcs))
  ([ctx a corcs]
   (some-> (:hypercrud.browser/schema ctx) deref (contrib.datomic/attr? a corcs))))

(defn element [ctx]
  (some-> ctx -infer-implicit-element :hypercrud.browser/element deref))

(defn element-type [ctx]
  (some-> ctx element contrib.datomic/parser-type))

(defn component? [ctx]
  {:pre [ctx]}
  (let [a (hf/a ctx)]
    (some-> (:hypercrud.browser/parent ctx)
      (attr? a :db/isComponent))))

(defn qfind [ctx]
  (some-> (:hypercrud.browser/qfind ctx) deref))

(defn identity?
  ([ctx]
   (identity? ctx (a ctx)))
  ([ctx a]                                                  ; might not be same a when checking children
   (let [[e _ _] (eav ctx)                                  ; Wrong in child case
         attr (and a (attr ctx a))]
     (cond
       (= :db/id a) true
       ; For first time entity creation only, use e.g. keyword editor to set the identity
       (= :db/ident a) (not (underlying-tempid ctx e))
       ; need to check v also. If there isn't a v (underlying), you are also allowed to set it.
       (attr? ctx a :db.unique/identity) (not (underlying-tempid ctx e)) ; this logic in wrong place?
       :else false))))

; If you ask for an attribute, the row is (may now be) inferred
; If you ask for a row, the element is (may now be) inferred

(defn row "Row does not set E. E is the parent, not the child, and row is analogous to :ref :many child."
  [ctx & [k]]
  {:pre [(s/assert :hypercrud/context ctx)
         #_(not (coll? k))]
   :post [(s/assert :hypercrud/context %)]}
  (s/assert ::result-path-segment k)
  (as-> ctx ctx
    ; What if k is not the canonical key? db/id vs identity, etc
    (update ctx :hypercrud.browser/result-path (fnil conj []) k)
    ; Not pullpath, that is irrespective of the actual data
    ; I don't think row should set v. Only if we infer an element can we set v.
    ; ::eav tells us exactly where we are without optimisim. (eav) infers the rest.

    (if (qfind-level? ctx)
      ; ::eav depends on element which isn't known yet
      ; (eav) can infer this, but keep the "cursor" precise
      (-infer-implicit-element ctx)

      ; In nested case we do know precisely the eav has changed
      (assoc ctx :hypercrud.browser/eav (r/fmap-> (:hypercrud.browser/eav ctx) (stable-eav-v (v! ctx)))))))

(defn ^:export spread-rows "spread across resultset row-or-rows; returns [k ctx] for your react key.
  Automatically accounts for query dimension - no-op in the case of FindTuple and FindScalar.
  Can be skipped in head case, or can address a row directly if you know the keyfn.
  Accounts for row order and handles client sorting."
  [ctx & [sort-fn page-fn]]
  {:pre [(:hypercrud.browser/qfind ctx)]}
  (s/assert :hypercrud/context ctx)
  (let [ctx (assoc ctx :hypercrud.browser/head-sentinel false) ; hack
        sort-fn (or sort-fn identity)
        page-fn (or page-fn identity)]
    (cond                                                   ; fiddle | nested attr (otherwise already spread)
      (= (depth ctx) 0)                                     ; fiddle level
      (let [{r-qfind :hypercrud.browser/qfind} ctx
            ; NOT result-indexed, use the raw one. It's been indexed already, can't use that, order is lost.
            r-ordered-result (:hypercrud.browser/result ctx)]
        (condp = (type @r-qfind)
          FindColl
          (for [[?k k] (->> (page-fn (sort-fn @r-ordered-result)) ; client side sorting – should happen in backend
                            (map (juxt (partial row-key ctx)
                                       (partial row-key-v ctx))))]
            [?k (row ctx k)])

          FindRel
          (for [[?k k] (->> (page-fn (sort-fn @r-ordered-result)) ; client side sorting – should happen in backend
                            (map (juxt (partial row-key ctx)
                                       (partial row-key-v ctx))))]
            [?k (row ctx k)])

          FindTuple                                         ; no index
          (let [?k (row-key ctx @r-ordered-result)]
            [[?k ctx]])

          FindScalar                                        ; no index
          (let [?k (row-key ctx @r-ordered-result)]
            [[?k ctx]])))

      ; where is the cardinality test? can elide, assume :many if they called spread-rows down here.
      ; Better also be past qfind level and into a pull
      (> (depth ctx) 0)                                     ; nested attr
      (let [r-ordered-result (data ctx)]                    ; remember row order; this is broken check order
        (for [[?k k] (->> (sort-fn r-ordered-result)
                          (map (juxt (partial row-key ctx)
                                     (partial row-key-v ctx))))]
          [?k (row ctx k)])))))

(defn- validate-fiddle [fiddle]
  (if-let [ed (s/explain-data :hyperfiddle/fiddle fiddle)]
    (either/left (ex-info "Invalid fiddle" {:fiddle/ident (:fiddle/ident fiddle)
                                            ::s/problems  (::s/problems ed)}))
    (either/right fiddle)))

(defn- augment-with-spec [spec #?(:cljs ^js schema, :clj schema)]
  (-> spec
      (:attributes)
      (spec-datomic/spec->schema)
      (merge (some-> schema .-schema-by-attr))
      (contrib.datomic/->Schema)))

(defn fiddle+ "Runtime sets this up, it's not public api.
  Responsible for setting defaults.
  Careful this is highly sensitive to order of initialization."
  [ctx r-fiddle]
  {:pre [r-fiddle]}
  ; todo applying fiddle defaults should happen above here
  ; todo fiddle should already be valid
  (mlet [r-qparsed @(r/apply-inner-r (r/fmap hyperfiddle.fiddle/parse-fiddle-query+ r-fiddle))
         _ (if (and (not= :eval @(r/cursor r-fiddle [:fiddle/type]))
                    @(r/fmap (r/comp nil? :qfind) r-qparsed))
             (either/left (ex-info "Invalid qfind" {}))     ; how would this ever happen?
             (either/right nil))
         _ @(r/apply contrib.datomic/validate-qfind-attrs+
                     [(r/track runtime/get-schemas (:runtime ctx) (:partition-id ctx))
                      (r/fmap :qfind r-qparsed)])
         r-fiddle @(r/apply-inner-r (r/apply hyperfiddle.fiddle/apply-fiddle-links-defaults+
                                             [r-fiddle
                                              (r/track runtime/get-schemas (:runtime ctx) (:partition-id ctx))
                                              r-qparsed]))
         r-fiddle @(r/apply-inner-r (r/fmap validate-fiddle r-fiddle))]
    (return
      ; Can't keep track of reaction types. Just set the key to (reactive nil) rather than omit the key.
      (assoc ctx
        :hypercrud.browser/fiddle r-fiddle
        :hypercrud.browser/qfind (r/fmap :qfind r-qparsed)
        :hypercrud.browser/qparsed r-qparsed
        :hypercrud.browser/link-index (r/fmap->> r-fiddle :fiddle/links (map link-identifiers))
        :hypercrud.browser/eav (r/apply stable-eav-av
                                        [(r/pure nil)
                                         (r/pure nil) #_(r/fmap :fiddle/ident r-fiddle)
                                         (r/pure nil)])
        ; push this down, it should be nil now
        :hypercrud.browser/pull-path []))))

(defn fiddle [ctx]
  (-> ctx :hypercrud.browser/fiddle deref :fiddle/ident))

(defn result-enclosure! [ctx]
  (contrib.datomic/result-enclosure!
    (runtime/get-schemas (:runtime ctx) (:partition-id ctx))
    @(:hypercrud.browser/qfind ctx)
    @(:hypercrud.browser/result ctx)))

(declare def-validation-message)

(defn spec-semantic-in [row-keyfn value in]  ; ({:id 1234 :some :value}  {:id 4321 :some :value})
  ; Trace the :in through the :val
  ; if the thing has an identity, use that instead
  ; accumulate the new path
  (reduce (fn [in k]
            (let [value (get-in value (conj in k))
                  k (if (int? k)
                      (or (row-keyfn value) k)
                      k)]
              (conj in k)))
    []
    in))

(defn result-explained-for-view
  "Efficient views need a stable keyfn to descend into rows. Canonicalize the spec problem :in's
  to align with the view's keyfn."
  [row-keyfn {::s/keys [value] :as e}]                      ; Need entire explained struct, problems alone is not enough
  (update e ::s/problems (fn [problems]
                           (map #(update % :in (partial spec-semantic-in row-keyfn value))
                             problems))))

(declare validation-messages)

(defn form-cell-problem
  "Re-path the problems so the UI (table or form) renders invalid where the problem is.
  Also looks up the invalid message using certain business rules to match the missing case.

  Todo: Perhaps we should return the problem itself so the user can match on it to decide the invalid message?
  Todo: what to do in diamond case - tax return is invalid, you need etiher a ssn or a visa, which field has the problem?"
  [{:keys [in val pred via] :as problem}]
  (let [pred (s/abbrev pred)]                               ; also flattens the 'contains? pred
    (#?(:clj match :cljs match*)
      pred

      ; Spec reports missing keys at the parent :in, path it at the child instead
      (['contains? '% k] :seq)                           ; Detect specific case of missing a key
      [(conj in k) (@validation-messages k)]                ; use the child path to lookup the invalid (missing) message

      _
      [in (@validation-messages (last via))])))

(defonce ^:private validation-messages (atom {}))           ; Should probably be a value in view props, not a registry

(defn get-validation-message [k]
  ;; readonly
  (get @validation-messages k))

(defmethod hf/def-validation-message :default [pred s]
  {:pre [(keyword pred)                                     ; name the spec at the granularity of the error message you want
         (not (clojure.string/blank? s))]}
  (swap! validation-messages assoc pred s)
  nil)

(defn form-validation-hints
  "UI for forms need to validate at the cell granularity, not the record, so certain
  types of problems need to be re-pathed from entity to attribute.

      ([[2 :foo/bar] :contrib.validation/missing])"
  [problems]                                                ; destructure ::s/problems at call site
  ; Don't collect here, they get filtered down later
  (map form-cell-problem problems))

(defn- fdef? [?spec]
  (and #?(:clj  (instance? clojure.lang.ILookup ?spec)
          :cljs (satisfies? cljs.core.ILookup ?spec))
       (contains? ?spec :args)
       (contains? ?spec :ret)
       (contains? ?spec :fn)))

(defn validate-result [?spec value keyfn]
  {:pre [keyfn]}
  (let [?spec (if (fdef? ?spec) (:ret ?spec) ?spec)]
    (when ?spec ; just make this easy, specs are always sparse
      (when-let [explain (s/explain-data ?spec value)]
        (let [problems (::s/problems (result-explained-for-view keyfn explain))]
          (form-validation-hints problems))))))

(defn validation-hints-enclosure! [ctx]
  (validate-result
    (s/get-spec @(r/fmap-> (:hypercrud.browser/fiddle ctx) :fiddle/ident))
    @(:hypercrud.browser/result ctx)
    ; i dont think fallback v
    (partial row-key ctx)))

(defn result [ctx r-result]                                 ; r-result must not be loading
  {:pre [r-result
         (:hypercrud.browser/fiddle ctx)]}
  (as-> ctx ctx
    (assoc ctx :hypercrud.browser/result r-result)          ; can be nil if no qfind
    (assoc ctx                                              ; uses result
      ; result-enclosure! can throw on schema lookup, but it doesn't need handled in theory,
      ; because how would you have data without a working schema?
      ; this may not prove true with the laziness of reactions
      :hypercrud.browser/result-enclosure (r/track result-enclosure! ctx)
      :hypercrud.browser/validation-hints (r/track validation-hints-enclosure! ctx))
    ; index-result implicitly depends on eav in the tempid reversal stable entity code.
    ; Conceptually, it should be after qfind and before EAV.
    (index-result ctx)))                                    ; in row case, now indexed, but path is not aligned yet

(defn to-map [route]
  (let [[f & args] route]
    (if (empty? args)
      {}
      (when-let [spec (s/get-spec f)]
        (let [spec-args (-> spec (spec/parse) (spec/args-spec))
              names     (spec/names spec-args)]
          (case (:type spec-args)
            ::spec/cat (zipmap names args)
            ;; TODO drop s/alt support?
            ::spec/alt (zipmap (spec/best-match-for names args) args)))))))

(defn derive-for-search-defaults
  "Build a new context with route defaults as result so they can be rendered as a form."
  [{:keys [:hypercrud.browser/route-defaults] :as ctx}]
  (-> ctx
      (assoc ;; Remove :hyperfiddle.route/fiddle from route defaults
       :hypercrud.browser/result (r/fmap to-map route-defaults) ; use spec to draw search params
       ;; We want to render as a form, so qfind is FindScalar
       :hypercrud.browser/qfind (r/pure (fiddle/shape 'FindScalar)))
      (dissoc :hypercrud.browser/route-defaults) ; avoid potential recursion
      (as-> ctx
          (assoc ctx :hypercrud.browser/result-enclosure (r/track result-enclosure! ctx)
                 :hypercrud.browser/validation-hints (r/track validation-hints-enclosure! ctx)))
      (index-result)))

(defn stable-element-schema! [rt pid element]
  (let [{{db :symbol} :source} element]
    (some->> db str (runtime/get-schema+ rt pid) deref)))

; This complects two spread-elements concerns which are separate.
; 1. Setting Schema and element
; 2. Setting result and result related things
(defn element-head [ctx i]
  (let [r-element (r/fmap-> (:hypercrud.browser/qfind ctx) datascript.parser/find-elements (get i))]
    (-> ctx
        (assoc :hypercrud.browser/element r-element
        ; stable-element-schema! can throw on schema lookup
        ; this may need to be caught depending on when how/when this function is used
        ; or with reactions; we may need to unlazily throw ASAP
               :hypercrud.browser/schema (r/fmap->> r-element (stable-element-schema! (:runtime ctx) (:partition-id ctx)))))))

(defn browse-element [ctx i]                                ; [nil :seattle/neighborhoods 1234345]
  {:pre []
   :post [(s/assert r/reactive? (:hypercrud.browser/qfind %)) ; qfind set in base if it is available
          (s/assert r/reactive? (:hypercrud.browser/schema %))]}
  (cond
    (:hypercrud.browser/element ctx)                        ; this was eagerly inferred. Weird spot to do it but makes spread-elements f-override logic work
    ctx

    :else
    (let [r-pull-enclosure (r/fmap-> (:hypercrud.browser/result-enclosure ctx) (get i))]
      (as-> ctx ctx
        (element-head ctx i)
        (assoc ctx :hypercrud.browser/root-pull-enclosure r-pull-enclosure) ; for refocus
        (assoc ctx :hypercrud.browser/pull-enclosure r-pull-enclosure)
        (condp some [(type @(:hypercrud.browser/qfind ctx))]
          #{FindRel FindTuple} (as-> ctx ctx
                                 (update ctx :hypercrud.browser/result-path (fnil conj []) i)
                                 ; I don't think we index result here, uncertain.
                                 ; Related to collapse FindRel-N to FindColl
                                 (assoc ctx :hypercrud.browser/element-index i)) ; used only in labels
          #{FindColl FindScalar} ctx)

        ; Do last, result-path is set
        ; If there is only one element, should we set V?
        ; Hard to interpret it since we don't know what it is (entity, aggregate, var)
        ; Setting :hypercrud.browser/validation-hints doesn't make sense here as specs are keyword oriented
        ; so at the fiddle level, or at the attribute level, but not relations.
        ;
        ; We can set i as the a, but you can't have links on i, we want to leave the fiddle-ident as the a.
        ; a has to be semantic. does it matter?
        (assoc ctx :hypercrud.browser/eav (r/fmap-> (:hypercrud.browser/eav ctx) (stable-eav-v (v! ctx))))))))

(defn -infer-implicit-element "auto-focus single elements - legacy field path compat"
  [ctx]
  (let [?qfind (some-> ctx :hypercrud.browser/qfind deref)]
    (cond
      (:hypercrud.browser/element ctx)
      ctx

      (#{FindColl FindScalar} (if ?qfind (type ?qfind)))
      (browse-element ctx 0)

      ; Can't infer this because the result index is not the right shape
      ;(and (#{FindRel FindTuple} (type qfind))
      ;     (= 1 (count (datascript.parser/find-elements qfind))))
      ;(element ctx 0)

      :else
      ctx)))

(defn -validate-qfind-element [ctx]
  {:pre [(s/assert r/reactive? (:hypercrud.browser/qfind ctx))
         (s/assert r/reactive? (:hypercrud.browser/element ctx))]}
  ctx)

(defn attribute "Will still set EA and V if we have it, e.g. works in table-head position (no row set).
  V becomes E"
  [ctx a']
  {:pre [(s/assert :hypercrud/context ctx)
         (s/assert keyword? a')]
   :post [(s/assert :hypercrud/context %)]}
  (let [{:keys [:hypercrud.browser/pull-path
                :hypercrud.browser/result-path
                :hypercrud.browser/qfind]} ctx]

    ; Schema aliases can crash here https://github.com/hyperfiddle/hyperfiddle.net/issues/182
    ;; #_(if-not (contrib.datomic/cardinality @(:hypercrud.browser/schema ctx) a')
    ;;     ctx)
    (as->
      ctx ctx
      (-infer-implicit-element ctx)                         ; ensure pull-enclosure
      ;; #_(-validate-qfind-element ctx)
      (set-parent ctx)
      (update ctx :hypercrud.browser/pull-path (fnil conj []) a') ; what is the cardinality? are we awaiting a row?
      (if (:hypercrud.browser/head-sentinel ctx)
        ctx                                                 ; no result-path in head, or result
        (as-> ctx ctx
          (update ctx :hypercrud.browser/result-path (fnil conj []) a')
          ; Guaranteed depth >= 1 due to stmt ordering
          (index-result ctx a')))
      ; V is for formulas, E is for security and on-change. V becomes E. E is nil if we don't know identity.
      (assoc ctx :hypercrud.browser/eav                     ; insufficent stability on r-?v? fixme
                 (case (contrib.datomic/cardinality @(:hypercrud.browser/schema ctx) a') ; have result data to infer cardinality in absense of schema
                   :db.cardinality/many (r/fmap-> (:hypercrud.browser/eav ctx) (stable-eav-a a')) ; dont have v yet
                   :db.cardinality/one (r/fmap-> (:hypercrud.browser/eav ctx) (stable-eav-av a' (v! ctx)))
                   ; Gracefully fail but still render.
                   nil (r/fmap-> (:hypercrud.browser/eav ctx) (stable-eav-av nil nil))))

      ; in :hf/new :identity case, E can be nil but we are about to have a tempid in the E
      ; It is not our job here to focus that. I dont think we can set the E to the tempid in that case.
      )))

(defn validation-hints-here [ctx]
  (for [[path hint] @(:hypercrud.browser/validation-hints ctx)
        :when (= path (:hypercrud.browser/result-path ctx))
        :let [a (last path)]]
    [a hint]))

(defn tree-invalid? "For popover buttons (fiddle level)" [ctx]
  {:pre [ctx]}
  (->> @(:hypercrud.browser/validation-hints ctx)
       seq boolean))

(defn leaf-invalid? "The thing that styles red" [ctx]
  (->> (validation-hints-here ctx)
       seq boolean))

(defn pull-enclosure-here "safe to be called anywhere"
  [ctx]
  (when (:hypercrud.browser/element ctx)
    (contrib.datomic/pullshape-get-in
      @(:hypercrud.browser/pull-enclosure ctx)
      (:hypercrud.browser/pull-path ctx))))

(defn children [ctx]
  (contrib.datomic/pull-level
    (pull-enclosure-here ctx)))

(defn focus "Unwind or go deeper, to where we need to be, within same dimension.
    Throws if you focus a higher dimension.
    This is about navigation through pulledtrees which is why it is path-oriented."
  [ctx relative-path]                                       ; fq path?
  {:pre [(s/assert :hypercrud/context ctx)
         (:hypercrud.browser/result ctx)]
   :post [(s/assert :hypercrud/context %)]}
  ; Is this legacy compat?
  (reduce (fn [ctx p]
            (cond
              (int? p) (browse-element ctx p)
              ;(#{:db/id #_:db/ident} p) ctx                 ; v is already properly set
              ; db/id is renderable, address it. The controls will unwind to parent ctx. Tangled
              (keyword? p) (attribute ctx p)
              :else (assert false (str "illegal focus: " p))))
          ctx relative-path))

(defn ^:export spread-result
  "Use this with `for` which means reagent needs the key."
  [ctx]
  (let [r-fiddle (:hypercrud.browser/fiddle ctx)]
    ; could also dispatch on qfind. Is fiddle/type unnecessary now?
    (condp some [(:fiddle/type @r-fiddle)]
      #{:eval} [[(:fiddle/ident @r-fiddle) ctx]]            ; Option[Tuple[_]]
      #{:query :entity} [[(:fiddle/ident @r-fiddle)
                          ; inference is lazy
                          ; But it needs to add something, so that we know that it is inferrable now
                          ; presence of path param?
                          ctx #_(-infer-implicit-element ctx)]])))

(defn ^:export spread-elements "yields [i ctx] foreach element.
  All query dimensions have at least one element.
  Default arity is for userland.
  Second arity is for keyfns who need elements but not data."
  [ctx & [f]]
  {:pre [(:hypercrud.browser/qfind ctx)
         #_(not (:hypercrud.browser/element ctx))]
   #_#_:post [(s/assert :hypercrud/context %)]}
  (let [r-qfind (:hypercrud.browser/qfind ctx)]
    ; No unsequence here? What if find elements change? Can we use something other than (range) as keyfn?
    (for [i (range (count (datascript.parser/find-elements @r-qfind)))]
      [i ((or f browse-element) ctx i)])))

(defn ^:export spread-attributes "not recursive, just one entity level"
  [ctx]
  (let [{:keys [:hypercrud.browser/element] :as ctx} (-infer-implicit-element ctx)
        el @element]
    (case (unqualify (contrib.datomic/parser-type el))
      ; Handle variable and aggregate above
      :variable [#_[(get-in element [:variable :symbol]) ctx]]
      :aggregate [#_[(get-in element [:fn :symbol]) ctx]]
      :pull (for [k (children ctx)]
              [k (attribute ctx k)]))))

; var first, then can always use db/id on row. No not true – collisions! It is the [?e ?f] product which is unique

; keyfn must support entities & scalars (including aggregates)
; scalars may not be unique
; entities might not be unique either, its the combination that is unique
; lift out of current context in this case?
; Make sure to optimize the single-fe case to use the identity/value which will be unique

(defn link-criteria-match? [?corcs [index-key v]]
  (clojure.set/superset? index-key (contrib.data/xorxs ?corcs #{})))

(defn links-at "where criterias is some of #{ident txfn class class2}.
  Links include all links reachable by navigating :ref :one. (Change this by specifying an :ident)
  The index internals are reactive."
  [index criterias]
  (->> index
       (filter #(link-criteria-match? criterias %))
       (mapv second))
  #_#_[r-link-index criterias]                              ; criterias can contain nil, meaning toptop
      (r/fmap->> r-link-index
                 (filter (r/partial link-criteria-match? criterias))
                 (mapv second)
                 #_(mapv (juxt :db/id identity))))

(defn reachable-pullpaths "
  txfn can be on scalar and it is harmless to allow this."
  [ctx]
  (let [result-path (:hypercrud.browser/result-path ctx)    ; [:domain/databases 17592186046511 :db/id]
        schema @(:hypercrud.browser/schema ctx)
        root-pullshape @(:hypercrud.browser/root-pull-enclosure ctx)]
    ; Start at the top. Traverse. Are we satisfied?
    ; :one is satisfied; :many is satisfied if we have it in the dependency path
    ; So turn result-path into {:domain/databases 17592186046511} ?
    ; This fails in complex pulls
    (let [one? #(contrib.datomic/cardinality? schema % :db.cardinality/one)
          satisfied? (set (filter keyword? result-path))
          child? (set (children ctx))]
      (contrib.datomic/pull-traverse schema root-pullshape #(or (one? %)
                                                                (satisfied? %)
                                                                (child? %))))))

(defn reachable-attrs [ctx]
  (->> (reachable-pullpaths ctx)
       (map last)
       (remove nil?)
       distinct))

(defn links-in-dimension' [ctx criterias]
  {:post [(not (r/reactive? %))]}
  ; https://github.com/hyperfiddle/hyperfiddle/issues/909
  (let [index @(:hypercrud.browser/link-index ctx)          ; lift to top #909
        ?element (some-> ctx :hypercrud.browser/element deref)
        ?schema (some-> ctx :hypercrud.browser/schema deref)
        ?pullpath (:hypercrud.browser/pull-path ctx)]
    (if-not (and ?element ?schema ?pullpath)                ; this if statement was causing chaos #909
      (links-at index criterias)
      (->> (reachable-attrs ctx) ; scan for anything reachable ?
           ; Places within reach
           (map (fn [a] (links-at index (conj criterias a))))
           ; This place is where we are now
           (cons (links-at index criterias)) ; this causes duplicates, there are bugs here
           (into [] (comp cat (distinct))) ; associative by index
           ))))

(defn links-in-dimension [ctx criterias]
  ; r/track
  (links-in-dimension' ctx criterias))

(defn unwind [ctx n]
  ((apply comp (repeat n :hypercrud.browser/parent)) ctx))

(defn refocus-in-element+
  [ctx a]
  {:pre [(:hypercrud.browser/element ctx)]
   :post [(s/assert either? %)]}
  (cond
    (contrib.datomic/unique? @(:hypercrud.browser/schema ctx) a :db.unique/identity)
    (if (qfind-level? ctx)
      (right ctx)                                           ; [nil :dustingetz.tutorial/blog [:dustingetz.post/slug :hehehe]]
      (right (unwind ctx 1)))

    (= a (a ctx))
    (right ctx)

    :else
    (let [current-path (:hypercrud.browser/pull-path ctx)
          ; find a reachable path that contains the target-attr in the fewest hops
          ;   me->mother ; me->sister->mother ; closest ctx is selected
          ; What if there is more than one?  me->sister->mother; me->father->mother
          ; Ambiguous, how did we even select this link? Probably need full datascript query language.
          path-solutions (->> (reachable-pullpaths ctx)
                              ; xs is like '([] [:dustingetz.reg/gender] [:dustingetz.reg/shirt-size])
                              (filter #(= a (last %))))]
      ; In tuple cases (off happy path) there might not be a solution. Can be fixed by tupling all the way up.
      (if (seq path-solutions)                              ; found at least one solution
        ; Warn if more than one solution? Diamonds are fine, but some are parallel solns
        (let [chosen-path (->> path-solutions
                               (sort-by count)              ; choose the shortest path, if ambiguous could be a problem
                               first)                       ; nil/empty means unwind to nop
              common-ancestor-path (ancestry-common current-path chosen-path)
              unwind-offset (- (count current-path) (count common-ancestor-path)) ; 1 > 0 is fine
              common-ancestor-ctx (unwind ctx unwind-offset)

              foo (if (contrib.datomic/unique? @(:hypercrud.browser/schema ctx) a :db.unique/identity)
                    ; FOR LINK REFOCUS ONLY, the parent entity ctx is correct
                    (butlast chosen-path)
                    chosen-path)]
          (right
            (focus common-ancestor-ctx (ancestry-divergence foo current-path))))

        ; No solution found.
        ; Sparse resultset, or possibly a constraint failure (userland asked for something impossible?)
        (left (str "unable to satisfy link at: " a))))))

(defn refocus+ "From view, find the closest satisfactory ctx accounting for cardinality. e.g. as in
  !link(:new-intent-naive :hf/remove). Caller believes that the deps are satisfied (right stuff is in scope).
  todo unify with refocus'.
  This is ALWAYS focusing a link.
  Handle :identity differently for links – we are not rendering forms, we are generating URLs,
  which means :identity attributes need to name an entity, not a scalar. (This fn is complected with build-args+)

  Notably: (refocus) and (attribute) are pretty much the same thing. This one has element tupling hacks
  and handles :identity differently, now."
  [ctx a']
  {:pre [ctx #_a']                                          ; disable assert for backwards compat until all links have a
   ; nil return is allowed, does this return either?
   :post [(s/assert either? %)]}
  (cond
    (not a')                                                ; backwards compat with Dec 2018 link/path
    (right ctx)

    (= a' (@(:hypercrud.browser/fiddle ctx) :fiddle/ident))
    ; if no element, we already there (depth = 0)
    (right (unwind ctx (pull-depth ctx)))

    (:hypercrud.browser/element ctx)
    (refocus-in-element+ ctx a')

    (let [qfind @(:hypercrud.browser/qfind ctx)]
      (and (#{FindRel FindTuple} (type qfind))
           (= 1 (count (datascript.parser/find-elements qfind)))))
    (refocus-in-element+ (browse-element ctx 0) a')

    :else
    ; This never happens in autogrids, only in custom views.
    ; If the custom view is on a FindRel-1, this does the right thing.
    ; If the custom view is a FindRel-N, then this needs to tuple all the way up.
    ; It is probably not what they want. They should make an element ctx and place
    ; each link individually. This hack probably never needs to get fixed.
    (refocus-in-element+ (browse-element ctx 0) a')))

(defn tag-v-with-color' [ctx v]
  (if v
    (->ThinEntity (or (dbname ctx) "$")                     ; busted element level
                  v)))

(defn tag-element-v-with-color [{:keys [:hypercrud.browser/element] :as ctx} v]
  {:pre [element]}
  (condp some [(type @element)]
    #{Pull} (tag-v-with-color' ctx v)
    #{Aggregate Variable} v))

(defn tag-v-with-color "Tag dbids with color, at the last moment before they render into URLs"
  [ctx v]
  (let [[e a _] (eav ctx)
        is-element-level (= (pull-depth ctx) 0)]
    (cond
      (instance? ThinEntity v) v                            ; legacy compat with IDE legacy #entity formulas
      #_#_(instance? DbName v) v

      is-element-level                                      ; includes hf/new
      (do
        ; Includes FindColl and FindScalar inferred above
        ; We can hack in FindRel-1 support here too
        (let [ctx (-infer-implicit-element ctx)             ; don't infer FindRel-1, the result index isn't shaped right
              element (:hypercrud.browser/element ctx)]
          (if element
            (tag-element-v-with-color ctx v)
            v)))

      (contrib.datomic/ref? @(:hypercrud.browser/schema ctx) a)
      (tag-v-with-color' ctx v)

      ; :dustingetz/slack-storm :new-storm exercises this branch
      (contrib.datomic/unique? @(:hypercrud.browser/schema ctx) a :db.unique/identity)
      (if e                                                 ; tempid | lookup-ref | nil (:hf/new)
        (tag-v-with-color' ctx e)
        (tag-v-with-color' ctx v))                          ; :hf/new has nil e, but valid tempid v

      :scalar
      v)))

(defn build-args "Params are EAV-typed (uncolored)"
  ;; There is a ctx per argument if we are element-level tuple.
  [ctx link]
  ;; if at element level, zip with the find-elements, so do this N times. That
  ;; assumes the target query is a query of one arg. If it takes N args, we can
  ;; apply as tuple. If they misalign thats an error. Return the tuple of args.
  (let [;; V legacy is tuple, it should be scalar by here (tuple the ctx, not
        ;; the v)
        [e a v]    (eav ctx)
        ;; Documented behavior is v in, tuple out, no colors.
        arg        (hf/formula ctx link v)]
    ;; Don't normalize, must handle tuple dimension properly. For now assume no
    ;; tuple.
    ;; !link[⬅︎ Slack Storm](:dustingetz.storm/view)
    arg))

(defn ^:export build-route+ "There may not be a route! Fiddle is sometimes optional" ; build-route+
  [args ctx]
  {:pre  [ctx]
   :post [(s/assert either? %)]}
  (mlet [f (let [link @(:hypercrud.browser/link ctx)]
             (if-let [fiddle (:link/fiddle link)]
                       (right (:fiddle/ident fiddle))
                       (left {:message ":link/fiddle required" :data {:link link}})))]
         ; Why must we reverse into tempids? For the URL, of course.
        (-> (filter identity args)
            (->> (map (partial tag-v-with-color ctx)))
            ;; this ctx is refocused to some eav
            (conj f)
            (return))))

(defn refocus-to-link+ "focus a link ctx"
  [ctx link-ref]
  {:pre [(s/assert :hypercrud/context ctx)]
   :post [(s/assert either? %)]}
  (->> (hyperfiddle.fiddle/path @(r/cursor link-ref [:link/path]))
       (refocus+ ctx)
       (cats/fmap #(assoc % :hypercrud.browser/link link-ref))))

(defn occlude-eav [ctx untagged-uncolored-datomic-args]     ; is it important these arguments are untagged/uncolored?
  (let [[v' & _vs] untagged-uncolored-datomic-args]
    ; EAV sugar is not interested in tuple case, that txfn is way off happy path
    (assoc ctx :hypercrud.browser/eav (r/apply stable-eav-v
                                               [(:hypercrud.browser/eav ctx)
                                                (r/track identity v')]))))

(defn build-route-and-occlude+ [ctx link-ref]
  (do-result
   (let [args  (build-args ctx @link-ref)
         ;; :hf/remove doesn't have route by default, :hf/new does, both can be customized
         route (from-result (build-route+ args ctx))
         ctx   (occlude-eav ctx args)]
      [ctx route])))

(defn refocus-build-route-and-occlude+ "focus a link ctx, accounting for link/formula which occludes the natural eav"
  [ctx link-ref]
  {:pre [(s/assert :hypercrud/context ctx)]
   :post [(s/assert either? %)]
   #_#_:post [(s/assert (s/cat :ctx :hypercrud/context :route r/reactive?) %)]}
  (>>= (refocus-to-link+ ctx link-ref)
       #(build-route-and-occlude+ % link-ref)))

(defn link [ctx]
  (some-> (:hypercrud.browser/link ctx) deref))

(defn link-class [?ctx]
  (if-let [link-ref (:hypercrud.browser/link ?ctx)]
    @(r/fmap-> link-ref :link/class)))

(defn link-fiddle [?ctx]
  (if-let [link-ref (:hypercrud.browser/link ?ctx)]
    @(r/fmap-> link-ref :link/fiddle)))

(defn link-tx [ctx]
  {:post [(or (keyword? %)
              (nil? %))]}
  (when-let [link-ref (:hypercrud.browser/link ctx)]
    (when-let [x @(r/cursor link-ref [:link/tx-fn])]
      (assert (not (string? x)) (str `link/tx-fn " : The link/tx-fn should not be a string anymore"))
      (cond
        (keyword? x) x
        (string? x)  (some-> x blank->nil keywordize)
        :else        (throw (ex-info "Unsuported link-tx value. Expected keyword or keyword as a strings." {:value x
                                                                                                            :type  (type x)}))))))

(defn branched-link?' [link]                                ; todo remove
  (-> link :link/tx-fn blank->nil some?))

(defn branched-link?
  "branched iframes are lazy loaded (e.g. Rosie sub-block)"
  [ctx]
  (branched-link?' (link ctx))
  #_@(r/fmap (r/comp some? blank->nil :link/tx-fn) (:hypercrud.browser/link ctx)))

(defn hash-portable [v]
  ; Transactions have db/id longs and longs hash differently on cljs vs clj
  (hash (pr-str v)))

(let [impl (fn [rt pid dbname]
             (->> (loop [pid pid
                         tx nil]
                    (let [tx (concat tx (runtime/get-stage rt pid dbname))]
                      (if-let [parent-pid (runtime/get-branch-parent-pid rt pid)]
                        (recur parent-pid tx)
                        tx)))
                  hash-portable
                  (str "hyperfiddle.tempid-")))]
  (defn tempid! "Generate a stable unique tempid that will never collide and also can be deterministicly
  reproduced in any tab or the server"
    ([ctx]
     (tempid! (or (dbname ctx) "$") ctx))
    ([dbname ctx]
     ; Use hash of current dbval, which changes with each edit
     @(r/track impl (:runtime ctx) (:partition-id ctx) dbname))))

(defn build-pid-from-link [parent-ctx link-ctx initial-route]
  ; try to auto-generate branch/popover-id from the product of:
  ; - link's :db/id
  ; - route
; - visual-ctx's data & path (where this popover is being drawn NOT its dependencies)
  (let [v [(if (:hypercrud.browser/qfind link-ctx)          ; guard crash on :blank fiddles
             (eav link-ctx)                                 ; if this is nested table head, [e a nil] is ambiguous. test: /:intents/
             (:hypercrud.browser/result-path link-ctx))
           @(r/fmap (r/partial entity-viewkey link-ctx)
                    (:hypercrud.browser/fiddle link-ctx))
           @(r/cursor (:hypercrud.browser/link link-ctx) [:db/id])
           initial-route
           ; visual-a de-dupes various identity columns which show exactly the same link
           #_(eav parent-ctx)]
        pid (str (hash (map pr-str v)))]
    ;(timbre/debug {:pid pid :args v})
    pid))

(defn set-partition [ctx partition-id]
  (-> ctx
    ; Dustin considered saving parent-ctx so popover can get parent route, but txfn is already in parent ctx so didn't need it
    #_(set-parent)
    (assoc :partition-id partition-id)
    clean))

(defrecord Context []
  hf/Browser
  (dbname [ctx]
    (dbname ctx))
  (data [ctx]
    (data ctx))
  (fiddle [ctx]
    (fiddle ctx))
  (a [ctx]
    (a ctx))
  (browse-element [ctx i]
    (browse-element ctx i))
  (e [ctx]
    (e ctx))
  (eav [ctx]
    (eav ctx))
  (attr [ctx]
    (attr ctx))
  (attr [ctx a]
    (attr ctx a))
  (attr? [ctx corcs]
    (attr? ctx corcs))
  (attr? [ctx a corcs]
    (attr? ctx a corcs))
  (element [ctx]
    (element ctx))
  (identity? [ctx]
    (identity? ctx))
  (element-type [ctx]
    (element-type ctx))
  (id [ctx pulltree]
    (entity-datakey ctx pulltree))
  (tempid! [ctx]
    (tempid! ctx))
  (tempid! [ctx dbname]
    (tempid! dbname ctx))                                   ; careful, params flipped
  (qfind [ctx]
    (qfind ctx))
  (qfind-level? [ctx]
    (qfind-level? ctx))
  (row-key [ctx row]
    (row-key ctx row))
  (spread-attributes [ctx]
    (spread-attributes ctx))
  (link-tx [ctx]
    (link-tx ctx))
  (v [ctx]
    (v ctx))
  ;; Solves a client side :require order issue. Was implemented in hf.ui via
  ;; extend-type for architectural reasons. It’s not problematic to implement it
  ;; here anymore since we want to run context on the client side only.
  hf/UI
  (display-mode [ctx]
    (:hyperfiddle.ui/display-mode ctx))
  (display-mode? [ctx k]
    (= k (unqualify (hf/display-mode ctx)))))

(defmethod hf/subject Context [ctx] (hyperfiddle.runtime/get-user-id (:runtime ctx)))
(defmethod hf/db-record Context [ctx] (hf/database (hf/domain (:runtime ctx)) (hf/dbname ctx)))

(defn- update-v [id->tempid schema a v]
  (if (contrib.datomic/ref? schema a)
    (get id->tempid v v)
    v))

(defmethod hf/stmt-id->tempid :db/add [id->tempid schema [op e a v]]
  (let [e (get id->tempid e e)
        v (update-v id->tempid schema a v)]
    [op e a v]))

(defmethod hf/stmt-id->tempid :db/retract [id->tempid schema [op e a v]]
  ; same as :db/add
  (let [e (get id->tempid e e)
        v (update-v id->tempid schema a v)]
    [op e a v]))

(defmethod hf/stmt-id->tempid :db/retractEntity [id->tempid schema [op e]]
  [op (get id->tempid e e)])

(defmethod hf/stmt-id->tempid :db/cas [id->tempid schema [op e a ov nv]]
  [op (get id->tempid e e) a
   (update-v id->tempid schema a ov)
   (update-v id->tempid schema a nv)])

(defmethod hf/stmt-id->tempid :default [id->tempid schema stmt]
  stmt)
