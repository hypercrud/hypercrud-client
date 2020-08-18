(ns contrib.datomic
  (:require
    [clojure.set :as set]
    [cats.core :refer [>>=]]
    [cats.monad.either :as either]
    [clojure.spec.alpha :as s]
    [clojure.set]
    [contrib.data :refer [group-by-pred]]
    [contrib.try$ :refer [try-either]]
    [datascript.parser #?@(:cljs [:refer [FindRel FindColl FindTuple FindScalar Variable Aggregate Pull]])]
    [hyperfiddle.spec :as spec]
    [hyperfiddle.spec.datomic :as spec-datomic])
  #?(:clj (:import
            (datascript.parser FindRel FindColl FindTuple FindScalar Variable Aggregate Pull)
            [clojure.lang IHashEq ILookup IPersistentSet IPersistentCollection])))

(defn tempid? [id]
  (string? id))

(defn identity-segment? [schema attr-spec]                  ; need schema for :identity
  ; Not necessarily a keyword
  ; But accepts pull-shapes, so doesn't have to be the full attr spec
  (contains? #{:db/id :db/ident} attr-spec))

(defn smart-lookup-ref-no-tempids "see hypercrud.browser.context/smart-entity-identifier"
  [{:keys [:db/id :db/ident] :as v}]
  (let [identity-lookup nil]
    (or #_(if (underlying-tempid ctx id) id)                ; the lookups are no good yet, must use the dbid (not the tempid, actions/with will handle that reversal)
      ident
      identity-lookup
      id
      (if-not (map? v) v)                                   ; id-scalar
      nil                                                   ; This is an entity but you didn't pull any identity - error?
      )))

(def -parser-types {FindRel :hf/find-rel
                    FindColl :hf/find-coll
                    FindTuple :hf/find-tuple
                    FindScalar :hf/find-scalar
                    Pull :hf/pull
                    Variable :hf/variable
                    Aggregate :hf/aggregate})

(defn parser-type "element or qfind" [?p]                   ; not reactive
  (-parser-types (if ?p
                   (type ?p))))

(defprotocol SchemaIndexedNormalized
  ; Implement this interface in both peer and Hypercrud client
  (-repr-portable-hack [this])
  (attr [this a]))

(def ^:private xf-attr-in-set
  "A specific operation user in `attr?`, designed for performance."
  (map (fn [kv]
         (let [v (val kv)] ;; avoid destructuring, nth was too slow
           (if (boolean? v) (key kv) v)))))

(defn attr? [this a corcs]
  (let [haystack (into #{} xf-attr-in-set (attr this a))            ; component
        needles (contrib.data/xorxs corcs #{})]
    ; haystack must have all the needles
    (clojure.set/superset? haystack needles)))

(defn cardinality [this a]
  ; TODO needs to respect qfind types
  ; If datomic schema is added out of band, don't need specs here
  (get (attr this a) :db/cardinality)
  #_(or (get (attr this a) :db/cardinality)
      (let [spec (s/form a)]
        (if (seq? spec)
          (let [[x & xs] spec]
            (if (= x `s/coll-of)
              :db.cardinality/many))))
      :db.cardinality/one))
(comment
  (cardinality nil :user/teachers)
  )

; These can support unqualified keywords
(defn cardinality? [this a k] (= k (cardinality this a)))
(defn isComponent [this a] (get (attr this a) :db/isComponent)) ; follows Datomic naming case conventions
(defn valueType [this a] (get (attr this a) :db/valueType))
(defn valueType? [this a k] (= k (valueType this a)))
(defn ref? [this k] (valueType? this k :db.type/ref))
(defn unique [this a] (get (attr this a) :db/unique))
(defn unique? [this a k] (= k (unique this a)))
(defn identity? [this a] (unique? this a :db.unique/identity))

(defn attr-unreverse [a]
  {:pre [(-> (name a) (subs 0 1) (= "_"))]}
  (keyword (namespace a) (-> (name a) (subs 1))))

(defn make-reverse-attr [schema a]
  ; unique scalars can't be pulled in reverse
  ; unique ref doesn't imply that _no other attr_ points to the entityvalue
  ; isComponent implies a 1-1 relationship, so the reverse of an isComponent attr will be cardinality-one
  {:db/ident a
   :db/valueType :db.type/ref
   :db/cardinality (if (isComponent schema (attr-unreverse a))
                     :db.cardinality/one
                     :db.cardinality/many)})

; :db/id is currently addressable (e.g. user wants to render that column)
(def dbid {:db/ident :db/id
           :db/cardinality :db.cardinality/one
           :db/valueType :db.type/long
           #_#_:db/unique :db.unique/identity})

(defn- attr-spec [a]
  (some-> (s/get-spec a)
          (spec/parse)
          (spec-datomic/from-spec)))

(defn- attr* [this a]
  (when (and a (qualified-keyword? a))
    (let [is-reverse-nav (-> (name a) (subs 0 1) (= "_"))]
      (cond
        (= a :db/id) dbid
        is-reverse-nav (make-reverse-attr this a)
        :else
        (if-let [attr (a (.-schema-by-attr #?(:clj this, :cljs ^js this)))]
          (-> attr
              (contrib.data/update-existing :db/valueType smart-lookup-ref-no-tempids)
              (contrib.data/update-existing :db/cardinality smart-lookup-ref-no-tempids)
              (contrib.data/update-existing :db/isComponent smart-lookup-ref-no-tempids)
              (contrib.data/update-existing :db/unique smart-lookup-ref-no-tempids))
          (attr-spec a))))))

(deftype Schema [schema-by-attr hash]
  SchemaIndexedNormalized
  (-repr-portable-hack [this] (str "#schema " (pr-str (.-schema-by-attr this))))
  (attr [this a]
    (attr* this a))

  #?@(:clj
      [Object (equals [o other] (and (instance? Schema other) (= (.-schema-by-attr o) (.-schema-by-attr other))))
       IHashEq (hasheq [o] (.-hash o))
       ILookup
       (valAt [this k] (get (.-schema-by-attr this) k))
       (valAt [this k not-found] (get (.-schema-by-attr this) k not-found))

       IPersistentSet
       ; More idiomatic than (contains? schema a) is (boolean (hf/attr ctx a))
       (contains [this k] (boolean (attr this k)) #_(boolean (get this k))) ; account for :db/id, reverse nav, etc
       IPersistentCollection                                ; IPersistentSet extends IPersistentCollection
       (count [this] (count (.-schema-by-attr this)))       ; does not include virtual attrs like :db/id
       ;(cons [this o] (throw (UnsupportedOperationException.)))
       ;(empty [this] (throw (UnsupportedOperationException.)))
       (equiv [this o] (= (.-hash this) (.-hash o)))]
      :cljs
      [IEquiv (-equiv [o other] (and (instance? Schema other) (= (.-hash o) (.-hash other))))
       IHash (-hash [o] (.-hash o))
       IPrintWithWriter (-pr-writer [o writer _] (-write writer (-repr-portable-hack o)))
       ILookup
       (-lookup [o k] (get (.-schema-by-attr o) k))
       (-lookup [o k not-found] (get (.-schema-by-attr o) k not-found))]))

;; Suppress a cljs compiler warning `:redef-in-file`
#?(:cljs (ns-unmap 'contrib.datomic '->Schema))
(defn ->Schema
  "@suppress {duplicate}"
  [schema-by-attr]
  (Schema. schema-by-attr (hash schema-by-attr)))

(comment
  (def s (Schema. {:user/a {:db/ident :user/a :db/valueType :db.type/string}}))
  (get s :user/a)
  (valueType s :user/a)
  (contains? s :user/a)
  (contains? s :user/b)
  (empty s)
  (count s)
  (isa? s clojure.lang.Associative)                         ; false
  )

; this has to be a hack, when ever are we asking questions about a schema that does not exist
(extend-protocol SchemaIndexedNormalized
  nil
  (-repr-portable-hack [this] (str "#schema " (pr-str nil)))
  (attr [this a] nil))

#?(:clj (defmethod print-method Schema [o ^java.io.Writer w] (.write w (-repr-portable-hack o))))
#?(:clj (defmethod print-dup Schema [o w] (print-method o w)))

(defn indexed-schema [schema-pulledtree] (->Schema (contrib.data/group-by-unique :db/ident schema-pulledtree)))

(declare pull-shape)

(defn- attr-spec->shape "Take the pull and get rid of everything, even splat, just attrs requested.
  Does not validate and does not look at schema, do that later."
  [schema a]
  ; todo :as
  (cond
    ;(and (keyword? a) (attr? schema a :db.type/ref)) {a [:db/id]}
    (keyword? a) a
    (#{'* "*"} a) nil
    (map? a) (reduce-kv (fn [m k v]
                          (when-not (number? v)             ; bail on recursive specs https://github.com/hyperfiddle/hyperfiddle/issues/363
                            (assoc m (attr-spec->shape schema k)
                                     (pull-shape schema v))))
                        {} a)
    (and (sequential? a) (keyword? (first a))) (first a)
    (and (sequential? a) (#{'limit "limit" 'default "default"} (first a))) (second a)
    :else a))

(defn pull-shape "Get rid of the pull syntax, leaving just the core pull shape.
Shape is normalized to match the shape of the Datomic result, e.g. [:user/a-ref] becomes {:user/a-ref [:db/id]}"
  [schema pull-pattern]
  (assert (sequential? pull-pattern) (pr-str pull-pattern))
  (into [] (comp (map (partial attr-spec->shape schema))
                 (map (fn [a]
                        (if (and (keyword? a)
                                 (attr? schema a :db.type/ref))
                          {a [:db/id]}
                          a)))
                 (remove nil?))
        pull-pattern))

(defn pull-union [& vs]
  ; they have to be the same data-shape (schema), which if this is a valid pull, they are
  (cond
    (sequential? (first vs))
    (let [pull-pattern (apply concat vs)
          [as bs] (group-by-pred map? pull-pattern)]        ; this loses order by rewriting the pull canonical
      (vec
        (concat
          (distinct bs)
          (if-let [as (apply pull-union as)]                ; don't concat [nil]
            [as]))))

    (map? (first vs))
    (apply merge-with pull-union vs)))

(defn- scalar? [value]
  (not (map? value)))

(defn tree-derivative "Derive a pull-shape which describes a pulled-tree"
  [schema pulled-tree]
  {:pre [schema]}
  ; nil pulled-tree is legal, see https://github.com/hyperfiddle/hyperfiddle/issues/298
  ; Can't normalize {:user/a-ref [:db/id]}, you have to work with what they asked for, our UI will deal with it.
  (->> pulled-tree
       (reduce-kv
        (fn [acc k v]
          (conj acc (cond
                      (= :db/id k) k
                      (ref? schema k)
                      (condp = (cardinality schema k)
                        ;; what if not found? What if wonky shape that
                        ;; doesn't fit Datomic shapes? what if
                        ;; cardinality/relations? Spec, polymorphism w/o
                        ;; schema. Have the v
                        :db.cardinality/one  (if (scalar? v) k {k (tree-derivative schema v)})
                        :db.cardinality/many (if (every? scalar? v)
                                               k
                                               {k (apply pull-union (map (partial tree-derivative schema) v))}))
                      :else        k)))
        [])))

(defn pull-enclosure "Union the requested pull-pattern-shape with the actual result shape"
  [schema shape coll]
  {:pre [schema]}
  ; Only do this if the pull contains splat
  (apply pull-union shape (map (partial tree-derivative schema) coll)))

(defn pull-traverse "Manufacture superset of possible link paths
  Collapses {:reg/gender [:db/ident :db/id]} into :reg/gender"
  ([schema pull-shape] (pull-traverse schema pull-shape (constantly true) []))
  ([schema pull-shape pred] (pull-traverse schema pull-shape pred []))
  ([schema pull-shape pred path]                            ; private, internal path accumulator, like a zipper (loop recur would hide this)
   (->> pull-shape
        (mapcat (fn [attr-spec]
                  (cond
                    #_#_(identity-segment? schema attr-spec) ; todo needs schema
                        [[]]                                ; blank path means the element

                    (keyword? attr-spec)                    ; scalars, never refs
                    (if (pred attr-spec)
                      [(conj (vec path) attr-spec)])

                    (map? attr-spec)
                    (->> attr-spec
                         (mapcat (fn [[ref-attr children]]  ; always refs
                                   (if (pred ref-attr)
                                     (let [path (conj (vec path) ref-attr)]
                                       (concat [path]
                                               ; Collapse pulled identity into parent from link's perspective
                                               ; :unique :identity should too todo
                                               (pull-traverse
                                                 schema
                                                 children #_(remove #(identity-segment? schema %) children) ; Address entities by :ref
                                                 pred
                                                 path))))))
                         distinct))))
        distinct)))                                         ; why distinct? This breaks tail recursion

(defn dimension [schema pullshape pullpath]
  {:pre [schema]}
  (count (filter #(cardinality? schema % :db.cardinality/many))))

(defn ref-one?
  ([schema] (fn [a] (ref-one? schema a)))                   ; curried?
  ([schema a]
   {:pre [#_(satisfies? SchemaIndexedNormalized schema)]}
   (and (valueType? schema a :db.type/ref)
        (cardinality? schema a :db.cardinality/one))))

(defn ref-many? [schema a]
  (and (ref? schema a)
       (cardinality? schema a :db.cardinality/many)))

(defn scalar-one? [schema a]
  (and (not (ref? schema a))
       (cardinality? schema a :db.cardinality/one)))

(defn scalar-many? [schema a]
  (and (not (ref? schema a))
       (cardinality? schema a :db.cardinality/many)))

(defn one? [schema a]
  (cardinality? schema a :db.cardinality/one))

(defn many? [schema a]
  (cardinality? schema a :db.cardinality/many))

(defn pullshape-get [pullshape a]                           ; arg order is like 'get
  (-> pullshape
      (->> (filter map?))
      (->> (apply merge))
      (get a)))

(defn pullshape-get-in [pullshape as]
  (reduce pullshape-get pullshape as))

(defn element-spread [schema {{pull-pattern :value} :pattern :as e} collection]
  ; derivative oriented, ignores spread
  {:pre [schema]}
  (condp = (type e)
    Pull (pull-traverse schema (pull-enclosure schema (pull-shape schema pull-pattern) collection))
    Variable [[]]
    Aggregate [[]]))

(defn normalize-result [qfind result]
  ; nil should be treated as empty
  (when result                                              ; unclear if nil result should have been a server error https://github.com/hyperfiddle/hyperfiddle/issues/584
    (condp = (type qfind)
      FindRel result
      FindColl (mapv vector result)
      FindTuple (vector result)
      FindScalar [[result]])))

(defn spread-elements [f qfind result]
  (mapcat f                                                 ; (map (partial console-link source)) % has the source
          (range)                                           ; find-element index, for source reversing
          (datascript.parser/find-elements qfind)
          (contrib.data/pad nil (contrib.data/transpose (normalize-result qfind result)))))

(defn pull-level "Get next level e.g. for driving UI forms."
  [pullshape]                                               ; see downtree-pullpaths
  {:pre [(or (sequential? pullshape)
             (nil? pullshape))]}
  (->> pullshape
       (mapcat (fn [attr-spec]
                 (cond
                   (keyword? attr-spec) [attr-spec]
                   (map? attr-spec) (keys attr-spec))))     ; Could verify :ref against schema here
       #_(remove (partial = :db/id))))


(defn pull-seq [pull]
  (tree-seq coll? seq pull))

(defn pp-recursion? [v]
  #_[:person/parents 3]
  (and (vector? v)
       (let [[a b] v]
         (keyword? a)
         (number? b))))

(defn result-enclosure! "
  no data is not a well-formed result - probably invalid query, but it's less confusing to users
  if the UI still works in this case, since tweaking a formshape does not require the form be populated"
  [schemas ?qfind ?data]
  #_{:pre [schemas ?qfind]
     :post [vector?]}                                       ; associative by index
  (if ?qfind
    (let [?data (normalize-result ?qfind ?data)]            ; nil data can mean no query or invalid query, we can still draw forms
      (->> (datascript.parser/find-elements ?qfind)
           (map-indexed (fn [i element]
                          (condp = (type element)
                            Variable nil
                            Aggregate nil
                            Pull (let [{{db :symbol} :source {pull-pattern :value} :pattern} element
                                       dbname (str db)
                                       schema (some-> (get schemas dbname) deref)
                                       shape (pull-shape schema pull-pattern)
                                       no-splat (->> (pull-seq pull-pattern) (filter (partial = '*)) count (= 0))
                                       no-recursion (->> (pull-seq pull-pattern) (filter pp-recursion?) count (= 0))
                                       enclosure #_(delay)  ; Delay this to repro https://github.com/hyperfiddle/hyperfiddle/issues/892
                                       (let [coll (mapv #(get % i) ?data)]
                                         (pull-enclosure schema shape coll))]
                                   (if (and no-splat no-recursion)
                                     shape                  ; fast path, and doesn't lose pull order
                                     enclosure              ; loses pull order
                                     ; Do not inline enclosure, it causes reaction bugs
                                     )))))
           vec))

    ; What shape do we have? {} [{}] [[{}]] <something else>
    ; Get to the pulled trees, call pull-enclosure
    ; cardinaltiy of non-modeled attrs?
    #_(pull-enclosure (get schemas "$") nil ?data)
    nil))

(defn spread-elements! [f schemas qfind data]
  (spread-elements (fn [ix e coll-normalized]
                     (let [dbname (str (get-in e [:source :symbol]))
                           ; fmap the exception instead of throw?
                           schema (some-> (get schemas dbname) deref)]
                       (f schema e coll-normalized)))
                   qfind data))

(defn qfind-collapse-findrel-1 "Collapse FindRel-1 to FindColl, which simplifies the context business rules.
  TODO: also collapse FindRel-N where all elements are aggregates except one?"
  [qfind]
  (let [[e :as es] (datascript.parser/find-elements qfind)]
    (if (= 1 (count es))
      (condp = (type qfind)
        FindRel (datascript.parser/->FindColl e)
        FindTuple (datascript.parser/->FindScalar e)
        FindColl qfind
        FindScalar qfind)
      qfind)))

(defn find-identity-attr "Search an entity map for a :db.unique/identity attribute, or nil if not found.
  Does not traverse :db.type/ref nor :db/isComponent."
  [schema e-map]
  {:pre [schema]}
  (->> (keys e-map)
       (map (juxt identity #(contrib.datomic/unique? schema % :db.unique/identity)))
       (filter second)
       (some first)))

(defn validate-element [schema element _]
  (case (parser-type element)
    :hf/variable []
    :hf/aggregate []
    :hf/pull (let [{{pull-pattern :value} :pattern} element]
               (->> (pull-traverse schema (pull-shape schema pull-pattern))
                    (sequence (comp (remove empty?)
                                    (map last)
                                    (filter #(nil? (some-> schema (attr %)))))) ; dont crash; reject good ones
                    #_empty?)
               #_(tree-seq map?
                           (fn [m]
                             (concat (keys m)
                                     (apply concat (vals m))))
                           (pull-shape pull-pattern))
               #_(->> (pull-traverse (pull-shape pull-pattern))
                      (map last)
                      (mapcat identity))
               ; collect the errors
               )))

(defn validate-qfind-attrs! "Validate the pull attributes against schema. Bug: doesn't see Datomic aliases."
  [schemas qfind]
  (if qfind
    (spread-elements! validate-element schemas qfind nil)))

(defn validate-qfind-attrs+ "Validate the pull attributes against schema. Bug: doesn't see Datomic aliases."
  [schemas qfind]
  (>>= (try-either (validate-qfind-attrs! schemas qfind))
       (fn [invalid-attrs]
         (if (seq invalid-attrs)
           (either/left (ex-info "Invalid attributes" {:attrs invalid-attrs}))
           (either/right nil)))))
