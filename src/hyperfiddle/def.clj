(ns hyperfiddle.def
  (:require
    [taoensso.timbre :as timbre]
    [clojure.spec.alpha :as s]
    [contrib.expr :refer :all]
    [contrib.do :refer [do-result]]
    [contrib.data :refer [qualify trim-str for-kv]]
    [hyperfiddle.fiddle]
    [hyperfiddle.spec :as hf-spec]))


(declare def!) ; main definitions
(declare attr)
(declare fiddle)                                            ; hyperfiddle.api ?
(declare project)

(declare read-def) ; parsing
(declare read-schema)
(declare read-props)
(declare read-links)
(declare map-attrs)
(declare def!)
(declare update-context!)
(declare annotate-source)
(declare get-source-for)

(defonce *defs (atom {}))

(defmacro lookup [& ks] `'~(get-in @*defs ks))

(defn get-def [& ks] (get-in @*defs ks))

(defn get-schemas
  ([]
   (@*defs :schema))
  ([ns]
   (let [ns (str (name ns))]
     (for-kv (get-schemas) {}
       (fn [m k v]
         (if (= ns (namespace k))
           (assoc m k v)
           m))))))

(defn get-attrs []
  (into {} (map (fn [[k v]] {k (:renderer v)}) (@*defs :attribute))))

(defn get-fiddle [id] (get-in @*defs [:fiddle id]))

(defn def! [type ident form val]
  {:pre [(#{:schema :attribute :fiddle :project} type)
         (qualified-keyword? ident)
         form
         val]}
  (update-context!)
  (swap! *defs
         assoc-in [type ident]
         (vary-meta val assoc :source (get-source-for form)))
  nil)

(defmacro schema [& attrs]
  (doseq [[_ attr] (read-schema (apply merge attrs))]
    (def! :schema (:db/ident attr) &form attr)))

(defmacro attr [ident & attrs]
  (doseq [[key attr] (apply merge attrs)]
     (def! :attribute key &form (map-attrs :attribute attr))))

(defmacro fiddle [ident & attrs]
  (def! :fiddle ident &form
        (read-def :fiddle ident attrs)))

(defmacro project [ident & attrs]
  (def! :project ident &form
    (read-def :project ident attrs)))

(s/def ::schema
  (s/cat
    :type (s/? keyword?)
    :mod (s/* #{:identity :many :index :isComponent})
    :doc (s/? string?)
    :& (s/* any?)))

(s/def ::def
  (s/cat
    :doc (s/? string?)
    :args (s/? (s/coll-of any?))
    :& (s/* any?)))

(s/def ::expr
  (s/and seq?
    (comp symbol? first)))

(s/def ::link-expr
  (read-alt
    (s/cat :class #{:hf/new :hf/iframe :hf/edit}
      :fiddle ::expr
      :& (s/* any?))
    (s/cat :class #{:hf/remove}
      :& (s/* any?))
    (s/cat :class (s/? set?)
      :fiddle ::expr
      :& (s/* any?))))

(s/def ::link-key
  (s/or :attribute keyword?
        :db-attribute (s/cat :db #{'$} :attribute keyword?)))

(s/def ::links
  (s/map-of
    ::link-key
    (read-alt (s/& ::link-expr (s/conformer vector))
      (s/+ (s/spec ::link-expr)))))

(defn read-schema [attrs]
  (reduce-kv
    (fn [schema attr desc]
      (assert (qualified-keyword? attr) attr)
      (assoc
        schema attr
        (let [desc (read-spec ::schema desc)]
          (merge
            {:db/ident    attr
             :db/cardinality :db.cardinality/one}
            (when (:type desc)
              (let [[_ type mod] (re-matches #"(.+?)(\*?)" (name (:type desc)))]
                (merge
                  {:db/valueType (qualify 'db.type type)}
                  (when (= mod "*") {:db/cardinality :db.cardinality/many}))))
            (when (some #{:many} (:mod desc)) {:db/cardinality :db.cardinality/many})
            (when (some #{:identity} (:mod desc)) {:db/unique :db.unique/identity})
            (when (some #{:index} (:mod desc)) {:db/index true})
            (when (some #{:isComponent} (:mod desc)) {:db/isComponent true})
            (when (:doc desc) {:db/doc (:doc desc)})
            (when-let [[& {:as rest}] (:& desc)] rest)))))
    {}
    attrs))

(comment
  (read-spec ::schema [:string :identity])
  => {:type :string, :mod [:identity]}
  (read-spec ::schema [:string :index :many :isComponent :foo :bar])
  => {:type :string, :mod [:index :many :isComponent], :& [:foo :bar]}

  (read-schema {::a [:string "docstring"]})
  (read-schema {::b [:string "docstring" :foo true]})
  (read-schema {::c [:string :isComponent]})
  (read-schema {::d [:string "docstring" :identity :many]})
  (read-schema {::d [:string :identity :many "docstring"]})
  (read-schema {::e [:string* :identity :many :index]})

  (read-links {":dustingetz/email" [[::submission-detail]
                                    [:hf/new ::submission-detail :tx-fn ":user.hello-world/new-submission"]]
               ":user.hello-world/submission-master" [:hf/iframe ::genders]
               ":dustingetz/gender" [:hf/iframe ::shirt-sizes]})
  )

(defn read-attrs [body]
  (->> (read-spec (s/* (s/cat :k keyword? :v (s/+ (comp not keyword?)))) body)
       (map (fn [{:keys [k v]}] [k v]))
       (into {})))

(defn read-def [type ident attrs]
  (s/assert qualified-keyword? ident)
  (let [attrs (read-spec ::def attrs)]
    (map-attrs type
      {:ident ident}
      (when (= type :fiddle)
        {:fiddle/source (symbol (.name *ns*))})
      (dissoc attrs :&)
      (let [attrs (read-attrs (:& attrs))]
        (merge
          attrs
          (when (:links attrs)
            {:links (read-links (one (:links attrs)))}))))))

(defn read-links [links]
  (let [links (read-spec ::links links)]
    (-> (seq links)
        (over (fn [[k v]] (over v #(do [k %]))))
        (#(apply concat %))
        (over (fn [[path link]]
                (merge {:path path}
                       (dissoc link :&)
                       (when-let [[& {:as rest}] (:& link)] rest)))))))

(defn map-val [v]
  (cond (string? v) (trim-str v)
        () v))

(defn repr-val [v]
  (cond (seq? v)
        (str v)
        (vector? v)
        (clojure.string/join "\n" (map str v))
        (string? v) (trim-str v)
        () v))

(defn map-expr [v]
  (cond (form? v)
        (cond (= (first v) `quote) (second v)
              () v)
        () v))

(defn requote-expr [x]
  ; ...
  )

(defn map-attr [type k v]
  (s/assert #{:project :fiddle :attribute :link} type)
  (s/assert keyword? k)
  (s/assert (comp not nil?) v)

  (let
    [kv
     (case (qualify type k)
       :project/ident  :db/ident

       :fiddle/pull
       (let [[db q] (->> v map-expr (split-with (some-fn string? seq?)))]
         (cond->
           {:fiddle/type :entity
            :fiddle/pull (one q)}
           (not (empty? db)) (merge {:fiddle/pull-database (one db)})))

       :fiddle/query
       {:fiddle/type  :query
        :fiddle/query (-> v one map-expr)}

       :fiddle/eval
       {:fiddle/type :eval
        :fiddle/eval (-> v one map-expr)
        :fiddle/spec (when-let [spec (s/get-spec (ffirst v))]
                       (-> (hf-spec/parse spec)
                           (hf-spec/fiddle-spec)))}

       :fiddle/shape
       {:fiddle/shape (-> v one map-expr)}

       :fiddle/code :fiddle/cljs-ns ;; DEPRECATED

       :fiddle/links
       {:fiddle/links
        (->> v (mapv (fn [link]
                       (->
                         (reduce-kv (fn [x k v] (merge x (map-attr :link k v))) {} link)
                         ; hack for https://github.com/hyperfiddle/hyperfiddle/issues/1022
                         ; :db/id must be `long? if Datomic specs are in the spec registry
                         (as-> link (assoc link :db/id (-> link :link/fiddle :fiddle/ident hash)))))))}

       :fiddle/with {:fiddle/with (one v)}

       :link/fiddle
       {:link/fiddle
        (let [v (map-expr v)]
          (cond (form? v) (if (and (= (first v) `unquote)
                                   (qualified-keyword? (second v)))
                            {:fiddle/ident (second v)}
                            v)
                (keyword? v) {:fiddle/ident v}
                () v))}

       :link/class
       {:link/class
        (let [x (map-expr v)]
          (cond (vector? x) x
                (keyword? x) [x]
                (set? x) (vec x)))}

       (cond (qualified-keyword? k) {k v}
             (= type :schema) {k v}
             () {(qualify type k) v}))]

    (if (not (map? kv))
      {kv v} kv)))

(defn adjust-type
  "When a fiddle is of type :fiddle/eval we want to be able to infer the resultset
  schema from a static definition. Passing a :query parameter to a fiddle
  provides a way to infer a schema, but this doesn't make this fiddle a :query
  fiddle, so it should stay of type :eval."
  [fiddle]
  (if (contains? fiddle :fiddle/eval)
    (assoc fiddle :fiddle/type :eval)
    fiddle))

(defn map-attrs [type & attrs]
  (->>
    (apply merge attrs)
    (reduce-kv
      (fn [acc k v]
        (merge acc (map-attr type k v))) {})
    (reduce-kv
      (fn [acc k v]
        (merge acc
          {k (cond
               (and (= type :fiddle)
                    (= (hyperfiddle.fiddle/kind acc) :fn)) (map-val v)
               (-> k name keyword #{:query :pull :formula}) (map-expr v)
               (-> k name keyword #{:code :cljs-ns :renderer :markdown :css}) (repr-val v)
               () v)}))
      {})
    (adjust-type)))

; ---

(defonce *file-ref (atom {}))

(defn update-context! []
  (when (not= @*file-ref *file*)
    (timbre/info "loading fiddle defs" *file*)
    (reset! *file-ref *file*)
    (do-result
      (swap! *defs assoc *file* (annotate-source (slurp (or (clojure.java.io/resource *file*) *file*))))))
  nil)

(defn annotate-source [input & [origin]]
  (let [r (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. input))]
    (when (:line origin) (.setLineNumber r (:line origin)))
    (take-while #(not= :. %)
      (repeatedly (fn [] (let [[val val-str] (read+string {:eof :. :read-cond :allow} r)]
                           (cond-> val (instance? clojure.lang.IObj val) (vary-meta assoc :source val-str))))))))

(defn line-at [x]
  (some-> x meta :line))

(defn get-source-for [form]
  (let [source (@*defs @*file-ref)]
    (when (and source (line-at form))
      (some->
        (drop-while
          #(and (line-at %)
                (< (line-at %)
                   (line-at form)))
          source)
        first))))
