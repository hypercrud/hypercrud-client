(ns hyperfiddle.def
  (:require
    [taoensso.timbre :as timbre]
    [clojure.spec.alpha :as s]
    [contrib.expr :refer :all]
    [contrib.data :refer [qualify trim-str]]))


(declare def!) ; main definitions
(declare attr)
(declare fiddle)
(declare project)

(declare read-def) ; parsing
(declare read-schema)
(declare read-props)
(declare read-links)
(declare map-attr)
(declare def!)
(declare update-context!)
(declare annotate-source)
(declare get-source-for)

(defonce *defs (atom {}))
(defonce *file-ref (atom {}))

(defn get-schema [id] (get-in @*defs [:schema id]))
(defn get-fiddle [id] (get-in @*defs [:fiddle id]))
(defn get-a [type id] (get-in @*defs [type id]))


(defn def! [type ident form val]
  {:pre [(#{:schema :attr :fiddle :project} type)
         (qualified-keyword? ident)
         form
         val]}
  (update-context!)
  (swap! *defs
         assoc-in [type ident]
         (vary-meta val assoc :source (get-source-for form (@*defs @*file-ref))))
  nil
  )

(defmacro schema [& attrs]
  (doseq [[_ attr] (read-schema (apply merge attrs))]
    (def! :schema (:db/ident attr) &form attr)))

(defmacro attr [ident & attrs]
  (doseq [[key attr] (apply merge attrs)]
    (def! :attr key &form attr)))

(defmacro fiddle [ident & attrs]
  (def! :fiddle ident &form
        (read-def :fiddle ident attrs)))

(defmacro project [ident & attrs]
  (def! :project ident &form
    (read-def :project ident attrs)))


(s/def ::schema
  (s/cat
    :type (s/? keyword?)
    :mod (s/* #{:unique :many})
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
      :fiddle (read-vec ::expr)
      :& (s/* any?))))

(s/def ::links
  (s/map-of
    any?
    (read-alt (s/& ::link-expr (s/conformer vector))
      (s/+ (s/spec ::link-expr)))))

(defn update-context! []
  (when (not= @*file-ref *file*)
    (timbre/info "loading fiddle defs" *file*)
    (reset! *file-ref *file*)
    (swap! *defs assoc *file* (annotate-source (slurp (or (clojure.java.io/resource *file*) *file*)))))
  nil)

(defn annotate-source [input & [origin]]
  (let [r (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. input))]
    (when (:line origin) (.setLineNumber r (:line origin)))
    (take-while #(not= :. %)
      (repeatedly (fn [] (let [[val val-str] (read+string {:eof :. :read-cond :allow} r)]
                           (cond-> val (instance? clojure.lang.IObj val) (vary-meta assoc :source val-str))))))))

(defn get-source-for [form source]
  (if (and source (-> form meta :line))
    (some->
      (drop-while
        (fn [el] (and (-> el meta :line)
                      (< (-> el meta :line)
                         (-> form meta :line))))
        source)
      first)))

(defn read-schema [attrs]
  (reduce-kv
    (fn [schema attr desc]
      (assert (qualified-keyword? attr))
      (assoc
        schema attr
        (let [desc (read-spec ::schema desc)]
          (merge
            {:db/ident    attr
             :cardinality :db.cardinality/one}
            (when (:type desc)
              (let [[_ type mod] (re-matches #"(.+?)(\*?)" (name (:type desc)))]
                (merge {:valueType (qualify 'db.type type)}
                  (when (= mod "*") {:cardinality :db.cardinality/many}))))
            (when (-> desc :mod #{:many}) {:cardinality :db.cardinality/many})
            (when (-> desc :mod #{:unique}) {:unique :db.unique/identity})
            (select-keys desc [:doc])
            (when-let [[& {:as rest}] (:& desc)] rest)))))
    {}
    attrs))

(defn read-attrs [body]
  (->> (read-spec (s/* (s/cat :k keyword? :v (s/+ (comp not keyword?)))) body)
       (map (fn [{:keys [k v]}] [k v]))
       (into {})))

(defn read-def [type ident attrs]
  (s/assert qualified-keyword? ident)
  (let [attrs (read-spec ::def attrs)]
    (reduce
      (fn [x [k v]]
        ;(timbre/debug :! ident x (map-attr type k v))
        (merge x (map-attr type k v))) {}
      (merge
        {:ident ident}
        (case type :fiddle {:fiddle/source (symbol (.name *ns*))}
                   nil)
        (dissoc attrs :&)
        (let [attrs (read-attrs (:& attrs))]
          (merge
            attrs
            (when (:links attrs)
              {:links (read-links (one (:links attrs)))})))))))

(defn read-links [links]
  (let [links (read-spec ::links links)]
    (-> (seq links)
        (over (fn [[k v]] (over v #(do [k %]))))
        (#(apply concat %))
        (over (fn [[path link]]
                (merge {:path path}
                       (dissoc link :&)
                       (when-let [[& {:as rest}] (:& link)] rest)))))))

(defn map-str [v]
  (trim-str v))

(defn map-code [k v]
  (cond (or (vector? v) (list? v)) (clojure.string/join "\n" (map str v))
        (string? v) (map-str v)))

(defn map-expr [v]
  (cond (form? v)
        (cond (= (first v) `quote) (second v)
              (and (= (first v) `unquote)
                   (qualified-keyword? (second v))) (second v))
        ()
        v))

(defn link-expr [v]
  {:fiddle/ident (map-expr v)})

(defn requote-expr [x]
  ; ...
  )

(defn map-attr [type k v]
  (s/assert #{:project :fiddle :attr :link} type)
  (s/assert keyword? k)
  (s/assert (comp not nil?) v)

  (as->
    (case [type k]
      [:project :ident] :db/ident
      [:attr :ident] :attribute/ident
      [:fiddle :ident] :fiddle/ident

      [:fiddle :pull] (let [[db q] (->> v map-expr (split-with (some-fn string? seq?)))]
                        (cond->
                          {:fiddle/type :entity
                           :fiddle/pull (one q)}
                          (not (empty? db)) (merge {:fiddle/pull-database (one db)})))
      [:fiddle :query] {:fiddle/type  :query
                        :fiddle/query (-> v one map-expr)}
      [:fiddle :code] :fiddle/cljs-ns
      [:fiddle :links] {:fiddle/links
                        (mapv (fn [link]
                                (reduce-kv (fn [x k v] (merge x (map-attr :link k v))) {} link))
                          v)}
      [:fiddle :with] {:fiddle/with (one v)}
      [:link :fiddle] {:link/fiddle
                       (link-expr v)}
      [:link :class] {:link/class
                       (let [x (-> v map-expr)]
                         (cond (vector? x) x
                               (keyword? x) [x]
                               (set? x) (vec x)))}

      (cond
        (qualified-keyword? k) k
        (= type :schema) k
        () (qualify type k)))

    remap

    (->> (if (keyword? remap)
           {remap v}
           remap)
         (reduce-kv
           (let [k' k]
             (fn [x k v]
               (assoc x k
                      (cond (k' #{:code :cljs-ns :renderer :markdown :css}) (map-code k' v)
                            (k #{:query :pull :formula}) (map-code k v)
                            (string? v) (map-str v)
                            () v))))
           {}))))
