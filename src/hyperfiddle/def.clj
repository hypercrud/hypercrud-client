(ns hyperfiddle.def
  (:require
    [taoensso.timbre :as timbre]
    [clojure.spec.alpha :as s]
    [hyperfiddle.etc.expr :refer :all]
    [contrib.etc :refer [with-ns trim-str]]
    [hyperfiddle.etc.etc :refer :all]
    ;[hypercrud.browser.base :as H]
    ))


(defonce *defs (atom {}))
(defonce *file-ref (atom {}))

(defn get-schema [id] (get-in @*defs [:schema id]))
(defn get-fiddle [id] (get-in @*defs [:fiddle id]))
(defn get-a [type id] (get-in @*defs [type id]))

(declare ; main definitions
  def!
  attr
  fiddle
  project)

(declare ; parsing
  read-def
  read-schema
  read-props
  read-link
  map-attrs
  def!
  update-context!
  annotate-source
  get-var-source)


(defn def! [type ident form val]
  {:pre [(#{:schema :attr :fiddle :project} type)
         (qualified-keyword? ident)
         form
         val]}
  (update-context!)
  (swap! *defs
         assoc-in [type ident]
         val
    ;(vary-meta val assoc :source (get-var-source ident (@*defs @*file-ref)))
    )
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
  (map-spec
    reduce-result
    (s/cat
      :doc (s/? string?)
      :args (s/? (s/coll-of any?))
      :& (s/* any?))))

(s/def ::expr
  (some-fn keyword? seq?))

(s/def ::links
  (map-spec reduce-result (s/map-of any? (s/or :_ ::link-expr :* (s/coll-of ::link-expr)))))

(s/def ::link-expr
  (s/cat
    :class (s/? (some-fn keyword? set?))
    :fiddle (s/? ::expr)
    :& (s/* any?)))


(defn update-context! []
  (when (not= @*file-ref *file*)
    (reset! *file-ref *file*)
    (swap! *defs assoc *file* (annotate-source (slurp *file*))))
  nil)

(defn annotate-source [input & [origin]]
  (let [r (clojure.lang.LineNumberingPushbackReader. (java.io.StringReader. input))]
    (when (:line origin) (.setLineNumber r (:line origin)))
    (take-while #(not= :. %)
      (repeatedly (fn [] (let [[val val-str] (read+string r false :.)]
                           (cond-> val (instance? clojure.lang.IObj val) (vary-meta assoc :source val-str))))))))

(defn get-var-source [ident source]
  (timbre/debug 'get-var-source ident source)
  (assert (-> ident meta :line))
  (some->
    (drop-while
      (fn [el] (< (-> el meta :line)
                  (-> ident meta :line)))
      source)
    first))

(defn read-schema [attrs]
  (into {}
    (for [[attr desc] attrs]
      (do (assert (qualified-keyword? attr))
          (let [desc (read-spec ::schema desc)]
            [attr
             (into {:db/ident attr
                    :cardinality :db.cardinality/one}
               (concat
                 (when (:type desc)
                   (let [[_ type mod] (re-matches #"(.+?)(\*?)" (name (:type desc)))]
                     (merge {:valueType (with-ns 'db.type type)}
                       (when (= mod "*") {:cardinality :db.cardinality/many}))))
                 (when (:doc desc) {:doc (trim-str (:doc desc))})
                 (when (-> desc :mod #{:many}) {:cardinality :db.cardinality/many})
                 (when (-> desc :mod #{:unique}) {:unique :db.unique/identity})
                 (when-let [[& {:as rest}] (:& desc)] rest)))]
            )))))

(defn read-props [body]
  (->> (read-spec (s/* (s/cat :k keyword? :v (s/+ (comp not keyword?)))) body)
       (map (fn [{:keys [k v]}] [k v]))
       (into {})))

(defn read-def [type ident attrs]
  (s/assert qualified-keyword? ident)
  ;(timbre/debug :parsing attrs)
  (let [attrs (read-spec ::def attrs)]
    (map-attrs type
      (concat {:ident ident}
        (when (= type :fiddle) {:fiddle/type :blank})
        (when (:doc attrs) {:doc (:doc attrs)})
        (when (:args attrs) {:args (:args attrs)})
        (dissoc attrs :id :doc :&)
        (read-props (:& attrs))))))

(defn one [v]
  {:pre [(= 1 (count v))]}
  (first v))

(defn normalize-str [v]
  (trim-str v))

(defn normalize-code [k v]
  (cond (or (vector? v) (list? v)) (clojure.string/join "\n" (map str v))
        (string? v) (normalize-str v)))

(defn normalize-expr [v]
  (cond (and (seq? v) (= (first v) `quote)) (second v)
        () v))

(defn map-attr [type k v]
  (as->
    (case [type k]
      [:project :ident] :db/ident
      [:attr :ident] :attribute/ident
      [:fiddle :ident] :fiddle/ident
      [:fiddle :code] :fiddle/cljs-ns
      [:fiddle :pull] (let [[db q] (->> v normalize-expr (split-with (some-fn string? seq?)))]
                        (cond->
                          {:fiddle/type :entity
                           :fiddle/pull (one q)}
                          (not (empty? db)) (merge {:fiddle/pull-database (one db)})))
      [:fiddle :query] {:fiddle/type  :query
                        :fiddle/query (-> v normalize-expr one)}
      [:fiddle :links] {:fiddle/links (->> v one (read-spec ::links))}
      (cond
        (qualified-keyword? k) k
        (= type :schema) k
        () (with-ns type k)))
    remapped
    (if (keyword? remapped)
      {remapped v}
      remapped)
    (->> remapped
        (map (let [k' k] (fn [[k v]]
                           [k (cond (k' #{:code :cljs-ns :renderer}) (normalize-code k' v)
                                    (string? v) (normalize-str v)
                                    () v)])))
        (into {}))))

(defn map-attrs [type attrs]
  (apply merge
    (map (fn [[k v]] (map-attr type k v))
         attrs)))
