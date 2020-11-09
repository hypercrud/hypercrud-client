(ns hyperfiddle.def
  (:require
   [clojure.spec.alpha :as s]
   [contrib.data :as data]
   [contrib.expr :refer [read-alt]]
   [hyperfiddle.fiddle]
   [hyperfiddle.spec :as hf-spec]
   [taoensso.timbre :as timbre])
  #?(:cljs (:require-macros [hyperfiddle.def :refer [serve-ns! serve!]])))

;; Holds a mapping of {ƒ -> function value}
;; Used to circumvent a lack of vars in CLJS.
(defonce *fiddles (atom {}))

(defn get-fiddle [f]
  (some-> *fiddles deref (get f)))

(s/def ::expr (s/cat :ident qualified-symbol?))

(s/def ::expr-or-ident (read-alt qualified-symbol? ::expr))

(s/def ::link-expr
  (read-alt
   (s/cat :link/class #{:hf/new :hf/iframe :hf/edit}
          :link/fiddle ::expr-or-ident
          :link/& (s/* any?))
   (s/cat :link/class #{:hf/remove}
          :link/& (s/* any?))
   (s/cat :link/class (s/? set?)
          :link/fiddle ::expr-or-ident
          :link/& (s/* any?))))

(s/def ::links (s/map-of ::link-key
                         (s/or :link   ::link-expr
                               :links  (s/coll-of ::link-expr))))

(s/def ::link-key (s/or :attribute    (s/or :sym qualified-symbol? :kw qualified-keyword?)
                        :db-attribute (s/cat :db symbol? :attribute (s/or :sym qualified-symbol? :kw qualified-keyword?))))

(declare expand)

(defn parse-links [links]
  (s/assert ::links links)
  (->> (s/conform ::links links)
       (mapcat (fn [[k [type links]]]
                 (let [links (case type
                               :link  [links]
                               :links links)]
                   (->> links
                        (map (fn [link] (map expand link)))
                        (map #(apply merge %))
                        (map #(assoc % :link/path k))))))))

(defn expand [[k v]]
  (s/assert keyword? k)
  (when v
    (case k
      :fiddle/links {:fiddle/links (parse-links v)}
      :link/fiddle  {:link/fiddle {:fiddle/ident v}}
      :link/class   {:link/class (cond (vector? v)  v
                                       (keyword? v) [v]
                                       (set? v)     (vec v))}
      :link/&       (data/map-keys (partial data/qualify :link) (apply hash-map v))
      {k v})))

;;;;;;;;;
;; NEW ;;
;;;;;;;;;

(defn- fiddle-args [avar]
  (let [{:keys [:hyperfiddle.api/fiddle :doc]} (meta avar)]
    (when (or (true? fiddle) (map? fiddle))
      (cond-> {}
        (not-empty doc) (assoc :fiddle/doc doc)
        (map? fiddle)   (merge fiddle)))))

(defn- fiddle-fn? [avar]
  (boolean (fiddle-args avar)))

(defn- fiddle-name [avar]
  (let [{:keys [ns name]} (meta avar)]
    (symbol (str ns) (str name))))

(defn- multi-arity? [avar]
  (some-> avar meta :arglists count (> 1)))

(defn parse [avar]
  (let [ident                     (fiddle-name avar)
        {:keys [shape links doc]} (fiddle-args avar)
        spec                      (some-> (s/get-spec ident) (hf-spec/parse))]
    (-> {:fiddle/ident ident
         :fiddle/shape shape
         :fiddle/links links}
        (cond->
            spec            (assoc :fiddle/spec (hf-spec/fiddle-spec spec))
            (nil? shape)    (assoc :fiddle/shape (hf-spec/shape spec))
            (not-empty doc) (assoc :fiddle/doc doc))
        (->> (map expand)
             (remove nil?)
             (into {})))))

#?(:clj
   (defn- fiddles [ns]
     (->> (ns-publics (the-ns ns))
          (vals)
          (filter fiddle-fn?))))

#?(:clj
   (defn serve-fiddle! [avar]
     (when (multi-arity? avar)
       (timbre/warnf "Fiddle %s has multiple arities, which would produce ambiguous UIs. Hyperfiddle will only consider the longuest one." avar))
     (swap! *fiddles assoc (fiddle-name avar) (vary-meta (deref avar) merge (-> (meta avar) (update :ns str) (update :name str))))))

#?(:clj
   (defn- watch! [fiddle]
     (add-watch fiddle ::serve! (fn [_ _ _ _]
                                  (timbre/infof "Refreshing fiddle %s. Don't forget to write this file to disk to refresh CLJS side." fiddle)
                                  (serve-fiddle! fiddle)))
     fiddle))

(defmacro serve-ns!
  "Serve listed namespaces"
  [& nss]
  (let [fs (->> (mapcat fiddles nss)
                (map watch!)
                (map (juxt fiddle-name meta))
                (mapcat (fn [[name meta]] [(list 'quote name) `(vary-meta ~name merge '~(-> (update meta :ns str)
                                                                                            (update :name str)))])))]
    (when (seq fs)
      `(swap! *fiddles assoc ~@fs))))

#_(serve-ns! user.hello-world)

(defmacro serve! []
  (if (:ns &env)
    ;; cljs
    `(serve-ns! ~(:name (:ns &env)))
    ;; clj
    `(serve-ns! ~*ns*)))

(defn check-metas []
  (doseq [[k v] @*fiddles
          :when (qualified-symbol? k)]
    #?(:cljs (js/console.log (meta v)))))
