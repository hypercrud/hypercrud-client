(ns hyperfiddle.fiddle
  (:require
    [cats.core :as cats]
    [clojure.spec.alpha :as s]
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [update-existing]]
    [contrib.reader :as reader]
    [contrib.string :refer [or-str]]
    [contrib.template :as template]
    [contrib.try$ :refer [try-either]]
    [cuerdas.core :as str]
    [datascript.parser]
    #_[hyperfiddle.ui]
    [taoensso.timbre :as timbre]))


(defmulti fiddle-type :fiddle/type)

(s/def :hyperfiddle/ide                                     ; !! fiddle/ident overlap with attributes
  (s/and
    #_(s/multi-spec fiddle-type :fiddle/type)
    #_(s/or :ident (s/keys :req [:fiddle/ident])
            :uuid (s/keys :req [:fiddle/uuid]))
    (s/keys :opt [:fiddle/ident
                  :fiddle/uuid
                  :fiddle/type
                  :fiddle/links
                  :fiddle/markdown
                  :fiddle/renderer
                  :fiddle/css
                  :fiddle/cljs-ns
                  :fiddle/hydrate-result-as-fiddle
                  :hyperfiddle/owners])))

(defmulti fiddle-link :link/rel)

(s/def :fiddle/ident keyword?)
(s/def :fiddle/uuid uuid?)
(s/def :fiddle/type #{:blank :entity :query})
(s/def :fiddle/query string?)
(s/def :fiddle/pull string?)
(s/def :fiddle/pull-database string?)
(s/def :fiddle/links (s/coll-of (s/and (s/multi-spec fiddle-link :link/rel)
                                       (s/keys :req [:link/rel]
                                               :opt [:link/class
                                                     :link/path :link/fiddle :link/formula :link/tx-fn]))))
(s/def :fiddle/markdown string?)
(s/def :fiddle/renderer string?)
(s/def :fiddle/css string?)
(s/def :fiddle/cljs-ns string?)
(s/def :fiddle/hydrate-result-as-fiddle string?)

(s/def :link/rel #{:hf/self :hf/rel :hf/new :hf/remove :hf/affix :hf/detach :hf/iframe})
(s/def :link/class (s/coll-of keyword?))
;(s/def :link/fiddle (s/keys))
(s/def :link/path string?)
(s/def :link/formula string?)
(s/def :link/tx-fn string?)

(s/def :hyperfiddle/owners (s/coll-of uuid?))

(defmethod fiddle-type :blank [_] (s/keys))
(defmethod fiddle-type :query [_] (s/keys :opt [:fiddle/query]))
(defmethod fiddle-type :entity [_] (s/keys :opt [:fiddle/pull :fiddle/pull-database]))
(defmethod fiddle-type nil [_] (s/keys))

(defmethod fiddle-link :hf/self [_] (s/keys))
(defmethod fiddle-link :hf/rel [_] (s/keys))
(defmethod fiddle-link :hf/new [_] (s/keys))
(defmethod fiddle-link :hf/remove [_] (s/keys))
(defmethod fiddle-link :hf/affix [_] (s/keys))
(defmethod fiddle-link :hf/detach [_] (s/keys))
(defmethod fiddle-link :hf/iframe [_] (s/keys))

(declare fiddle-defaults)
(declare apply-defaults)

(defn infer-query-formula [query]
  (unwrap
    #(timbre/warn %)
    (->> (reader/memoized-read-string+ query)
         (cats/=<< #(try-either (datascript.parser/parse-query %)))
         (cats/fmap (fn [{:keys [qin]}]
                      (if (->> qin
                               (remove #(and (instance? datascript.parser.BindScalar %)
                                             (instance? datascript.parser.SrcVar (:variable %))))
                               seq)
                        "identity"
                        "(constantly nil)"))))))

(def link-defaults
  {:link/formula (fn [link]
                   (condp contains? (:link/rel link)
                     #{:hf/new} "(constantly (hyperfiddle.api/tempid-detached ctx))"
                     #{:hf/affix} "(partial hyperfiddle.api/tempid-child ctx)"
                     #{:hf/remove :hf/detach} "identity"
                     #{:hf/rel :hf/self :hf/iframe}
                     (case (get-in link [:link/fiddle :fiddle/type] ((:fiddle/type fiddle-defaults) (:link/fiddle link)))
                       :query (infer-query-formula (get-in link [:link/fiddle :fiddle/query] ((:fiddle/query fiddle-defaults) (:link/fiddle link))))
                       :entity "identity"
                       :blank nil)
                     nil))
   :link/tx-fn (fn [link]
                 (case (:link/rel link)
                   :hf/new ":zero"                            ; hack to draw as popover
                   :hf/remove ":db.fn/retractEntity"
                   :hf/affix ":db/add"
                   :hf/detach ":db/retract"
                   :hf/self nil
                   :hf/iframe nil
                   :hf/rel nil
                   nil))})

(def fiddle-defaults
  {:fiddle/markdown (fn [fiddle] (str/fmt "### %s" (some-> fiddle :fiddle/ident str)))
   :fiddle/pull (constantly "[:db/id *]")
   :fiddle/pull-database (constantly "$")
   :fiddle/query (constantly (template/load-resource "fiddle-query-default.edn"))
   :fiddle/renderer (fn [fiddle]
                      #?(:cljs (-> hyperfiddle.ui/fiddle meta :expr-str)
                         :clj  nil))
   :fiddle/type (constantly :blank)})

(defn auto-link [link]
  (let [link (cond-> link
                     (contains? #{:hf/rel :hf/self :hf/new :hf/iframe} (:link/rel link)) (update-existing :link/fiddle apply-defaults))]
    (-> link
        (update :link/formula or-str ((:link/formula link-defaults) link))
        (update :link/tx-fn or-str ((:link/tx-fn link-defaults) link)))))

(defn apply-defaults [fiddle]
  (as-> fiddle fiddle
        (update fiddle :fiddle/links (partial map auto-link))
        (update fiddle :fiddle/type #(or % ((:fiddle/type fiddle-defaults) fiddle)))
        (cond-> fiddle
                (= :query (:fiddle/type fiddle)) (update :fiddle/query or-str ((:fiddle/query fiddle-defaults) fiddle))
                (= :entity (:fiddle/type fiddle)) (-> (update :fiddle/pull or-str ((:fiddle/pull fiddle-defaults) fiddle))
                                                      (update :fiddle/pull-database or-str ((:fiddle/pull-database fiddle-defaults) fiddle))))
        (update fiddle :fiddle/markdown or-str ((:fiddle/markdown fiddle-defaults) fiddle))
        (update fiddle :fiddle/renderer or-str ((:fiddle/renderer fiddle-defaults) fiddle))))

(def browser-pull                                           ; synchronized with http://hyperfiddle.hyperfiddle.net/:hyperfiddle!ide/
  [:db/id
   :db/doc
   :fiddle/css
   :fiddle/ident
   :fiddle/uuid
   {:fiddle/links [:db/id
                   :link/class
                   {:link/fiddle [:db/id
                                  :fiddle/ident             ; routing
                                  :fiddle/query             ; validation
                                  :fiddle/type              ; validation
                                  ]}
                   :link/formula
                   :link/ident                              ; legacy
                   :link/path
                   :link/rel
                   :link/tx-fn]}
   :fiddle/markdown
   :fiddle/pull
   :fiddle/pull-database
   :fiddle/query
   :fiddle/cljs-ns
   :fiddle/renderer
   :fiddle/type
   :fiddle/hydrate-result-as-fiddle
   '*                                                       ; For hyperblog, so we can access :hyperblog.post/title etc from the fiddle renderer
   ])
