(ns hyperfiddle.fiddle
  #?(:cljs
     (:require-macros
       [backtick :refer [template]]))
  (:require
    #?(:clj [backtick :refer [template]])
    [cats.context]
    [cats.core :as cats]
    [cats.monad.either :as either :refer [right]]
    [clojure.spec.alpha :as s]
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [orf update-existing keywordize]]
    [contrib.reader]
    [contrib.string :refer [or-str]]
    [contrib.template :refer [load-resource]]
    [contrib.try$ :refer [try-either]]
    [contrib.do :refer [do-result from-result]]
    [cuerdas.core :as str]
    [datascript.parser]
    #_[hyperfiddle.api] ; tempid formulas
    [taoensso.timbre :as timbre]
    [hyperfiddle.api :as hf]))


(s/def :swing/quux string?)
(s/def :fiddle/ident keyword?)
(s/def :fiddle/uuid uuid?)
(s/def :fiddle/type #{:blank :entity :query :eval})
(s/def :fiddle/query (some-fn string? coll?))
#_(s/def :fiddle/query-needle string?)
(s/def :fiddle/pull (some-fn string? coll?))
(s/def :fiddle/pull-database string?)
(s/def :fiddle/links (s/coll-of (s/and (s/keys :req [:link/path]
                                               :opt [:link/class :link/fiddle :link/formula :link/tx-fn])
                                       #_(s/multi-spec fiddle-link :link/class))))
(s/def :fiddle/markdown string?)
(s/def :fiddle/css string?)
(s/def :fiddle/cljs-ns string?)
(s/def :fiddle/hydrate-result-as-fiddle boolean?)

(s/def :link/class (s/coll-of keyword?))                    ; hf/new is not allowed on FindScalar at the top (no parent)
(s/def :link/fiddle (comp not nil?))
(s/def :link/path keyword?)
(s/def :link/formula (some-fn string? coll?))
(s/def :link/tx-fn keyword?)

(s/def :hyperfiddle/owners (s/coll-of uuid?))

(defmulti fiddle-type :fiddle/type)
(defmethod fiddle-type :blank [_] (s/keys :req [:fiddle/ident]))
(defmethod fiddle-type :eval [_] (s/keys :req [:fiddle/ident]))
(defmethod fiddle-type :query [_]
  (s/keys :req [:fiddle/ident
                :fiddle/query]))
(defmethod fiddle-type :entity [_]
  (s/keys :req [:fiddle/ident
                :fiddle/pull
                :fiddle/pull-database]))

(s/def :hyperfiddle/fiddle (s/multi-spec fiddle-type :fiddle/type))

(declare fiddle-defaults)
(declare apply-defaults)

(defn kind [fiddle]
  (cond (:fiddle/apply fiddle) :eval
        (:fiddle/args fiddle) :fn
        () :static))

(defn infer-query-formula [query]
  (unwrap
    #(timbre/warn %)
    (->> (contrib.reader/memoized-read-string+ query)
         (cats/=<< #(try-either (datascript.parser/parse-query %)))
         (cats/fmap (fn [{:keys [qin]}]
                      (if (->> qin
                               (remove #(and (instance? datascript.parser.BindScalar %)
                                             (instance? datascript.parser.SrcVar (:variable %))))
                               seq)
                        ; todo (juxt first second)
                        "identity"
                        "(constantly nil)"))))))

(defn validate-link-a [[src a :as v]]
  ; reject 0, for instance
  (when (and (symbol? src)
             (keyword? a))
    v))

;; TODO: GG: hf-def links are read here
(defn read-a "Use :qin to infer the src if the link/a eschews it."
  [s qin]
  (let [[x :as xs] (->> (contrib.reader/memoized-read-edn-string+ (str "[" s "]"))
                        (unwrap #(timbre/error %)))]
    (validate-link-a
      (condp = (count xs)
        2 xs
        1 ['$ x]                                            ; TODO: Use :qin to choose the right color
        nil))))

(def link-defaults
  {:link/formula (fn [link]
                   (cond

                     (some #{:hf/new} (:link/class link)) "(constantly (hyperfiddle.api/tempid! ctx))"

                     (:link/fiddle link)                    ; If there is a fiddle-target, infer the expected :in shape
                     (case (get-in link [:link/fiddle :fiddle/type] ((:fiddle/type fiddle-defaults) (:link/fiddle link)))
                       :query "identity" #_(infer-query-formula (get-in link [:link/fiddle :fiddle/query] ((:fiddle/query fiddle-defaults) (:link/fiddle link))))
                       :entity "identity"
                       :eval "identity"

                       ; this is the case where the v is inferred but we don't actually want it, like a toplevel iframe for a FindScalar.
                       ; Update: can use :db/id for the dependent iframe, use :fiddleident for a standalone iframe ? or naked (no :link/attr)
                       :blank nil)

                     ; TODO: What if :txfn is combined with :target-fiddle :blank, or :target-fiddle :FindTuple?
                     :else-txfn "identity"))

   :link/tx-fn (fn [schemas qin link]
                 ; Auto parent-child management for eav ref contexts
                 (condp some (:link/class link)
                   #{:hf/new} (let [[src-db a] (read-a (contrib.string/blank->nil (:link/path link)) qin)
                                    ; Consider: ref, fiddle-ident, scalar (e.g. :streaker/date, :fiddle/ident, :db/ident)
                                    dbname (str src-db)
                                    schema (some-> (get schemas dbname) deref)]
                                (cond
                                  ; Need to know what context we in.
                                  ; Identity can be parent-child ref.
                                  (contrib.datomic/attr? schema a :db.unique/identity) ":zero" ; hack to draw as popover
                                  (contrib.datomic/attr? schema a :db.type/ref) ":db/add"
                                  :else ":zero"))           ; nil a, or qfind-level

                   #{:hf/remove} (let [[src-db a] (read-a (contrib.string/blank->nil (:link/path link)) qin)
                                       ; Consider: ref, fiddle-ident, scalar (e.g. :streaker/date, :fiddle/ident, :db/ident)
                                       dbname (str src-db)
                                       schema (some-> (get schemas dbname) deref)]
                                   (cond
                                     (contrib.datomic/attr? schema a :db.unique/identity) ":db/retractEntity"
                                     (contrib.datomic/isComponent schema a) ":db/retractEntity"
                                     (contrib.datomic/attr? schema a :db.type/ref) ":db/retract"
                                     :else ":db/retractEntity")) ; legacy compat, remove
                   nil))})

;; to be manually kept in sync with hf.ui/fiddle
(defmethod hf/render-fiddle :default [val ctx props]
  ;; Format this manually:
  ;; - Syntax quote will expand @ into `(clojure.core/deref ..)`
  ;; - pretty printers suck at clojure, even the slow one
  ;; embedded newline lets this pass the cursive clojure formatter
  (let [{:keys [:hypercrud.browser/fiddle]} ctx]
    #?(:cljs [:div.container-fluid props
              [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]
              [hyperfiddle.ui/result val ctx props]])))

(def fiddle-defaults
  {:fiddle/markdown (fn [fiddle] (str/fmt "### %s" (some-> fiddle :fiddle/ident str)))
   :fiddle/pull (constantly "[:db/id\n *]")
   :fiddle/pull-database (constantly "$")
   :fiddle/query (constantly (load-resource "fiddle-query-default.edn"))
   :fiddle/eval (constantly "(->> \n  (datomic.api/q\n   '[:in $ \n     :find [(pull ?e [:db/ident]) ...]     ; Final value must match FindColl pattern as here\n     :where [?e :db/ident]]\n   hyperfiddle.api/*$*)\n  (sort-by :db/ident)\n  (take 20))\n")
   :fiddle/type (constantly :blank)})                       ; Toggling default to :query degrades perf in ide

(defn auto-link+ [schemas qin link]
  (try-either                                               ; link/txfn could throw, todo wrap tighter
    (let [formula (or-str (:link/formula link) ((:link/formula link-defaults) link))
          tx-fn   (keywordize (or-str (:link/tx-fn link) ((:link/tx-fn link-defaults) schemas qin link)))]
      (-> (update-existing link :link/fiddle apply-defaults)
          (update-existing :link/path keywordize)
          (cond->
            formula (assoc :link/formula formula)
            tx-fn (assoc :link/tx-fn tx-fn))))))

(defn apply-defaults "Fiddle-level defaults but not links.
  Links need the qfind, but qfind needs the fiddle defaults.
  Don't depend on ctx, this function runs in many situations including in datalog"
  [fiddle]
  (as-> fiddle fiddle
    (update fiddle :fiddle/type (orf ((:fiddle/type fiddle-defaults) fiddle)))
    (case (:fiddle/type fiddle)
      :query (update fiddle :fiddle/query or-str ((:fiddle/query fiddle-defaults) fiddle))
      :entity (-> fiddle
                  (update :fiddle/pull-database or-str ((:fiddle/pull-database fiddle-defaults) fiddle))
                  (cond->
                    (empty? (:fiddle/pull fiddle)) (assoc :fiddle/pull ((:fiddle/pull fiddle-defaults) fiddle))))
      :eval (update fiddle :fiddle/eval or-str ((:fiddle/eval fiddle-defaults) fiddle))
      :blank fiddle)
    (update fiddle :fiddle/markdown or-str ((:fiddle/markdown fiddle-defaults) fiddle))))

(defn apply-fiddle-links-defaults+ "Link defaults require a parsed qfind, so has to be done separately later."
  [fiddle schemas qparsed]
  (let [link+s (map #(auto-link+ schemas (:qin qparsed) %) (:fiddle/links fiddle))
        links+ (binding [cats.context/*context* either/context] ; need an inferable ctx when nil links
                 (cats/sequence link+s))]
    (cats/fmap #(assoc fiddle :fiddle/links %) links+)))

(defn as-expr [val]
  (cond (string? val) (contrib.reader/memoized-read-edn-string+ val)
        () (either/right val)))

(defn parse-fiddle-query+ [{:keys [fiddle/type fiddle/query fiddle/shape fiddle/pull fiddle/pull-database]}]
  (case type
    :blank (right nil)
    :eval (do-result
           (datascript.parser/parse-query (if shape
                                            (from-result (as-expr shape))
                                            (template [:find [(pull $ ?e [*]) ...] :in $ :where [$ ?e]]))))
    :entity (do-result
              (let [pull (from-result (as-expr pull))]
                (let [source (symbol pull-database)
                      ;fake-q (template [:find (pull ~source ?e ~pull) . :in ~source :where [~source ?e]])
                      fake-q `[:find (~'pull ~source ~'?e ~pull) . :in ~source :where [~source ~'?e]]]
                  (datascript.parser/parse-query fake-q))))
    :query (do-result (datascript.parser/parse-query (from-result (as-expr query))))))
