(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require
    [cats.monad.either :as either]
    [clojure.spec.alpha :as s]
    [clojure.pprint]
    [contrib.css :refer [css]]
    [contrib.data :refer [assoc-if]]
    [contrib.ednish]
    [contrib.hfrecom :refer [anchor-tabs]]
    [contrib.rfc3986]
    [contrib.reactive :as r]
    [hypercrud.browser.context :as context]
    [hyperfiddle.api :as hf]
    [hyperfiddle.domain]
    [hyperfiddle.fiddle :as fiddle]
    [hyperfiddle.ide.domain :as ide-domain]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui :as ui :refer [anchor field value hyper-control link table]]
    [hyperfiddle.ui.controls :as controls]))


(defn with-fiddle-default [props fiddle-val attr]
  (if-some [f (get fiddle/fiddle-defaults attr)]
    (assoc props :default-value (f fiddle-val))
    props))

(defn schema-links [ctx]
  (->> (hf/domain (:runtime ctx)) ::ide-domain/user-dbname->ide keys sort
       (map (fn [user-dbname]
              (let [props {:route {::route/fiddle (keyword "hyperfiddle.ide.schema" user-dbname)}
                           #_#_:target "_blank"}]
                ^{:key user-dbname}
                [anchor ctx props (hyperfiddle.domain/dbname-label user-dbname)])))
       (doall)))

(defn query-composite-stable [ctx-top val ctx-fiddle-type props]
  ; The divs are for styling
  [:div (-> (select-keys props [:class])
            (assoc-in [:style :flex] "0 0 auto"))
   [hyper-control val ctx-fiddle-type (with-fiddle-default props val :fiddle/type)]
   [:div.pull-db
    (if (= val :entity)
      (let [ctx (context/focus ctx-top [:fiddle/pull-database])]
        [hyper-control (context/v ctx) ctx (with-fiddle-default {:class "pull-database"} val :fiddle/pull-database)]))]
   [:span.schema "schema: " (schema-links ctx-fiddle-type)]])

(defmethod hf/render #{:fiddle/type
                       :hyperfiddle/ide}
  [ctx props]
  (let [props (assoc props
                :class "qe"
                :options [{:value :query :label "query"}
                          {:value :entity :label "pull"}
                          {:value :eval :label "eval"}
                          {:value :blank :label "static"}])]
    [hyperfiddle.ui.controls/radio-group (hf/data ctx) ctx props]))

(defmethod hf/render #{:link/fiddle
                       :hyperfiddle/ide}
  [ctx props]
  [:<>
   [controls/ref
    (context/data ctx) ctx
    {:options :hyperfiddle.ide/fiddle-options
     :hf/where '[[?link :fiddle/ident ?ident]
                 [(clojure.string/includes? ?ident %)]]
     :option-label (r/comp pr-str :fiddle/ident)}]
   [link :hyperfiddle.ide/new-fiddle ctx "new"]])

(defmethod hf/render #{:fiddle/markdown
                       :hyperfiddle/ide}
  [ctx props]
  [hyperfiddle.ui.controls/markdown-editor
   (context/data ctx) ctx props])

(defmethod hf/tx ::new-fiddle [ctx [e a v] props]
  ; This is the right default for most links that traverse the graph
  ; For options, sometimes it is not. They will have to fix it in that case.
  ; The UX is more smooth this way, especially to do the right thing for beginners in tutorial.
  [[:db/add e a v]
   [:db/add v :fiddle/type :entity]])

(defn link-control [val ctx props]
  (let [props (assoc props :default-value (get (::record ctx) (context/a ctx)))]
    (hyper-control val ctx props)))

(defmethod hf/render #{:hyperfiddle/ide :fiddle/links} [ctx props]
  ; Shouldn't be necessary! Use default renderer with multimethod extensions and modified ctx.
  [table
   (fn [ctx]
     (let [ctx (assoc-if ctx ::record (if-not (:hypercrud.browser/head-sentinel ctx)
                                        ; todo shouldn't this be a meta-ctx? all these arguments look extremely suspect
                                        (-> (fiddle/auto-link+ (runtime/get-schemas (:runtime ctx) (:partition-id ctx))
                                                               (:qin @(:hypercrud.browser/qparsed ctx))
                                                               (context/data ctx))
                                            ; just throw, unlikely we can ever get this far if there was an issue
                                            (either/branch (fn [e] (throw e)) identity))))]
       [(field [:link/path] ctx link-control)
        (field [:link/fiddle] ctx)
        (field [:link/class] ctx link-control)
        (field [:link/tx-fn] ctx link-control)
        (when (exists? js/show_formulas) (field [:link/formula] ctx))
        (field [:db/id] ctx)]))
   ctx props])

(defmethod hf/render #{:fiddle/eval} [ctx props] [hyperfiddle.ui.controls/code (hf/data ctx) ctx props])
(defmethod hf/render #{:fiddle/pull} [ctx props] [hyperfiddle.ui.controls/code (hf/data ctx) ctx props])
(defmethod hf/render #{:fiddle/query} [ctx props] [hyperfiddle.ui.controls/code (hf/data ctx) ctx props])
(defmethod hf/render #{:fiddle/css} [ctx props] [hyperfiddle.ui.controls/css (hf/data ctx) ctx props])
(defmethod hf/render #{:fiddle/renderer} [ctx props] [hyperfiddle.ui.controls/code (hf/data ctx) ctx props])
(defmethod hf/render #{:fiddle/cljs-ns} [ctx props] [hyperfiddle.ui.controls/code (hf/data ctx) ctx props])

(def tabs
  {:hf/query
   (fn [val ctx props]                                      ; val is the toplevel val
     [:<>
      (value [:fiddle/type] ctx (r/partial query-composite-stable ctx) props)
      (case (or (:fiddle/type val)
                ((:fiddle/type fiddle/fiddle-defaults)
                 val))
        :entity ^{:key "pull"} [value [:fiddle/pull] ctx hyper-control (with-fiddle-default props val :fiddle/pull)]
        :query
        ^{:key "query"}
        [:<>
         [:div.query.cm-wrap
          [value [:fiddle/query] ctx hyper-control (with-fiddle-default props val :fiddle/query)]]

         #_[:div.query-needle-label
            (let [ctx (context/focus ctx [:fiddle/query-needle])
                  props (-> props
                            (dissoc :label-fn)
                            (update :class css (ui/semantic-css ctx)))]
              [ui/hyper-label nil ctx props])]

         #_[:div.query-needle.cm-wrap
            [value [:fiddle/query-needle] ctx hyper-control (with-fiddle-default props val :fiddle/query-needle)]]]
        :eval ^{:key "eval"} [value [:fiddle/eval] ctx hyper-control (with-fiddle-default props val :fiddle/eval)]
        :blank nil)])

   :hf/links
   (fn [val ctx props]
     (value [:fiddle/links] ctx nil props))

   :hf/cljs
   (fn [val ctx props]
     (value [:fiddle/cljs-ns] ctx nil (with-fiddle-default props val :fiddle/cljs-ns)))

   :hf/view
   (fn [val ctx props]
     (value [:fiddle/renderer] ctx nil (with-fiddle-default props val :fiddle/renderer)))

   :hf/markdown
   (fn [val ctx props]
     (value [:fiddle/markdown] ctx nil (with-fiddle-default props val :fiddle/markdown)))

   :hf/css
   (fn [val ctx props]
     (value [:fiddle/css] ctx nil (with-fiddle-default props val :fiddle/css)))

   :hf/fiddle
   (fn [val ctx props]
     [:<>
      (field [:fiddle/ident] ctx)
      (field [:fiddle/uuid] ctx)
      (field [:fiddle/hydrate-result-as-fiddle] ctx)
      [:div.p "Additional attributes"]
      (doall
        (for [[k _] (hypercrud.browser.context/spread-attributes ctx)
              :when (and (not= :db/id k)
                         (not= "fiddle" (namespace k)))]
          (field [k] ctx)))
      #_(field [:fiddle/ident] ctx (fn [val ctx props]
                                     [:div (link #{:fiddle/ident :hf/remove} ctx "Remove fiddle" {:class "btn-outline-danger"})]))
      #_[:div.p "Spec debugging"]
      #_[:pre
         (with-out-str
           (clojure.pprint/print-table
             (->> (:hypercrud.browser/spec-explain ctx)
                  ::s/problems
                  (map #(update % :pred s/abbrev))
                  (map #(select-keys % [:in :pred])))))]
      #_[contrib.ui/code
         {:read-only true
          :value (with-out-str
                   (clojure.pprint/pprint
                     (hypercrud.browser.context/validation-hints-here ctx)))}]])
   :hf/edn
   (fn [v ctx props]
     [contrib.ui/code {:value (contrib.pprint/pprint-str v 50) :read-only true}])})

(defn fiddle-src-tabs [tab-state]
  [:<>
   [anchor-tabs
    :tabs (->> [:hf/query :hf/links :hf/markdown :hf/view :hf/cljs :hf/css :hf/fiddle :hf/edn]
               (map (fn [k]
                      {:id k
                       ; Fragments mess with routing, they throw away the file segment on click
                       :href "" #_(->> (pr-str k)
                                       contrib.ednish/encode-ednish
                                       contrib.rfc3986/encode-rfc3986-pchar
                                       (str js/document.location.pathname "#"))})))
    :id-fn :id
    :label-fn (comp name :id)
    :model tab-state
    :on-change (r/partial reset! tab-state)]
   "·"])

;; TODO: GG: this is the RHS
(defn ^:export fiddle-src-renderer [_ ctx props]
  (let [ctx (hypercrud.browser.context/browse-element ctx 0)
        val (hypercrud.browser.context/data ctx)
        tab-state (::tab-state props)]
    [:<>
     [:div (into {:key (str (:fiddle/ident val))} (select-keys props [:class]))
      [(get tabs @tab-state) val ctx {}]]
     (when (exists? js/hyperfiddle_show_ide_stage)
       [hyperfiddle.ide/ide-stage ctx])]))
