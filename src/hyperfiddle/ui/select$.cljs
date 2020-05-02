(ns hyperfiddle.ui.select$                                  ; Namespace clashes with var hyperfiddle.ui/select
  (:require
    [cats.core :as cats :refer [mlet return]]
    [cats.monad.either :as either :refer [left right]]
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [unqualify]]
    [contrib.eval]
    [contrib.reactive :as r]
    [contrib.reader]
    [contrib.string :refer [blank->nil]]
    [cuerdas.core :as str]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hyperfiddle.api :as hf]
    [hyperfiddle.data :as data]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.error :as ui-error]
    [hyperfiddle.ui.iframe :as iframe]
    [hyperfiddle.ui.util :refer [with-entity-change! writable-entity?]]
    [taoensso.timbre :as timbre]))

(defn ident->label [v]
  (cond
    ; A sensible default for userland whose idents usually share a long namespace.
    (instance? cljs.core/Keyword v) (name v)
    (and (vector? v) (= 1 (count v))) (str (first v))
    :else (str v)))

;(defn option-label-default' [row ctx]                                ; typechecks with keyword
;  ; spread-rows is external, f.
;  (->>
;    (for [[_ ctx] (hypercrud.browser.context/spread-fiddle ctx)
;          [_ ctx] (hypercrud.browser.context/spread-elements ctx)]
;      (let [[_ _ v] @(:hypercrud.browser/eav ctx)]
;        (condp = (type @(:hypercrud.browser/element ctx))
;          Variable [v]
;          Aggregate [v]
;          Pull (for [[_ ctx] (hypercrud.browser.context/spread-attributes ctx)]
;                 (let [[_ _ v] @(:hypercrud.browser/eav ctx)]
;                   (ident->label v))))))
;    (mapcat identity)
;    (remove nil?)
;    (interpose ", ")
;    (apply str)))

;(defn option-label-default [row ctx]
;  ; row is FindRel or FindCol
;  (or
;    (some-> (hf/row-key ctx row) ident->label)
;    (clojure.string/join " " (vals row))))

(defn wrap-change-for-select [hf-change!]
  (fn [e]
    (-> (some->> (.-target.value e)
                 blank->nil
                 contrib.reader/read-edn-string+
                 ; todo why handle this exception? just throw and call it a day
                 ; instead of terminating on error, user now transacts a retract
                 (unwrap #(timbre/warn %)))
        hf-change!)))

(declare options-value-bridge+)
(declare select-error-cmp)

(defn select-html [_ ctx props]                             ; element, etc
  (either/branch
    (options-value-bridge+ (::value-ctx props) props)
    (fn [err]
      [select-error-cmp err])
    (fn [[select-props option-props]]
      ; hack in the selected value if we don't have options hydrated?
      ; Can't, since we only have the #DbId hydrated, and it gets complicated with relaton vs entity etc
      ; This is possible if the value matches the row-key (qfind) of options query
      (let [is-no-options (empty? (hf/data ctx))
            select-props (-> select-props
                             (assoc :value (str (hf/v (::value-ctx props)))) ; serialize v to string for dom roundtrip
                             (update :on-change wrap-change-for-select)
                             (dissoc :disabled)             ; Use :read-only instead to allow click to expand options
                             (update :read-only #(or % (:disabled select-props) is-no-options))
                             (update :class #(str % (if (:disabled option-props) " disabled"))))]
        [:select.ui (select-keys select-props [:value :class :style :on-change :read-only :on-click]) ;(dissoc props :option-label)
         ; .ui is because options are an iframe and need the pink box
         (conj
           (->> (hf/data ctx)
                (mapv (juxt #((:option-value select-props) % ctx) ; is lookup ref good yet?
                            #((:option-label select-props) % ctx)))
                (sort-by second)
                (map (fn [[id label]]
                       [:option (assoc option-props :key (str id) :value (str id)) label])))
           [:option (assoc option-props :key :blank :value "") "--"])]))))


(defn select-error-cmp [msg]
  [:span msg])

(defn compute-disabled [ctx props]
  (or (boolean (:disabled props))
      (boolean (:read-only props))                          ; legacy
      (nil? (hf/e ctx))))

(defn options-value-bridge+ [anchor-ctx #_target-ctx props] ; no longer need target-ctx because it has to work without options hydrated
  ; if select is qfind-level, is value tupled according to options qfind?
  ; if select is pull-level, is options qfind untupled?
  ; We must compare both ctxs to decide this.
  (let [option-element (:option-element props 0)
        select-props {:option-element option-element
                      :option-value (let []
                                      (fn [val ctx]
                                        ; in the case of tupled options, the userland view props must
                                        ; indicate which element is the entity has an identity which matches the anchor eav.
                                        (condp some [(unqualify (contrib.datomic/parser-type @(:hypercrud.browser/qfind ctx)))]
                                          #{:find-coll :find-scalar} (hf/row-key ctx val)
                                          #{:find-rel :find-tuple} (get (hf/row-key ctx val) option-element))))
                      :option-label (fn [val]
                                      (let [option-label (contrib.eval/ensure-fn (:option-label props pr-str))]
                                        (option-label val)))}
        ; The pulled v is always the select value, options must align
        ; There is always an attribute here because all widgets are attribute-centric
        value (hf/v anchor-ctx)]
    (cond
      ; Select options renderer is an eav oriented control like all controls, thus the
      ; anchor is always a pull context, never a qfind context. To see this is true, consider that you can't
      ; pick a tuple then write it into a database. You can only write through entities.
      (hf/qfind-level? anchor-ctx)
      (left (str "No attribute in scope. eav: " (hf/eav anchor-ctx)))

      :else
      (let [select-props (merge {:value value               ; typeahead does not use this
                                 :on-change (with-entity-change! anchor-ctx)}
                                select-props
                                props)
            option-props {:disabled (compute-disabled anchor-ctx select-props)}]
        (right [select-props option-props])))))

(defn- options-values [options-ctx select-props]
  (condp some [(unqualify (contrib.datomic/parser-type (hf/qfind options-ctx)))]
    #{:find-coll :find-scalar} (hf/data options-ctx)
    #{:find-rel :find-tuple} (mapv #(get % (:option-element select-props 0))
                                   (hf/data options-ctx))))

(defn- truncated-options [n]
  ; todo this destroys table cells
  #_(when (= base/browser-query-limit n)
      [:div.alert.alert-warning (str/format "Warning: Options resultset has been truncated to %s records. Please add additional filters" base/browser-query-limit)]))

(def react-bootstrap-typeahead
  (cond
    (exists? js/ReactBootstrapTypeahead) js/ReactBootstrapTypeahead
    (exists? js/require) (set! js/ReactBootstrapTypeahead (or (js/require "react-bootstrap-typeahead")
                                                              (throw (js/Error. "require('react-bootstrap-typeahead') failed"))))
    :else (throw (js/Error. "js/react-bootstrap-typeahead is missing"))))

(defn typeahead-html [_ options-ctx props]                  ; element, etc
  (either/branch
    (options-value-bridge+ (::value-ctx props) props)
    (fn [err]
      [select-error-cmp err])
    (fn [[select-props option-props]]
      ; hack in the selected value if we don't have options hydrated?
      ; Can't, since we only have the #DbId hydrated, and it gets complicated with relaton vs entity etc
      ; This is possible if the value matches the row-key (qfind) of options query
      (let [is-no-options (empty? (hf/data options-ctx))
            disabled (when (:disabled select-props) true)
            select-props (-> select-props
                             (dissoc :disabled)             ; Use :read-only instead to allow click to expand options
                             (update :read-only #(or % (:disabled select-props) is-no-options))
                             (update :class #(str % (if (:disabled option-props) " disabled"))))

            ?v (hf/data (::value-ctx props))

            ; Need schema set and in multicolor case you need to know which element
            option-records-untupled (options-values options-ctx select-props)


            ; Convoluted context manipulation due to https://github.com/hyperfiddle/hyperfiddle/issues/949
            ; The above untupling logic should be done by hyperfiddle
            options-ctx (hf/browse-element options-ctx (:option-element select-props 0))
            #_#_option-records-untupled' (hf/data options-ctx)
            #_#__ (js/console.log option-records-untupled option-records-untupled')]
        [:<>
         [truncated-options (count option-records-untupled)]
         (reagent.core/create-element
           (.-Typeahead react-bootstrap-typeahead)
           #js {"labelKey" (fn [record]
                             ; Must return string otherwise "invariant undefined"; you can pr-str from userland
                             (str ((:option-label select-props) record)))
                "placeholder" (:placeholder select-props)
                ; widget requires the option records, not ids
                "options" (->> option-records-untupled (sort-by (:option-label select-props)) to-array)
                "onChange" (fn [jrecord]
                             ; foreign lib state is js array, single select is lifted into List like multi-select
                             ; unselected is []
                             (let [[?record] (array-seq jrecord)
                                   ?v (hf/id options-ctx ?record)]
                               ((:on-change select-props) ?v)))
                ; V might not be in options - widget accounts for this by taking a selected record rather than identity
                ; V might have different keys than options - as long as :option-label works, it doesn't matter
                "selected" (if ?v #js [?v] #js [])

                "highlightOnlyResult" true                  ; this helps avoid

                ; Rendering strategy that works in tables
                ; http://hyperfiddle.hyperfiddle.site/:hyperfiddle.ide!edit/(:fiddle!ident,:hyperfiddle!ide)
                "bodyContainer" true
                "align" "left"
                "disabled" disabled})]))))

(defn- props->async-typeahead [props]
  (let [j-props (apply js-obj (apply concat props))]
    (reagent.core/create-element (.-AsyncTypeahead react-bootstrap-typeahead) j-props)))

(defn- adapt-options [select-props options]
  (to-array (sort-by (:option-label select-props) options)))

(defn- typeahead-error [{rt :runtime pid :partition-id :as ctx} e select-props common-props]
  ; even if browsing fails, the user needs a chance to alter their search, so just add an ugly error message
  [:<>
   [select-error-cmp (or (ex-message e) (str e))]           ; should use error-comp, wrong ctx in scope though
   [truncated-options 0]
   ^{:key :async-typeahead}
   [props->async-typeahead
    (assoc common-props
      "options" (adapt-options select-props [])
      "onChange" (fn [jrecord]
                   ; foreign lib state is js array, single select is lifted into List like multi-select
                   ; unselected is []
                   (let [[?record] (array-seq jrecord)]
                     (if (nil? ?record)                     ; not sure how this can be anything but nil
                       ((:on-change select-props) nil)
                       (runtime/set-error rt pid (ex-info "Record selected when no records hydrated"
                                                          {:record (pr-str ?record)}))))))]])

(defn- typeahead-success [ctx select-props common-props]
  [:<>
   [truncated-options @(r/fmap count (:hypercrud.browser/result ctx))]
   ^{:key :async-typeahead}
   [props->async-typeahead
    (assoc common-props
      ; widget requires the option records, not ids
      "options" (->> (condp some [(unqualify (contrib.datomic/parser-type (hf/qfind ctx)))]
                       #{:find-coll :find-scalar} (hf/data ctx)
                       #{:find-rel :find-tuple} (mapv #(get % (:option-element select-props 0))
                                                      (hf/data ctx)))
                     (adapt-options select-props))
      "onChange" (fn [jrecord]
                   ; foreign lib state is js array, single select is lifted into List like multi-select
                   ; unselected is []
                   (let [[?record] (array-seq jrecord)
                         element-ctx (hf/browse-element ctx (:option-element select-props 0))
                         ?id (hf/id element-ctx ?record)]
                     ((:on-change select-props) ?id))))]])

(defn- select-needle-typeahead [{rt :runtime :as parent-ctx} options-pid value
                                ; todo these props need cleaned up, shouldn't be adapting them over and over again
                                select-props options-props]
  [:div {:key options-pid}
   [iframe/stale-browse (context/set-partition parent-ctx options-pid)
    typeahead-error typeahead-success select-props
    {
     #_#_"delay" 200
     "isLoading" (runtime/loading? rt options-pid)
     #_#_"minLength" 2
     "filterBy" (constantly true)                           ; no client side filtering
     "onSearch" (fn [s]
                  (-> (mlet [args (context/build-args+ parent-ctx @(:hypercrud.browser/link parent-ctx))
                             route (context/build-route+ args parent-ctx)]
                        (if (and s (:hf/where select-props))
                          (->> (route/fill-where (:hf/where select-props) s)
                               (assoc route ::route/where)
                               route/validate-route+)
                          (return route)))
                      (either/branch
                        (fn [e] (runtime/set-error rt options-pid e))
                        (fn [route]
                          (runtime/set-route rt options-pid route)))))

     ; Must return string otherwise "invariant undefined"; you can pr-str from userland
     "labelKey" (comp str (:option-label select-props))
     "placeholder" (:placeholder select-props)
     ; V might not be in options - widget accounts for this by taking a selected record rather than identity
     ; V might have different keys than options - as long as :option-label works, it doesn't matter
     "selected" (if value #js [value] #js [])

     "highlightOnlyResult" true                             ; this helps avoid

     ; Rendering strategy that works in tables
     ; http://hyperfiddle.hyperfiddle.site/:hyperfiddle.ide!edit/(:fiddle!ident,:hyperfiddle!ide)
     "bodyContainer" true
     "align" "left"

     #_#_"promptText" "Type to search..."
     #_#_"searchText" "Searching..."
     "useCache" false}]])

(defn ^:export select-needle
  ([_ ctx props] (select-needle ctx props))
  ([ctx props]
   (assert (:options props) "select: :options prop is required")
   ; any error in this mlet is fatal, don't bother rendering the typeahead
   (-> (mlet [link-ref (data/select+ ctx (keyword (:options props)))
              link-ctx (context/refocus-to-link+ ctx link-ref)]
         (either/branch
           (context/build-route-and-occlude+ link-ctx link-ref)
           (fn [e]
             ; if this failed we don't care, either not an iframe or the route is likely incomplete, just show no initial options
             ; we do need to create a placeholder partition though, (because the backend would not have due to errors)
             (let [options-pid (context/build-pid-from-link ctx link-ctx nil)]
               ; hopefully we are in a unique enough parent-ctx slot, otherwise pid clashes may happen (no occlusion or route)
               (runtime/create-partition (:runtime ctx) (:partition-id ctx) options-pid)
               (mlet [[select-props options-props] (options-value-bridge+ ctx props)]
                 (return [select-needle-typeahead link-ctx options-pid (hf/data ctx) select-props options-props]))))
           (fn [[occluded-ctx initial-route]]
             (let [options-pid (context/build-pid-from-link ctx link-ctx initial-route)]
               (cond
                 ; hybrid, use the existing iframe to render initial options, but use the needle to filter on server
                 (:hf/where props)
                 (mlet [[select-props options-props] (options-value-bridge+ ctx props)]
                   (return [select-needle-typeahead occluded-ctx options-pid (hf/data ctx) select-props options-props]))

                 ; iframe (data is local), complete-route, and no needle
                 ; just use client side filtering on the intial options
                 :else (->> (context/set-partition occluded-ctx options-pid)
                            (base/browse-partition+)
                            (cats/fmap (fn [options-ctx] [typeahead-html nil options-ctx (assoc props ::value-ctx ctx)]))))))))

       (either/branch
         (fn [e] [(ui-error/error-comp ctx) e])
         identity))))

(defn ^:export typeahead [ctx props]
  [select-needle ctx props])

(defn ^:export select
  ([ctx props] [select-needle ctx props])
  ([_ ctx props] (select ctx props)))
