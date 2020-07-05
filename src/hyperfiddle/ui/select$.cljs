(ns hyperfiddle.ui.select$                                  ; Namespace clashes with var hyperfiddle.ui/select
  (:require-macros [contrib.do :refer [do-result from-result]])
  (:require
    ["react-bootstrap-typeahead" :refer [Typeahead AsyncTypeahead #_TypeaheadInputSingle]]
    [cats.core :as cats :refer [mlet return]]
    [cats.monad.either :as either :refer [left right]]
    [contrib.ct :refer [unwrap]]
    [contrib.data :refer [unqualify]]
    [contrib.reactive :as r]
    [contrib.reader]
    [contrib.string :refer [blank->nil]]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hyperfiddle.api :as hf]
    [hyperfiddle.data :as data]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui.error :as ui-error]
    [hyperfiddle.ui.iframe :as iframe]
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

(declare options-value-bridge+)
(declare select-error-cmp)

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
        select-props {:option-element option-element        ; add ::hf namespace to all these keys
                      :option-value (let []
                                      (fn [val ctx]
                                        ; in the case of tupled options, the userland view props must
                                        ; indicate which element is the entity has an identity which matches the anchor eav.
                                        (condp some [(unqualify (contrib.datomic/parser-type @(:hypercrud.browser/qfind ctx)))]
                                          #{:find-coll :find-scalar} (hf/row-key ctx val)
                                          #{:find-rel :find-tuple} (get (hf/row-key ctx val) option-element))))
                      :option-label (fn [val]
                                      (let [option-label (:option-label props pr-str)]
                                        (option-label val)))
                      :multiple (context/attr? anchor-ctx :db.cardinality/many)}
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
                                 :on-change ((::hf/view-change! anchor-ctx) anchor-ctx)}
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
  #_(when (= hf/browser-query-limit n)
      [:div.alert.alert-warning (str/format "Warning: Options resultset has been truncated to %s records. Please add additional filters" hf/browser-query-limit)]))

(defn label-key
  "Given a `select-props` and a `record`, will generate a label key suitable for
  react bootstrap typeahead component. The result is also suitable as an `:id`
  stub. Must return string otherwise \"invariant undefined\"; you can pr-str
  from userland."
  [select-props record]
  ; :option-label can be a function or a keyword
  (str ((:option-label select-props) record)))

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
         [:> Typeahead {:id          (:html/id select-props) ; Warning: [react-bootstrap-typeahead] The `id` prop will be required in future versions to make the component accessible for users of assistive technologies such as screen readers.
                        :labelKey    (r/partial label-key select-props)
                        :placeholder (:placeholder select-props)
                        ;; widget requires the option records, not ids
                        :options     (->> option-records-untupled (sort-by (:option-label select-props)) to-array)
                        :onChange    (fn [jrecord]
                                       ;; foreign lib state is js array, single select is lifted into List like multi-select
                                       ;; unselected is []
                                       (let [?n (vec (array-seq jrecord))
                                             ?n (if (and (:allow-new select-props)
                                                         (object? (last ?n))
                                                         (.-customOption (last ?n)))
                                                  (conj (pop ?n) (.-label (last ?n)))
                                                  ?n)]
                                         ((:on-change select-props) ?v ?n)))
                        ;; V might not be in options - widget accounts for this by taking a selected record rather than identity
                        ;; V might have different keys than options - as long as :option-label works, it doesn't matter
                        :selected    (if (:multiple select-props)
                                       (object-array ?v)
                                       #js [?v])

                        :multiple (:multiple select-props)

                        :allowNew (:allow-new select-props)

                        :highlightOnlyResult (not (:allow-new select-props)) ; does not work with allowNew enabled, avoiding a warning

                        ;; Rendering strategy that works in tables
                        ;; http://hyperfiddle.hyperfiddle.site/:hyperfiddle.ide!edit/(:fiddle!ident,:hyperfiddle!ide)
                        :bodyContainer true
                        :align         "left"
                        :disabled      disabled}]]))))

(defn- adapt-options [select-props options]
  (to-array options))

(defn- typeahead-error [{rt :runtime pid :partition-id :as ctx} e ?v select-props common-props]
  ; even if browsing fails, the user needs a chance to alter their search, so just add an ugly error message
  [:<>
   [select-error-cmp (or (ex-message e) (str e))]           ; should use error-comp, wrong ctx in scope though
   [truncated-options 0]
   ^{:key :async-typeahead}
   [:> AsyncTypeahead
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

(defn- typeahead-success [ctx ?v select-props common-props]
  #_(js/console.log "typeahead-success " (hf/data ctx))
  [:<>
   [truncated-options @(r/fmap count (:hypercrud.browser/result ctx))]
   ^{:key :async-typeahead}
   [:> AsyncTypeahead
    (assoc common-props
      #_#_                                                  ; Todo, add invalid css
      "renderInput" (fn [jsInputProps]
                      ; https://github.com/ericgio/react-bootstrap-typeahead/blob/master/docs/Rendering.md#renderinput-gotchas
                      ; https://github.com/ericgio/react-bootstrap-typeahead/blob/e80cf3b73c6996cf03ee60836cb79bf5b064c4eb/src/Typeahead.react.js#L110-L115
                      (reagent.core/create-element
                        TypeaheadInputSingle
                        jsInputProps                        ; add hyperfiddle-invalid
                        ))
      ; widget requires the option records, not ids
      "options" (->> (condp some [(unqualify (contrib.datomic/parser-type (hf/qfind ctx)))]
                       #{:find-coll :find-scalar} (hf/data ctx)
                       #{:find-rel :find-tuple} (mapv #(get % (:option-element select-props 0))
                                                      (hf/data ctx)))
                     (adapt-options select-props))
      "onChange" (fn [jxs] ; foreign lib state is js array of selections (single select is lifted into multi)
                   (let [xs (vec (array-seq jxs))           ; current selection state
                         is-new (and (object? (last xs)) (.-customOption (last xs)))
                         xs (if (and is-new (:allow-new select-props))
                              (conj (pop xs) (.-label (last xs))) ; appears broken
                              xs)]      ; appears broken
                     ; we want to send up the hf-ident?
                     ((:on-change select-props) ?v xs))     ; sending records but need to send ident?
                   #_
                   (let [[?record] (array-seq jxs)
                         element-ctx (hf/browse-element ctx (:option-element select-props 0))
                         ?id (hf/id element-ctx ?record)]
                     ((:on-change select-props) ?id))))]])

(defn- select-needle-typeahead [{rt :runtime :as parent-ctx} options-pid ?v
                                ; todo these props need cleaned up, shouldn't be adapting them over and over again
                                select-props options-props]
  #_(js/console.log `select-needle-typeahead 'options (hf/data parent-ctx)) ; this is the sub
  [:div {:key options-pid}
   [iframe/stale-browse (context/set-partition parent-ctx options-pid)
    typeahead-error typeahead-success ?v select-props
    {
     #_#_"delay" 200
     "isLoading" (runtime/loading? rt options-pid)
     "minLength" 0                                          ; minLength 0 shows the initial result page before having typed anything
     "filterBy" (constantly true)                           ; no client side filtering
     "onSearch" (fn [s]
                  (try
                    (let [args (context/build-args parent-ctx @(:hypercrud.browser/link parent-ctx))
                          route (from-result (context/build-route+ args parent-ctx))]
                      (hf/set-route rt options-pid (assoc route (::hf/needle-key select-props) s)))
                    (catch js/Error e
                      (runtime/set-error rt options-pid e))))
     ; Must return string otherwise "invariant undefined"; you can pr-str from userland
     "labelKey" (comp str (:option-label select-props))
     "placeholder" (:placeholder select-props)
     ; V might not be in options - widget accounts for this by taking a selected record rather than identity
     ; V might have different keys than options - as long as :option-label works, it doesn't matter
     :selected (if (:multiple select-props)
                 (object-array ?v)
                 (if ?v #js [?v] #js []))                 ; adapt to multi-select
     :multiple (:multiple select-props)
     :allowNew (:allow-new select-props)
     :highlightOnlyResult (not (:allow-new select-props)) ; does not work with allowNew enabled, avoiding a warning

     ; Rendering strategy that works in tables
     ; http://hyperfiddle.hyperfiddle.site/:hyperfiddle.ide!edit/(:fiddle!ident,:hyperfiddle!ide)
     "bodyContainer" true
     "align" "left"

     #_#_"promptText" "Type to search..."
     #_#_"searchText" "Searching..."
     "useCache" false}]])

(defn ^:export picklist "typeahead picker integrated with hyperfiddle IO, single- and multi-select"
  [ctx props]
  (assert (:options props) "select: :options prop is required")
  (assert (::hf/view-change! ctx))                        ; good: (f e a adds rets) (f ctx selection) bad: (f ctx os ns)
  (assert (::hf/needle-key props))
  (try
    (let [link-ref (from-result (data/select+ ctx (:options props)))
          link-ctx (from-result (context/refocus-to-link+ ctx link-ref))
          [occluded-ctx initial-route] (from-result (context/build-route-and-occlude+ link-ctx link-ref)) ; ":link/fiddle required"
          options-pid (context/build-pid-from-link ctx link-ctx initial-route)
          [select-props options-props] (from-result (options-value-bridge+ ctx props))] ; "no attribute in scope"
      #_(js/console.log `picklist 'data (hf/data ctx) 'link-data (hf/data occluded-ctx))
      [select-needle-typeahead occluded-ctx options-pid (hf/data ctx) select-props options-props])
    (catch js/Error e
      [(ui-error/error-comp ctx) e])))
