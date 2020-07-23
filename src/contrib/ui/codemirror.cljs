(ns contrib.ui.codemirror
  (:require
    [contrib.css :refer [css]]
    [contrib.data :refer [orp]]
    [clojure.string :as string]
    [cuerdas.core :as str]
    [goog.object :as object]
    [hyperfiddle.api :as hf]
    [reagent.core :as reagent]))


(defn camel-keys "Bad anti-abstraction to undo Reagent's key obfuscating" [m]
  (reduce-kv (fn [acc k v]
               (assoc acc (keyword (str/camel (name k))) v))
             {} m))

(defn sync-changed-props! [^js ref props]
  (doseq [[prop val] props
          :let [option (str/camel (name prop))]             ; casing hacks see https://github.com/hyperfiddle/hyperfiddle/issues/497
          :when (not= val (.getOption ref option))]
    (.setOption ref option val)))

(defn set-invalid! [^js cm is-invalid]
  ; placeholder addon manipulates DOM node classname this way too:
  ; https://codemirror.net/addon/display/placeholder.js
  (let [wrapper (.getWrapperElement cm)
        new-classname (str (string/replace (.-className wrapper) " invalid" "") (when is-invalid " invalid"))]
    (set! (.-className wrapper) new-classname)))

(defn ensure-mode [^js ref new-mode]
  (js/parinferCodeMirror.setMode ref new-mode)
  #_(js/parinferCodeMirror.setOptions ref #js {"locus" (= new-mode "indent")})
  (doto (-> ref .getWrapperElement .-classList)
    (.remove "paren")
    (.remove "indent")
    (.add new-mode)))

(def -codemirror

  ; all usages of value (from react lifecycle) need to be (str value), because
  ; codemirror throws NPE if value is nil

  (reagent/create-class
    {:reagent-render
     (fn [props]
       [contrib.ui/textarea
        (-> props
            (select-keys [:id :class :default-value :on-change :value]) ; #318 Warning: React does not recognize the `lineNumbers` prop on a DOM element
            (assoc :auto-complete "off"
                   :read-only true)                         ; react-dom.inc.js:3137 Warning: Failed prop type: You provided a `value` prop to a form field without an `onChange` handler. This will render a read-only field. If the field should be mutable use `defaultValue`. Otherwise, set either `onChange` or `readOnly`.
            (cond-> (::hf/is-invalid props) (update :class css "invalid")))])

     :component-did-mount
     (fn [this]
       ; Codemirror will default to the first mode loaded in preamble
       (let [[_ props] (reagent/argv this)
             ref (js/CodeMirror.fromTextArea (reagent/dom-node this)
                                             (-> (dissoc props :on-change ::hf/is-invalid)
                                                 camel-keys ; casing hacks see https://github.com/hyperfiddle/hyperfiddle/issues/497
                                                 clj->js))]

         (when (and (:parinfer props) (= "clojure" (:mode props)))
           ; `mode` is 'paren', 'indent', or 'smart'
           (js/parinferCodeMirror.init ref "paren" #js {"locus" true})
           (ensure-mode ref "paren")                        ; sets up css
           (.addKeyMap ref #js {"Ctrl-1" #(let [cur-mode (goog.object/getValueByKeys ref "__parinfer__" "mode")]
                                            (ensure-mode ref (case cur-mode "paren" "indent" "paren")))}))

         (set-invalid! ref (::hf/is-invalid props))

         ; Props are a shitshow. Remark is stringly, and codemirror wants js types.
         ; set `lineNumber=` to disable line numbers (empty string is falsey).
         ; Also, reagent/as-element keyword-cases all the props into keywords, so
         ; they must be camelized before we get here.

         (object/set this "codeMirrorRef" ref)
         (.on ref "change" (fn [_ e]
                             (let [[_ {:keys [on-change] :as props}] (reagent/argv this)]
                               (when on-change
                                 (let [value (orp seq (.getValue ref))]
                                   (when (not= value (:value props))
                                     (if (= value (:default-value props))
                                       (on-change nil)
                                       (on-change value))))))))

         (.on ref "blur" (fn [_ ^js e]
                           (when (and (some? (:default-value props)) (= (.getValue ref) ""))
                             (.setValue ref (:default-value props)))))))

     :component-will-unmount
     (fn [this]
       (let [^js ref (object/get this "codeMirrorRef")]
         (.toTextArea ref)))

     :component-did-update
     (fn [this]
       (let [[_ props] (reagent/argv this)
             ^js ref (object/get this "codeMirrorRef")
             new-value (orp some? (:value props) (:default-value props) "")
             current-value (.getValue ref)]
         ; internal CM value state != ctor props
         (when (and (not= current-value new-value)
                    (not (and (= "" current-value) (= (:default-value props) new-value)))
                    (not (and (= "" new-value) (= (:default-value props) current-value))))
           (.setValue ref new-value))
         (set-invalid! ref (::hf/is-invalid props))
         (sync-changed-props! ref (dissoc props :default-value :value :on-change ::hf/is-invalid))))}))
