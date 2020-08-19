(ns contrib.ui
  (:refer-clojure :exclude [keyword long])
  (:require
    [cats.core :as cats]
    [contrib.data :refer [update-existing for-kv unqualify]]
    [contrib.pprint :refer [pprint-str pprint-async]]
    [contrib.reactive :as r]
    [contrib.reader]
    [contrib.string :refer [blank->nil]]
    [contrib.try$ :refer [try-either]]
    [contrib.ui.codemirror :refer [-codemirror]]
    [contrib.ui.tooltip :refer [tooltip]]
    [goog.functions :as functions]
    [reagent.core :as reagent]
    [taoensso.timbre :as timbre]
    [hyperfiddle.api :as hf]))


(def default-debounce-ms 300)

(comment
  ; component-did-update
  (for [[bs os] [[["aaa" nil "xxx"] ["aaa" nil]]
                 [["aaa"] ["aaa"]]
                 [["bbb"] ["aaa"]]]
        b bs]
    (let [[l r] (split-with #(not= % b) os)]
      {:b b
       :os os
       :discards l
       :rest r}))
  ; =>
  {:b "aaa", :os ["aaa" nil], :discards (), :rest ("aaa" nil)}
  {:b nil, :os ["aaa" nil], :discards ("aaa"), :rest (nil)}
  {:b "xxx", :os ["aaa" nil], :discards ("aaa" nil), :rest ()}
  {:b "aaa", :os ["aaa"], :discards (), :rest ("aaa")}
  {:b "bbb", :os ["aaa"], :discards ("aaa"), :rest ()})

(let [debounce (memoize functions/debounce)
      debounced-adapter (fn [os-ref f n]
                          (let [o (last (:old-values @os-ref))]
                            (swap! os-ref update :old-values conj n)
                            (when f (f o n))))
      on-change (fn [os-ref n]                              ; letfn not working #470
                  (swap! os-ref assoc :value n)
                  n)]
  (def ^:private debounced-impl
    (reagent/create-class
      {:reagent-render
       (fn [os-ref props comp & args]
         (let [props (-> (if-some [value @(r/cursor os-ref [:value])]
                           (assoc props :value value)
                           (dissoc props :value))
                         (dissoc :debounce/interval)
                         (update :on-change (fn [f]
                                              (let [f (debounce (r/partial debounced-adapter os-ref f)
                                                                (or (:debounce/interval props) default-debounce-ms))]
                                                (r/comp f (r/partial on-change os-ref))))))]
           (into [comp props] args)))
       :component-did-update
       (fn [this]
         (let [[_ os-ref props comp & args] (reagent/argv this)
               b (:value props)
               {os :old-values a :value} @os-ref]
           (let [[discards rest] (split-with #(not= % b) os)]
             (if (empty? rest)
               (do
                 (when (< 1 (count discards))
                   ; this is either not a big deal, e.g. default values have been applied
                   ; or multiple users are editing the same value and changes are probably being lost
                   (timbre/warn "Potential conflicting concurrent edits detected, discarding local state" {:a a :b b :os os}))
                 (reset! os-ref {:old-values [b] :value b}))
               (swap! os-ref assoc :old-values (vec rest))))))})))

(defn debounced [props comp & args]
  (let [os-ref (r/atom {:value (:value props)
                        :old-values [(:value props)]})]
    (fn [props comp & args]
      (into [debounced-impl os-ref props comp] args))))

(defn- validated-cmp-initial-state-val [to-string props]
  {:s-value (to-string (:value props))
   :last-valid-value (:value props)})

(let [on-change (fn [f state parse-string new-s-value]
                  (swap! state assoc :s-value new-s-value)
                  (->> (try-either
                         (let [new-value (parse-string new-s-value)] ; todo this should be atomic, but we still want to throw
                           (swap! state assoc :last-valid-value new-value)
                           new-value))
                       (cats/fmap f))
                  nil)
      on-blur (fn [state f e]
                (f (:last-valid-value @state)))]
  (def ^:private validated-cmp-impl
    (reagent/create-class
      {:reagent-render
       (fn [state props parse-string to-string cmp & args]
         (let [s-value @(r/cursor state [:s-value])
               props (-> (assoc props :value s-value)
                         (assoc ::hf/is-invalid (or (try (parse-string s-value) false (catch :default e true))
                                                (::hf/is-invalid props)))
                         (update-existing :on-blur (fn [f]
                                                     (r/partial on-blur state f)))
                         (update :on-change (fn [f]
                                              (r/partial on-change f state parse-string)))
                         (dissoc :magic-new-mode))]
           (into [cmp props] args)))
       :component-did-update
       (fn [this]
         (let [[_ state props parse-string to-string cmp & args] (reagent/argv this)]
           (when-not (:magic-new-mode props)                ; https://github.com/hyperfiddle/hyperfiddle/issues/586
             (when-not (= (:last-valid-value @state) (:value props))
               (reset! state (validated-cmp-initial-state-val to-string props))))))})))

(defn validated-cmp [props parse-string to-string cmp & args]
  (let [state (r/atom (validated-cmp-initial-state-val to-string props))]
    (fn [props parse-string to-string cmp & args]
      (into [validated-cmp-impl state props parse-string to-string cmp] args))))

(let [target-value (fn [e] (.. e -target -value))]          ; letfn not working #470
  (defn textarea [props]
    [:textarea (update-existing props :on-change r/comp target-value)]))

(let [checked (fn [e] (.. e -target -checked))]             ; letfn not working #470
  (defn checkbox [props]
    [:input (-> (assoc props :type "checkbox")
                (update-existing :on-change r/comp checked)
                (select-keys [:type :checked :on-change :disabled :read-only :class :style #_::hf/is-invalid]))]))

(let [target-value (fn [e] (.. e -target -value))]          ; letfn not working #470
  (defn text [props]
    (let [props (-> (assoc props :type "text")
                    (update-existing :on-change r/comp target-value)
                    (select-keys [:type :value :default-value :on-change :style :read-only :disabled
                                  :html/placeholder :html/class
                                  :placeholder :class])
                    (for-kv {} (fn [m k v] (assoc m (unqualify k) v))))]
      [:input props])))

(let [parse-string (fn [s]                                  ; letfn not working #470
                     (let [v (some-> s contrib.reader/read-edn-string!)]
                       (assert (or (nil? v) (keyword? v)))
                       v))
      to-string (fn [v] (some-> v pr-str))]
  (defn keyword [props]
    [validated-cmp props parse-string to-string text]))

(let [parse-string (fn [s] (some-> s contrib.reader/read-edn-string!)) ; letfn not working #470
      to-string (fn [v] (some-> v pr-str))]
  (defn edn [props]
    [validated-cmp props parse-string to-string text]))

(let [parse-string (fn [s]                                  ; letfn not working #470
                     (when-let [s (blank->nil s)]
                       (let [v (js/parseInt s 10)]
                         (assert (integer? v))
                         v)))]
  (defn long [props]
    [validated-cmp props parse-string str text]))

(let [target-value (fn [e] (js/parseInt (.. e -target -value)))]          ; letfn not working #470
  (defn slider [props]
    (let [props (-> (assoc props :type "range")
                    (update-existing :on-change r/comp target-value)
                    (select-keys [:type :value :default-value :on-change :style :read-only :disabled
                                  :html/placeholder :html/class
                                  :placeholder :class :min :max :step])
                    (for-kv {} (fn [m k v] (assoc m (unqualify k) v))))]
      [:input props])))

(defn easy-checkbox [props & [label]]
  (let [control [checkbox props]]
    (if (blank->nil label)
      [:label (-> props
                  (assoc :style {:font-weight "400"})
                  (select-keys [:class :style]))
       control " " label]
      control)))

(defn ^:export easy-checkbox-boolean [label r & [props]]
  [easy-checkbox (assoc props
                   :checked (boolean @r)
                   :on-change (r/partial swap! r not))
   label])

(def inline-spinner
  [:svg
   {:viewBox "0 0 32 32", :height "32", :width "32"}
   [:g {:fill "#1a202c"} ;; text-gray-900
    [:circle
     {:r "3", :cy "16", :cx "4"}
     [:animate
      {:values        "3;4;4;3;3;3",
       :keyTimes      "0;0.2;0.4;0.6;0.8;1",
       :repeatCount   "indefinite",
       :begin         "0s",
       :dur           "1s",
       :attributeName "r"}]
     [:animate
      {:values        "0.4;0.8;0.8;0.4;0.4;0.4",
       :keyTimes      "0;0.2;0.4;0.6;0.8;1",
       :repeatCount   "indefinite",
       :begin         "0s",
       :dur           "1s",
       :attributeName "opacity"}]]
    [:circle
     {:r "3", :cy "16", :cx "16"}
     [:animate
      {:values        "3;3;4;4;3;3",
       :keyTimes      "0;0.2;0.4;0.6;0.8;1",
       :repeatCount   "indefinite",
       :begin         "0s",
       :dur           "1s",
       :attributeName "r"}]
     [:animate
      {:values        "0.4;0.4;0.8;0.8;0.4;0.4",
       :keyTimes      "0;0.2;0.4;0.6;0.8;1",
       :repeatCount   "indefinite",
       :begin         "0s",
       :dur           "1s",
       :attributeName "opacity"}]]
    [:circle
     {:r "3", :cy "16", :cx "28"}
     [:animate
      {:values        "3;3;3;4;4;3",
       :keyTimes      "0;0.2;0.4;0.6;0.8;1",
       :repeatCount   "indefinite",
       :begin         "0s",
       :dur           "1s",
       :attributeName "r"}]
     [:animate
      {:values        "0.4;0.4;0.4;0.8;0.8;0.4",
       :keyTimes      "0;0.2;0.4;0.6;0.8;1",
       :repeatCount   "indefinite",
       :begin         "0s",
       :dur           "1s",
       :attributeName "opacity"}]]]])

(defn async-props [_ _ _]
  (let [a-state (r/atom nil)]
    (fn [comp updatef & props]
      (let [state @a-state]
        (apply updatef (fn [value] (when-not (= state value)
                                     (reset! a-state value)))
               props)
        (if-not state
          [:div {:style {:margin "0 1rem"}}
           inline-spinner]
          (into [comp] (list state)))))))

(defn ^:export code [props]                                 ; Adapt props to codemirror props
  (let [defaults {:lineNumbers true
                  :matchBrackets true
                  :autoCloseBrackets true
                  :viewportMargin js/Infinity}
        props (-> props
                  (update :read-only #(or % (:disabled props))) ; (if (:disabled props) "nocursor" false) -- nocursor disables copy/paste
                  (dissoc :disabled))
        props (into defaults props)]
    ; There is nothing to be done about invalid css down here.
    ; You'd have to write CodeMirror implementation-specific css.
    [-codemirror props]))

(defn ^:export code-inline-block [props]
  ; (when (::hf/is-invalid props) {:class "invalid"})
  (text props))

(defn- stable-pprint-async [set-props props]
  (pprint-async (:value props) #(set-props (assoc props :value %))))

(defn async-code [props]
  [async-props code stable-pprint-async props])

(defn ^:export cm-edn [props]
  [validated-cmp (assoc props :mode "clojure") contrib.reader/read-edn-string! pprint-str code])

(defn ^:export cm-edn-inline-block [props]
  [validated-cmp (assoc props :mode "clojure") contrib.reader/read-edn-string! pprint-str code-inline-block])

(let [on-change (fn [value e]
                  ; ideally use e -target -value, but that is not very useful since it would require string serialization
                  value)]
  (defn radio [props]
    [:input (-> (assoc props :type "radio")
                (update-existing :value str)
                (update-existing :on-change r/comp (r/partial on-change (:value props))))]))

(defn radio-with-label [props]
  [tooltip {:label (:tooltip props)}
   [:label.radio-option {:class (if (:disabled props) "disabled")}
    [radio (-> props
               (dissoc :tooltip :label)
               (update :style merge {:width "auto"}))]
    (or (:label props) (str (:value props)))]])
