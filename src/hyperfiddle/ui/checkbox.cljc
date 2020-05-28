(ns hyperfiddle.ui.checkbox
  "@nossr"
  (:require [contrib.reactive :as r]))

(defn- on-change* [{:keys [on-change]}, e]
  (on-change (.. e -target -checked)))

(defn- Checkbox* [{:keys [type name checked? disabled? children]
                   :as   props
                   :or   {type :checkbox}}]
  (into [:label (merge {:style {:display     :flex
                                :align-items :center}}
                       (dissoc props :type :name :checked? :disabled? :children))
         [:input {:type      type
                  :name      name
                  :checked   checked?
                  :disabled  disabled?
                  :on-change (r/partial on-change* props)
                  :style     {:margin-right "0.25rem"}}]]
        children))

(defn Checkbox [props & children]
  (Checkbox* (assoc props :children children)))

(defn- Radio [props & children]
  (apply Checkbox (assoc props :type :radio) children))


;; Example usage:
;; [RadioGroup {:name      "unique-group-name"
;;              :options   [{:key :foo, :text "option foo"}
;;                          {:key :bar, :text "option bar"}
;;                          {:key :baz, :text "option baz"}]
;;              :value     :bar
;;              :on-change js/console.log}]
(defn RadioGroup [{:keys [on-change props name options value]
                   :or   {name (gensym)}}]
  [:div props
   (for [{:keys [key text props]} options]
     ^{:key key}
     [Radio (merge {:name      name
                    :on-change (r/partial on-change key)
                    :checked?  (= key value)}
                   props)
      text])])
