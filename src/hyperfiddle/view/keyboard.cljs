(ns hyperfiddle.view.keyboard
  (:require [clojure.string :as str]
            [contrib.data :as data]
            [hyperfiddle.view.keyboard.keys :as keys]))

(defn code [^js e]
  (let [key (.-key e)]
    (if (= "Dead" key)
      (get keys/native-keys (.-which e) key)
      key)))

(defn keyboard-key [^js e]
  (let [key (code e)]
    {:key   key
     :ctrl  (.-ctrlKey e)
     :alt   (.-altKey e)
     :shift (.-shiftKey e)
     :enter (= "Enter" key)}))

(defn- key->name [key]
  (str/lower-case (name key)))

(defn- combo?* [combo key]
  (let [key-name  (key->name (:key key))
        key-set   (->> (data/filter-vals true? key)
                       (keys)
                       (map key->name)
                       (set))
        combo-set (set (map key->name combo))]
    #_(js/console.log {:key-name  key-name
                       :key-set   key-set
                       :combo-set combo-set
                       :final     (conj key-set key-name)})
    (= combo-set
       (conj key-set key-name))))

(defn combo? [combo key]
  (if (set? combo)
    (some true? (map #(combo?* % key) combo))
    (combo?* combo key)))
