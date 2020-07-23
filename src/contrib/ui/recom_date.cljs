(ns contrib.ui.recom-date
  (:require
    [clojure.set :refer [rename-keys]]
    [contrib.data :refer [update-existing]]
    [contrib.string :refer [lpad-str]]
    [contrib.reactive :as r]
    [contrib.ui :refer [validated-cmp]]
    [goog.date.UtcDateTime]
    [re-com.core :as re-com]))


(let [parse-iso8601-string (fn [s]
                             (if (empty? s)
                               nil
                               (let [ms (.parse js/Date s)] ; NaN if not valid string
                                 (assert (integer? ms))
                                 (js/Date. ms))))
      to-string (fn [v] (some-> v .toISOString))]           ; letfn .toISOString throws in browser? #470
  (defn iso8601-string [props]
    [validated-cmp props parse-iso8601-string to-string contrib.ui/text]))

(let [on-change (fn [^js v] (.-date v))]                    ; goog.date -> javascript Date
  (defn recom-date [props]
    ; (new goog.date.UtcDateTime(new Date())).toIsoString()
    (let [props (-> props
                    (assoc :model (some-> (:value props) (goog.date.UtcDateTime.)))
                    (update-existing :on-change r/comp on-change)
                    (update :class #(str % (if (:disabled props) " disabled")))
                    (rename-keys {:disabled :disabled?})
                    (select-keys [:class :disabled? :id :model :on-change]))]
      ; I don't think :class does anything
      (into [re-com/datepicker-dropdown] (apply concat props)))))

(defn date->hhmm-str
  "From an instant, extract the mins and seconds and encode as a string, for recom timepicker."
  [?v]
  {:pre [(or (nil? ?v)
             (inst? ?v))]}
  (if-let [v (some-> ?v (goog.date.UtcDateTime.))]
    (str (lpad-str 2 "0" (str (.getHours v)))
         (lpad-str 2 "0" (str (.getMinutes v))))
    ; nil means empty or identity element; handle nil at call-site if you don't like this
    "0000"))

(comment
  (= "1450" (date->hhmm-str (goog.date.UtcDateTime. #inst "2018-11-26T14:50:40.621-00:00")))
  (= "1908" (date->hhmm-str (goog.date.UtcDateTime. #inst "2018-12-13T19:08:37.800-00:00"))))

(let [on-change (fn [v-old hhmm]
                  (let [hh (js/Math.floor (/ hhmm 100))
                        mm (mod hhmm 100)
                        v' (doto (goog.date.UtcDateTime. v-old) ; don't mutate any shared refs
                             (.setMinutes mm)
                             (.setHours hh))]
                    (.-date v')))]
  (defn recom-time [props]
    (let [hhmm (date->hhmm-str (:value props))
          hhmm-int (js/parseInt hhmm 10)                    ; wut recom says it accepts a string and recom internals use string form
          ;_ (println "recom-time: "  hhmm "; " hhmm-int "; " (pr-str (:value props)))
          props (-> props
                    (assoc :model hhmm-int)
                    (update-existing :on-change r/comp (r/partial on-change (:value props)))
                    (update :class #(str % (if (:disabled props) " disabled")))
                    (rename-keys {:disabled :disabled?})
                    (select-keys [:class :disabled? :id :model :on-change]))]
      (into [re-com/input-time] (apply concat props)))))
