(ns hyperfiddle.ide.ui.route-editor
  (:require
    [clojure.spec.alpha :as s]
    [contrib.pprint :refer [pprint-str]]
    [contrib.ui]
    [hypercrud.browser.base]
    [hyperfiddle.api :as hf]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime :as runtime]))


(defn- assert-new-v [route attr new-v]
  (->> (if (nil? new-v)
         (dissoc route attr)
         (assoc route attr new-v))
       (s/assert :hyperfiddle/route))
  new-v)

(defn- route-element [rt branch attr]
  (let [route (runtime/get-route rt branch)
        parse-string (fn [s] (assert-new-v route attr (contrib.reader/read-edn-string! s)))
        to-string pprint-str]
    [contrib.ui/debounced
     {:value (get route attr)
      :on-change (fn [o n]
                   (->> (if (nil? n)
                          (dissoc route attr)
                          (assoc route attr n))
                     (hf/set-route rt branch)))
      :mode "clojure"
      :lineNumbers false}
     contrib.ui/validated-cmp parse-string to-string contrib.ui/code]))

(defn- datomic-args [rt branch]
  [route-element rt branch ::route/datomic-args]
  #_(either/branch
      (hypercrud.browser.base/hydrate-fiddle+ rt pid (::route/fiddle (runtime/get-route rt branch)))
      (fn [e]
        ; no fiddle for whatever reason, just hand the user a raw edn editor
        [route-element rt branch ::route/datomic-args])
      (fn [fiddle]
        (case (:fiddle/type fiddle)
          :query (let []
                   [route-element rt branch ::route/datomic-args])
          :entity (let []
                    [route-element rt branch ::route/datomic-args])
          :blank [route-element rt branch ::route/datomic-args]))))

(defn- form-row [label & content]
  [:div.row
   [:label.col-sm-2.col-form-label label]
   (into [:div.col-sm-10] content)])

(defn form-editor [rt branch]
  [:form {:style {:padding "0 1rem" :background-color "#def"}}
   [form-row "fiddle" [route-element rt branch ::route/fiddle]]
   [form-row "inputs" [datomic-args rt branch]]
   ;[form-row "service args" [route-element rt branch ::route/service-args]]
   [form-row "fragment" [route-element rt branch ::route/fragment]]])

(defn edn-editor [rt branch]
  [contrib.ui/debounced
   {:value (runtime/get-route rt branch)
    :on-change (fn [o n] (hf/set-route rt branch n))
    :lineNumbers false}
   contrib.ui/cm-edn])
