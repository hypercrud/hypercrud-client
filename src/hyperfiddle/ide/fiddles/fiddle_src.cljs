(ns hyperfiddle.ide.fiddles.fiddle-src
  (:require
    [cuerdas.core :as str]
    [contrib.pprint]
    [contrib.reactive :as r]
    [contrib.reagent :refer [fragment]]
    [contrib.ui]
    [hypercrud.types.URI :refer [is-uri?]]
    [hyperfiddle.ide.fiddles.topnav :refer [shadow-fiddle]]
    [hyperfiddle.ui :refer [markdown]]
    [hyperfiddle.ui :refer [browse field link hyper-control]]))


(defn schema-links [ctx]
  (->> (-> ctx :target-domain :domain/environment)
       (filter (fn [[k v]] (and (str/starts-with? k "$") (is-uri? v))))
       sort
       (map (fn [[$db _]]
              (let [props {:route [(keyword "hyperfiddle.schema" $db)]
                           :target "_blank"}]
                ^{:key $db}
                [(:navigate-cmp ctx) props $db])))
       (doall)))

(defn control-with-unders [frag value ctx props]
  [:div [(hyper-control ctx) value ctx props] frag])

(def underdocs
  {:fiddle/pull "See [:fiddle/pull examples](http://www.hyperfiddle.net/:docs!fiddle-pull/) and the
  [Datomic pull docs](https://docs.datomic.com/on-prem/pull.html)."
   :fiddle/query "See [:fiddle/query examples](http://www.hyperfiddle.net/:docs!fiddle-query/) and the
   [Datomic query docs](https://docs.datomic.com/on-prem/query.html)."
   :fiddle/markdown "See [:fiddle/markdown examples](http://www.hyperfiddle.net/:docs!fiddle-markdown/)."
   :fiddle/css "See [:fiddle/css examples](http://www.hyperfiddle.net/:docs!fiddle-css/)."
   :fiddle/renderer "See [:fiddle/renderer examples](http://www.hyperfiddle.net/:docs!fiddle-renderer/). `ctx` and
   `class` are in lexical scope. No `(ns (:require ...))` yet so vars must be fully qualified."
   :fiddle/links "See [:fiddle/links examples](http://www.hyperfiddle.net/:docs!fiddle-links/)."})

(defn fiddle-src-renderer [ctx-real class & {:keys [embed-mode]}]
  (let [ctx-real (dissoc ctx-real :user-renderer)           ; this needs to not escape this level; inline links can't ever get it
        ctx (shadow-fiddle ctx-real)
        controls
        {:fiddle/pull (r/partial control-with-unders (fragment :_ [:span.schema "schema: " (schema-links ctx)] [:div.hf-underdoc [markdown (:fiddle/pull underdocs)]]))
         :fiddle/query (r/partial control-with-unders (fragment :_ [:span.schema "schema: " (schema-links ctx)] [:div.hf-underdoc [markdown (:fiddle/query underdocs)]]))
         :fiddle/markdown (r/partial control-with-unders [:div.hf-underdoc [markdown (:fiddle/markdown underdocs)]])
         :fiddle/css (r/partial control-with-unders [:div.hf-underdoc [markdown (:fiddle/css underdocs)]])
         :fiddle/renderer (r/partial control-with-unders [:div.hf-underdoc [markdown (:fiddle/renderer underdocs)]])
         :fiddle/links (r/partial control-with-unders [:div.hf-underdoc [markdown (:fiddle/links underdocs)]])
         }]
    [:div.fiddle-src {:class class}                         ; unify fiddle-src .devsrc
     [:h3 (str @(r/cursor (:hypercrud.browser/result ctx) [:fiddle/ident])) " source"]
     (field [0 :fiddle/ident] ctx nil)
     (field [0 :fiddle/type] ctx nil)
     (case @(r/cursor (:hypercrud.browser/result ctx) [:fiddle/type])
       :entity (fragment :_
                         (field [0 :fiddle/pull-database] ctx nil)
                         (field [0 :fiddle/pull] ctx (controls :fiddle/pull)))
       :query (field [0 :fiddle/query] ctx (controls :fiddle/query))
       :blank nil
       nil nil)
     (field [0 :fiddle/markdown] ctx (controls :fiddle/markdown))
     (field [0 :fiddle/css] ctx (controls :fiddle/css))
     (field [0 :fiddle/renderer] ctx (controls :fiddle/renderer))
     (when-not embed-mode (field [0 :fiddle/links] ctx-real (controls :fiddle/links)))
     (when-not embed-mode (link :hyperfiddle/remove [0] ctx "Remove fiddle"))]))

(defn hacked-links [value ctx props]
  (let [links (->> value
                   (remove :link/disabled?)
                   (map #(select-keys % [:link/rel :link/path :link/fiddle :link/render-inline? :link/formula]))
                   (map #(update % :link/fiddle :fiddle/ident))
                   (map pr-str)
                   (interpose "\n"))]
    [:div
     [:pre (if (seq links) links "No links or only auto links")]
     [:div.hf-underdoc [markdown "Due to an issue in the live embed, the links are shown as EDN until we fix it."]]]))

(defn docs-embed [& attrs]
  (fn [ctx-real class & {:keys [embed-mode]}]
    (let [ctx-real (dissoc ctx-real :user-renderer)         ; this needs to not escape this level; inline links can't ever get it
          ctx (shadow-fiddle ctx-real)
          {:keys [:fiddle/ident]} @(:hypercrud.browser/result ctx)
          controls
          {:fiddle/pull (r/partial control-with-unders (fragment :_ [:span.schema "schema: " (schema-links ctx)]))
           :fiddle/query (r/partial control-with-unders (fragment :_ [:span.schema "schema: " (schema-links ctx)]))
           :fiddle/links hacked-links}]
      (fn [ctx-real class & {:keys [embed-mode]}]
        (into
          [:div.fiddle-src {:class class}]
          (for [k attrs]
            (field [0 k] ctx (controls k))))))))

(defn result-edn [& attrs]
  (fn [{:keys [hypercrud.browser/result]}]
    (let [s (-> @result
                (as-> $ (if (seq attrs) (select-keys $ attrs) $))
                hyperfiddle.ui.hacks/pull-soup->tree
                (contrib.pprint/pprint-str 40))]
      [contrib.ui/code-block {:read-only true} s])))

; This is in source control because all hyperblogs want it.
; But, they also want to tweak it surely. Can we store this with the fiddle ontology?
(defn hyperfiddle-live [rel ctx & fiddle-attrs]
  (let [state (r/atom {:edn-fiddle false :edn-result false})]
    (fn [rel ctx & fiddle-attrs]
      [:div.hf-live.unp
       [:div.row
        ; Reverse order so it looks right on mobile, larger views reorder
        (let [as-edn (r/cursor state [:edn-result])]
          [:div.col-sm-6.col-sm-push-6
           [:div "Result:" [contrib.ui/easy-checkbox as-edn " EDN?" "hf-live"]]
           (browse rel [] ctx (if @as-edn (result-edn)))])
        (let [as-edn (r/cursor state [:edn-fiddle])]
          [:div.col-sm-6.col-sm-pull-6
           [:div "Interactive Hyperfiddle editor:" [contrib.ui/easy-checkbox as-edn " EDN?" "hf-live"]]
           (browse rel [] ctx (apply (if @as-edn result-edn docs-embed) fiddle-attrs) :frag ":src" :class "devsrc")])
        ]])))
