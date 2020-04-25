(ns hyperfiddle.def.system
  #?(:clj
     (:require
       [hyperfiddle.def :as hf-def])))


(hf-def/schema
  #:hyperfiddle
      {:project    []
       :owners     [:uuid* "Entity owners uuids, used by ACLs"]
       :starred    [:boolean]
       :archived   [:boolean]
       :deprecated [:boolean]}
  #:project
      {:code [:string "a ClojureScript form for storing view functions, evaluated on page load"]
       :css [:string]}
  #:attribute
      {:ident    [:keyword :unique]
       :renderer [:string "Default attribute renderer, a CLJS var like `hyperfiddle.ui.controls/code`."]}
  #:fiddle
      {:ident         [:keyword :unique "Fiddle identifier used in URLs. Warning: changing this breaks fiddle URLs."]
       :type          [:keyword "Which Datomic query API"]
       :query         [:string
                       "Datomic query datalog.

                        Warning: no support yet for rules, d/history, d/log or other datomic API access."]
       :pull          [:string "Datomic pull expression for the entity addressed by the URL"]
       :pull-database [:string "Argument to `datomic.api/pull`, defaults to $"]
       :links         [:ref* "Links to other fiddles that are available from this fiddle"
                       :isComponent true]
       :renderer      [:string "Reagent expression for the fiddle view"]
       :cljs-ns       [:string "ClojureScript `user` namespace, available in :fiddle/renderer."]
       :css           [:string
                       "Fiddle CSS.

                        Warning: CSS is not scoped, please write targetted CSS"]
       :markdown      [:string "Markdown expression for fiddle view, optional"]
       :hydrate-result-as-fiddle
                      [:boolean "Experimental. When set, data-sync will interpret this fiddle's result as a fiddle - like a higher order fiddle - this is a recursion mechanic."]
       :uuid          [:uuid :unique "For naming anonymous fiddles"]
       ;:query-needle [:string]
       }
  #:link
      {:path    [:string "todo rename; specifies the attribute for which this link is valid"]
       :class   [:keyword* "semantic selector, like html css classes"]
       :fiddle  [:ref "link target"]
       :tx-fn   [:string "names a hyperfiddle.api/tx-fn multimethod which builds a transaction"]
       :rel     [:keyword "archived"]
       :formula [:string "\"deprecated; function hook to influence target query inputs wiring, this is fully managed now\""]}
  #:user
      {:user-id      [:uuid]
       :name         [:string]
       :email        [:string :unique :index true]
       :picture      [:string]
       :sub          [:string]
       :created-date [:instant]
       :last-seen    [:instant]})

(hf-def/project :hyperfiddle/project
  :code
  (defn filter-controls [needle #_is-regex]
    [:div.form-group #_{:class (when (and @is-regex err) "has-error")}
     [:input.form-control
      {:style         {:display "inline-block"}            ; bootstrap styles are half imported, wonky hacks
       :type          "text"
       :on-change     #(reset! needle (.. % -target -value))
       :auto-complete "off"
       #_#_:auto-focus true                                ; page sroll pos gets lost, otherwise this is great
       :placeholder   "filter"}]
     #_[:span.col-sm-3 [contrib.ui/easy-checkbox-boolean "regex " is-regex]]
     #_(when (and @is-regex err) [:span (ex-message err)])])

  (defn parse-regex [needle]
    (try
      [nil (re-pattern @needle)]
      (catch :default e
        [e #""])))

  (defn fiddle-list [ctx]
    [:div.scroll-wrapper
     [:table.table-hover
      [:tbody
       (->> (hypercrud.browser.context/data ctx)
            (sort (fn [[a b] [a' b']]
                    (if (= b b')
                      (compare (clojure.string/replace (str (:fiddle/ident a)) #"/" ".")
                        (clojure.string/replace (str (:fiddle/ident a')) #"/" "."))
                      (> b b'))
                    ))
            (map (fn [[fiddle tx is-entrypoint]]
                   [:tr {:key (str (hash (:fiddle/ident fiddle)))}
                    [:td
                     (if is-entrypoint
                       (let [route {:hyperfiddle.route/fiddle       :hyperfiddle.ide/edit
                                    :hyperfiddle.route/datomic-args [(:fiddle/ident fiddle)]}]
                         #_[:div (pr-str {:route route})]
                         [hyperfiddle.ui/anchor ctx {:route route} (str (:fiddle/ident fiddle))])
                       (str (:fiddle/ident fiddle)))
                     ]]))
            (doall))]]])

  :css "
    /* Not th – that hits fiddle shortcuts */
    div.hyperfiddle.ui div.hyperfiddle.field.-fiddle-pull,
    div.hyperfiddle.ui div.hyperfiddle.field.-fiddle-query,
    div.hyperfiddle.ui div.hyperfiddle.field.-fiddle-cljs-ns,
    div.hyperfiddle.ui div.hyperfiddle.field.-fiddle-renderer,
    div.hyperfiddle.ui div.hyperfiddle.field.-fiddle-css,
    div.hyperfiddle.ui div.hyperfiddle.field.-fiddle-markdown { display: block !important; }
  ")

(hf-def/attr
  #:project
      {:code {:renderer hyperfiddle.ui.controls/code}
       :css  {:renderer hyperfiddle.ui.controls/css}}

  #:domain
      {:router {:renderer hyperfiddle.ui.controls/code}
       :home-route
               {:renderer
                (let [parse-string (fn [s]
                                     (-> (contrib.reader/read-edn-string! s)
                                         hyperfiddle.route/validate-route+
                                         (cats.monad.either/branch (fn [e] (throw e)) (constantly s))))
                      to-string identity]
                  (fn [val ctx & [props]]
                    (let [props (-> (assoc props
                                      :value val
                                      :on-change (hyperfiddle.ui.util/with-entity-change! ctx)
                                      :mode "clojure"
                                      :linkNumbers false)
                                    (update :mode #(or % "clojure")))]
                      [contrib.ui/debounced props contrib.ui/validated-cmp parse-string to-string contrib.ui.codemirror/-codemirror])))}
       :css                      {:renderer hyperfiddle.ui.controls/css}
       :environment              {:renderer hyperfiddle.ui.controls/code}
       :code                     {:renderer hyperfiddle.ui.controls/code}}

  {:attribute/renderer              {:renderer hyperfiddle.ui.controls/code}
   :database.custom-security/client {:renderer hyperfiddle.ui.controls/code}
   :database.custom-security/server {:renderer hyperfiddle.ui.controls/code}}

  #:fiddle
      {:type     {:renderer
                  (fn [value ctx props]
                    (let [props (assoc props
                                  :class "qe"
                                  :options [{:value :query :label "query"}
                                            {:value :entity :label "pull"}
                                            {:value :blank :label "static"}])]
                      [hyperfiddle.ui.controls/radio-group value ctx props]))}

       :pull     {:renderer
                  (fn [value ctx props]
                    (let [props (assoc props :debounce/interval contrib.ui/default-debounce-ms)]
                      [hyperfiddle.ui.controls/code value ctx props]))}

       :query    {:renderer
                  (fn [value ctx props]
                    (let [props (assoc props :debounce/interval contrib.ui/default-debounce-ms)]
                      [hyperfiddle.ui.controls/code value ctx props]))}

       :cljs-ns  {:renderer
                  (fn [value ctx props]
                    (let [props (assoc props :debounce/interval contrib.ui/default-debounce-ms)]
                      [hyperfiddle.ui.controls/code value ctx props]))}

       :css      {:renderer hyperfiddle.ui.controls/css}

       :markdown {:renderer
                  (fn [value ctx props]
                    (let [props (assoc props :debounce/interval contrib.ui/default-debounce-ms)]
                      [hyperfiddle.ui.controls/markdown-editor value ctx props]))}

       :renderer {:renderer
                  (fn [value ctx props]
                    (let [props (assoc props :debounce/interval contrib.ui/default-debounce-ms)]
                      [hyperfiddle.ui.controls/code value ctx props]))}})

(hf-def/fiddle :hyperfiddle.system/decoding-error []
  :renderer
  (let [[s message data] (:hyperfiddle.route/datomic-args @(:hypercrud.browser/route ctx))]
    [:div
     [:h3 (str "Unable to decode route: " s)]
     [:h4 message]
     [:pre data]]))

(hf-def/fiddle :hyperfiddle.system/not-found []
  :markdown "# Route for url not found")

(hf-def/fiddle :hyperfiddle.system/unauthorized []
  :markdown "## Credentials invalid or stale. Please login again.")
