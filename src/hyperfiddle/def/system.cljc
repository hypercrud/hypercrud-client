(ns hyperfiddle.def.system
  (:require
    [hyperfiddle.def :as hf-def]))

(hf-def/schema
  #:hyperfiddle
      {:project    []
       :owners     [:uuid* "Entity owners uuids, used by ACLs"]
       :starred    [:boolean]
       :archived   [:boolean]
       :deprecated [:boolean]}
  #:attribute
      {:ident    [:keyword :identity]
       :renderer [:string "Default attribute renderer, a CLJS var like `hyperfiddle.ui.controls/code`."]}
  #:fiddle
      {:ident         [:keyword :unique "Fiddle identifier used in URLs. Warning: changing this breaks fiddle URLs."]
       :type          [:keyword "Which Datomic query API"]
       :query         [:string
                       "Datomic query datalog.

                        Warning: no support yet for rules, d/history, d/log or other datomic API access."]
       :pull          [:string "Datomic pull expression for the entity addressed by the URL"]
       :pull-database [:string "Argument to `datomic.api/pull`, defaults to $"]
       :links         [:ref* :isComponent "Links to other fiddles that are available from this fiddle"]
       :renderer      [:string "Reagent expression for the fiddle view"]
       :markdown      [:string "Markdown expression for fiddle view, optional"]
       :hydrate-result-as-fiddle
                      [:boolean "Experimental. When set, data-sync will interpret this fiddle's result as a fiddle - like a higher order fiddle - this is a recursion mechanic."]
       :uuid          [:uuid :identity "For naming anonymous fiddles"]
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
       :email        [:string :identity :index]
       :picture      [:string]
       :sub          [:string]
       :created-date [:instant]
       :last-seen    [:instant]})

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

(hf-def/attr
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
                                      :on-change ((:hyperfiddle.api/view-change! ctx) ctx)
                                      :mode "clojure"
                                      :linkNumbers false)
                                    (update :mode #(or % "clojure")))]
                      [contrib.ui/debounced props contrib.ui/validated-cmp parse-string to-string contrib.ui.codemirror/-codemirror])))}
       :environment              {:renderer hyperfiddle.ui.controls/code}}

  {:attribute/renderer              {:renderer hyperfiddle.ui.controls/code}
   :database.custom-security/client {:renderer hyperfiddle.ui.controls/code}
   :database.custom-security/server {:renderer hyperfiddle.ui.controls/code}})
