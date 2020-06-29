(ns fixtures.domains
  (:require
    [contrib.datomic]
    [contrib.reader]
    [cats.monad.exception :as exception]))


(def fiddles
  {:hyperfiddle.ide/domain
   [{:fiddle/pull-database "$domains",
     :fiddle/pull
     "[:db/id\n :domain/ident\n :domain/environment\n :domain/aliases\n :domain/disable-javascript\n :domain/home-route\n #_ :domain/router\n :domain/code\n :domain/css\n {:domain/databases [:db/id \n                     :domain.database/name \n                     {:domain.database/record [:db/id \n                                               :database/uri\n                                               #_#_#_:hyperfiddle/owners\n                                               {:database/write-security [:db/ident]}\n                                               :database.custom-security/client\n                                               :database.custom-security/server]}]}\n {:domain/fiddle-database [:database/uri\n                           #_#_#_#_:hyperfiddle/owners\n                           {:database/write-security [:db/ident]}\n                           :database.custom-security/client\n                           :database.custom-security/server]}\n :hyperfiddle/owners]",
     :fiddle/ident :hyperfiddle.ide/domain,
     :fiddle/renderer
     "(let [ctx (update ctx :hypercrud.browser/result (partial contrib.reactive/fmap hyperfiddle.foundation/shadow-domain))]\n  [:div props\n   [:h3 \"Environment: \" [hyperfiddle.ui/value [:domain/ident] ctx #(str %)]]\n   [hyperfiddle.ui/field [:domain/ident] ctx hyperfiddle.ide.fiddles.domain/domain-ident-renderer]\n   [hyperfiddle.ui/field [:hyperfiddle/owners] ctx hyperfiddle.ui/hyper-control]\n   [hyperfiddle.ui/field [:domain/databases] ctx\n    (fn [val ctx props]\n      [hyperfiddle.ui/table\n       (fn [ctx]\n         [[hyperfiddle.ui/field [:db/id] ctx \n           (fn [val ctx props]\n             (hyperfiddle.ui/link #{:domain/databases :hf/remove} ctx \"remove\"))]\n          [hyperfiddle.ui/field [:domain.database/name] ctx hyperfiddle.ui/hyper-control]\n          [hyperfiddle.ui/field [:domain.database/record :database/uri] ctx hyperfiddle.ui/hyper-control]])\n       ctx])]\n   [hyperfiddle.ui/field [:domain/fiddle-database] ctx hyperfiddle.ui/hyper-control \n    {:options :database/options-list\n     :option-label (comp :database/uri first)}]\n   [hyperfiddle.ui/field [:domain/aliases] ctx hyperfiddle.ui/hyper-control]\n   [hyperfiddle.ui/field [:domain/disable-javascript] ctx hyperfiddle.ui/hyper-control]\n   [hyperfiddle.ui/field [:domain/home-route] ctx hyperfiddle.ui/hyper-control]\n   [hyperfiddle.ui/field [:domain/environment] ctx hyperfiddle.ui/hyper-control]\n   [:p \"Source code concerns. todo migrate into fiddle-database\"]\n   [hyperfiddle.ui/field [:domain/code] ctx hyperfiddle.ui/hyper-control]\n   [hyperfiddle.ui/field [:domain/css] ctx hyperfiddle.ui/hyper-control]\n   [:p \"Attribute renderers. todo migrate into fiddle-database\"]\n   [hyperfiddle.ui/browse :hyperfiddle.ide/domain-attribute-renderers ctx]])",
     :db/doc "Databases, DNS, API keys, etc",
     :db/id 17592186045564,
     :fiddle/type :entity,
     :fiddle/links
     [{:db/id 17592186061425,
       :link/class [:hf/iframe],
       :link/fiddle
       {:db/id 17592186060438,
        :fiddle/ident
        :hyperfiddle.ide/domain-attribute-renderers,
        :fiddle/query
        "[:find \n [(pull ?e [:db/id :attribute/ident :attribute/renderer]) ...]\n :where [?e :attribute/ident]]",
        :fiddle/type :query},
       :link/path :hyperfiddle.ide/domain}
      {:db/id 17592186061562,
       :link/class [:hf/remove],
       :link/path "$domains :domain/ident"}
      {:db/id 17592186061564,
       :link/class [:hf/remove],
       :link/path "$domains :domain/databases"}
      {:db/id 17592186061567,
       :link/class [:hf/new],
       :link/fiddle
       {:db/id 17592186061548,
        :fiddle/ident :domain.databases/add,
        :fiddle/type :entity},
       :link/path "$domains :domain/databases"}
      {:db/id 17592186061770,
       :link/class [:hf/iframe],
       :link/fiddle
       {:db/id 17592186061550,
        :fiddle/ident :database/options-list,
        :fiddle/query
        "[:find \n (pull $domains ?e [:db/id :database/uri]) \n ?name\n :in $domains \n :where \n [$domains ?e :database/uri ?uri]\n [(str ?uri) ?suri]\n [(.substring ?suri 28) ?name]]",
        :fiddle/type :query},
       :link/path :hyperfiddle.ide/domain}]}
    {:db/id 17592186045517,
     :domain/home-route "[:hyperfiddle/topnav [#entity[\"$\" \"tempid\"]]]",
     :domain/databases
     [{:db/id 17592186046511,
       :domain.database/name "$domains",
       :domain.database/record
       {:db/id 17592186046177, :database/uri #uri "datomic:free://datomic:4334/domains", :database.custom-security/server "hyperfiddle.security.domains/server"}}
      {:db/id 17592186046512,
       :domain.database/name "$users",
       :domain.database/record {:db/id 17592186046100, :database/uri #uri "datomic:free://datomic:4334/hyperfiddle-users"}}
      {:db/id 17592186046513, :domain.database/name "$", :domain.database/record {:db/id 17592186046090, :database/uri #uri "datomic:free://datomic:4334/root"}}],
     :hyperfiddle/owners [#uuid "87108fa3-e7d5-4ed5-a87a-81a6eb6e1aae" #uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39" #uuid "ca192cc8-4ccb-48c6-853f-6fc7dcdd1810"],
     :domain/fiddle-database {:database/uri #uri "datomic:free://datomic:4334/root"},
     :domain/css
     "/* Not th – that hits fiddle shortcuts */\ndiv.hyperfiddle.ui div.hyperfiddle.field.-fiddle-pull,\ndiv.hyperfiddle.ui div.hyperfiddle.field.-fiddle-query,\ndiv.hyperfiddle.ui div.hyperfiddle.field.-fiddle-renderer,\ndiv.hyperfiddle.ui div.hyperfiddle.field.-fiddle-css,\ndiv.hyperfiddle.ui div.hyperfiddle.field.-fiddle-markdown { display: block !important; }",
     :domain/ident "hyperfiddle",
     :domain/environment ""}]

   :database/options-list
   [{:db/id 17592186061550,
     :fiddle/query
     "[:find \n (pull $domains ?e [:db/id :database/uri]) \n ?name\n :in $domains \n :where \n [$domains ?e :database/uri ?uri]\n [(str ?uri) ?suri]\n [(.substring ?suri 28) ?name]]",
     :fiddle/type :query,
     :fiddle/ident :database/options-list}
    [[{:db/id 17592186046713, :database/uri #uri "datomic:free://datomic:4334/mbrainz"} "mbrainz"]
     [{:db/id 17592186046090, :database/uri #uri "datomic:free://datomic:4334/root"} "root"]
     [{:db/id 17592186046093, :database/uri #uri "datomic:free://datomic:4334/hyperfiddle-blog-source"} "hyperfiddle-blog-source"]
     [{:db/id 17592186046097, :database/uri #uri "datomic:free://datomic:4334/sandbox"} "sandbox"]
     [{:db/id 17592186046639, :database/uri #uri "datomic:free://datomic:4334/tank"} "tank"]
     [{:db/id 17592186046092, :database/uri #uri "datomic:free://datomic:4334/dustingetz.com"} "dustingetz.com"]
     [{:db/id 17592186046705, :database/uri #uri "datomic:free://datomic:4334/clojurians-log"} "clojurians-log"]
     [{:db/id 17592186046096, :database/uri #uri "datomic:free://datomic:4334/seattle"} "seattle"]
     [{:db/id 17592186046103, :database/uri #uri "datomic:free://datomic:4334/~dustin.getz"} "~dustin.getz"]]]
   })

(def schema
  (contrib.datomic/indexed-schema
    '({:db/ident :attribute/ident, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "FK to schema, they can't be directly on $ schema because attribute renderers are a \"source code\" concern. TODO: move these off domain and into the fiddle repo."}
       {:db/ident :attribute/renderer, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Default attribute renderer, a CLJS var like `hyperfiddle.ui.controls/code`."}
       {:db/ident :database/custom-write-sec, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :database/uri, :db/valueType {:db/ident :db.type/uri}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "Datomic connection URI"}
       {:db/ident :database/write-security, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :database.custom-security/client, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :database.custom-security/server, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :domain/aliases, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/many}, :db/unique {:db/ident :db.unique/value}, :db/doc "Register production hostname here and point it at the hyperfiddle.net IP. In production, server-side rendering is enabled, auto-transact is always on, and the hyperfiddle toolbar is not served."}
       {:db/ident :domain/code, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Do not use and undocumented. CLJS namespace for storing view functions, evaluated on page load. Todo: clean this up."}
       {:db/ident :domain/css, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "CSS which is always loaded. Todo: expose a way to load foreign assets."}
       {:db/ident :domain/databases, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/isComponent true, :db/doc "Datomic databases available for query from this domain."}
       {:db/ident :domain/disable-javascript, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Don't serve hyperfiddle javascript on aliased domains. Today, this is good for static sites, but in the future may make things slower becasuse it limits our ability to use `Cache-Control: Immutable`. Todo: expose more I/O configuration choices here."}
       {:db/ident :domain/environment, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "EDN map of constants available to your fiddles, for example API keys."}
       {:db/ident :domain/fiddle-database, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Database to store your fiddles. It is probably also assigned a name above, so your fiddles can query it, for example to generate site maps."}
       {:db/ident :domain/home-route, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Index hyperfiddle route like `[:demo/shirt-sizes [#entity[\"$\" :gender/male]]]`, copy it from data mode"}
       {:db/ident :domain/ident, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "Hyperfiddle Cloud subdomain."}
       {:db/ident :domain/router, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Experimental and undocumented userland router definition"}
       {:db/ident :domain.database/name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Name of this database in Datomic query :in clause"}
       {:db/ident :domain.database/record, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
       {:db/ident :fiddle/hydrate-result-as-fiddle, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Experimental flag for higher-order fiddles. When set, data-sync will interpret this fiddle's arguments as a fiddle, which is a recursion mechanic. We're not sure if this is a good idea, but the docs site uses it for embedding examples."}
       {:db/ident :fiddle/ident, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "Fiddle identifier used in default URL router"}
       {:db/ident :fiddle/links, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/isComponent true, :db/doc "Links to other fiddles, used for data-sync, automatic UI and business logic."}
       {:db/ident :fiddle/markdown, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Optional place to store markdown, your :fiddle/renderer may render this."}
       {:db/ident :fiddle/pull, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Datomic pull expression, the pulled entity is passed by URL. Pull :db/id or :db/ident for an editable form."}
       {:db/ident :fiddle/pull-database, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Argument to `datomic.api/pull`"}
       {:db/ident :fiddle/query, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Datomic query. Database inputs are resolved by name through the `:domain/environment`. Pull `:db/id` for editable forms. Currently no support yet for rules, d/history or d/log."}
       {:db/ident :fiddle/renderer, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Reagent expression for the view. Clear to restore default. There are some bugs related to default values, so if Hyperfiddle generates a datoms conflict, just fix it at the stage."}
       {:db/ident :fiddle/type, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Datomic API for data fetching, blank means nothing"}
       {:db/ident :hyperfiddle/owners, :db/valueType {:db/ident :db.type/uuid}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "FK to users who have administrator role on this domain."}
       {:db/ident :link/class, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "App-specific semantic class of the link, like HTML's css classes. Fiddle views and API clients should select links by class with: \n* `hyperfiddle.data/select`\n* `hyperfiddle.data/select-all`\n* `hyperfiddle.data/select-here`\n* `hyperfiddle.data/browse`"}
       {:db/ident :link/fiddle, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Links point to fiddles. Any fiddle dependencies needed for the query or pull are passed by URL and encoded as \"ednish\". Allowed parameters are entity identifiers and scalars."}
       {:db/ident :link/formula, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Deprecated – this is fully managed based on :link/rel. You can override it, but there isn't a good reason to do that anymore."}
       {:db/ident :link/path, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "E.g. `0 :reg/gender :db/ident`. Associates this link with a pulled entity by pull path, accounting for data cardinality. Find element index must be specified only for relation queries."}
       {:db/ident :link/rel, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Set the rel to opt-in to automatic CRUD functionality. The set of rels is open for extension, but there is no admin panel for that yet. The integrated Datomic console's entity navigation and CRUD ensures that these rels exist for every entity pulled. Builtin rels:\n* *:hf/self* – Editor for the entity at a pulled path, accounting for cardinality. There must only be one :self link.\n* *:hf/rel* – Like :self but optional and can have more than one, for linking to related data.\n* *:hf/iframe* – Like :rel but data loads inline with this fiddle, use this for picker options\n* *:hf/new* – Like :rel but manufactures an entity tempid\n* *:hf/affix* – Like :new but affixes the new entity as a child to self.\n* *:hf/remove* – Retracts the entity\n* *:hf/detach* – Retract only the parent-child reference to this entity"}
       {:db/ident :link/tx-fn, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Optional CLJS function which generates a Datomic transaction value. Turns the link into a button which calls the :tx-fn and stages the result. If there is a :link/fiddle, the link will render as a popover and the tx-fn will be called with the popover form's value when it stages. Some rels provide a default tx-fn which you can override. TODO: clean this up."})
    ))

(def schemas
  {"$domains" (exception/success schema)
   "$" (exception/success
         (contrib.datomic/indexed-schema
           [{:db/ident :attribute/ident, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "FK to schema, they can't be directly on $ schema because attribute renderers are a \"source code\" concern. TODO: move these off domain and into the fiddle repo."}
            {:db/ident :attribute/renderer, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Default attribute renderer, a CLJS var like `hyperfiddle.ui.controls/code`."}
            {:db/ident :fiddle/hydrate-result-as-fiddle, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Experimental. When set, data-sync will interpret this fiddle's arguments as a fiddle, which is a recursion mechanic."}
            {:db/ident :fiddle/ident, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "Fiddle identifier used in fiddle URLs.\n\nWarning: changing this breaks fiddle URLs."}
            {:db/ident :fiddle/links, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/isComponent true, :db/doc "Links to other fiddles. Like HTML there are anchors, buttons, and iframes."}
            {:db/ident :fiddle/markdown, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Optional, your :fiddle/renderer may render this."}
            {:db/ident :fiddle/pull, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Datomic pull expression for the entity addressed by the URL"}
            {:db/ident :fiddle/pull-database, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Argument to `datomic.api/pull`, defaults to $"}
            {:db/ident :fiddle/query, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Datomic query datalog. \n\nWarning: no support yet for rules, d/history, d/log or other datomic API access."}
            {:db/ident :fiddle/renderer, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Reagent expression for the view."}
            {:db/ident :fiddle/type, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Which Datomic query API"}
            {:db/ident :fiddle/uuid, :db/valueType {:db/ident :db.type/uuid}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "For creating new fiddles without needing a human to fill in an ident"}
            {:db/ident :hyperfiddle/owners, :db/valueType {:db/ident :db.type/uuid}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "Used in opt-in entity-level ACLs configured through hyperfiddle.net subdomains"}
            {:db/ident :hyperfiddle/starred, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}}
            {:db/ident :link/class, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "Lets fiddle views select a link by name or class, like `<a class=\"\"`"}
            {:db/ident :link/fiddle, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Hyperlink target (like `<a href=\"\">` or `<iframe src=\"\">`)"}
            {:db/ident :link/formula, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Deprecated – this is fully managed now."}
            {:db/ident :link/path, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Location in a Datomic result like `0 :reg/gender :db/ident`. Find-element index should only be specified for Datomic query forms that permit more than one find-element."}
            {:db/ident :link/rel, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "* *:hf/rel*\n* *:hf/new*, *:hf/remove*\n* *:hf/affix*, *:hf/detach*\n* *:hf/iframe*\n* *:hf/self*"}
            {:db/ident :link/tx-fn, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "CLJS function that turns a form submission into a Datomic transaction."}]))})
