(ns fixtures.tank
  (:require
    [cats.monad.exception :as exception]
    [contrib.reader]))


; These can be copy pasted from hyperfiddle gui
; Don't forget the fiddle ontology

(def fiddles
  {:dustingetz/gender-shirtsize
   [{:fiddle/ident :dustingetz/gender-shirtsize
     :fiddle/type :query
     :fiddle/query
     (pr-str
       '[:find
         [(pull ?e [:dustingetz.reg/email
                    :dustingetz.reg/name
                    {:dustingetz.reg/gender [:db/ident]}
                    {:dustingetz.reg/shirt-size [:db/ident]}
                    :db/id])
          ...]
         :where
         [?e :dustingetz.reg/email]])}
    [{:db/id 17592186046196,
      :dustingetz.reg/email "dustin@example.com",
      :dustingetz.reg/name "Dustin Getz",
      :dustingetz.reg/gender {:db/ident :dustingetz.gender/male},
      :dustingetz.reg/shirt-size {:db/ident :dustingetz.shirt-size/mens-large}}
     {:db/id 17592186046763,
      :dustingetz.reg/email "bob@example.com",
      :dustingetz.reg/gender {:db/ident :dustingetz.gender/male},
      :dustingetz.reg/shirt-size {:db/ident :dustingetz.shirt-size/mens-large}}
     #_{:db/id 17592186046764, :dustingetz.reg/email "charlie@example.com", :dustingetz.reg/gender {:db/ident :dustingetz.gender/male}}
     #_{:db/id 17592186046317,
        :dustingetz.reg/email "alice@example.com",
        :dustingetz.reg/name "Alice",
        :dustingetz.reg/gender {:db/ident :dustingetz.gender/female},
        :dustingetz.reg/shirt-size {:db/ident :dustingetz.shirt-size/womens-medium}}
     #_{:db/id 17592186046765,
        :dustingetz.reg/email "elizabeth@example.com",
        :dustingetz.reg/gender {:db/ident :dustingetz.gender/female},
        :dustingetz.reg/shirt-size {:db/ident :dustingetz.shirt-size/womens-medium}}]]

   :tutorial.race/submission
   [{:fiddle/pull
     "[:db/id \n :dustingetz.reg/email\n :dustingetz.reg/name\n :dustingetz.reg/age\n :dustingetz.reg/birthdate\n {:dustingetz.reg/gender [:db/ident]}\n {:dustingetz.reg/shirt-size [:db/ident]}]",
     :fiddle/ident :tutorial.race/submission,
     :fiddle/renderer
     "(let [{:keys [:hypercrud.browser/fiddle]} ctx]\n  [:div.container-fluid props\n   [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]])",
     :hyperfiddle/owners
     [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"],
     :db/id 17592186046198,
     :fiddle/type :entity,
     :fiddle/links
     [{:db/id 17592186046199,
       :link/class [:hf/iframe],
       :link/path :tutorial.race/submission
       :link/fiddle
       {:db/id 17592186046200,
        :fiddle/ident :tutorial.race/genders,
        :fiddle/query
        "[:find\n [(pull ?e [:db/ident]) ...]\n :where\n [?e :db/ident ?i]\n [(namespace ?i) ?ns]\n [(ground \"dustingetz.gender\") ?ns]]",
        :fiddle/type :query},
       :link/rel :hf/iframe}
      {:db/id 17592186046201,
       :link/class [:hf/iframe],
       :link/fiddle
       {:db/id 17592186046202,
        :fiddle/ident :tutorial.race/shirt-sizes,
        :fiddle/query
        "[:find\n [(pull ?e [:db/ident {:dustingetz.reg/gender [:db/ident]}]) ...]\n :in $ ?gender\n :where\n [?e :db/ident ?i]\n [?e :dustingetz.reg/gender ?gender]\n [(namespace ?i) ?ns]\n [(ground \"dustingetz.shirt-size\") ?ns]]",
        :fiddle/type :query},
       :link/path :dustingetz.reg/gender,
       :link/rel :hf/iframe}],
     :fiddle/markdown
     "### Registrant's submission\n\n!field[](:dustingetz.reg/email)\n!field[](:dustingetz.reg/name)\n!field[](:dustingetz.reg/age)\n!field[](:dustingetz.reg/birthdate)\n!field[](:dustingetz.reg/gender){options=\"tutorial.race/genders\" option-label=\":db/ident\"}\n\nWould you like a tee-shirt with that?\n\n!field[](:dustingetz.reg/shirt-size){options=\"tutorial.race/shirt-sizes\" option-label=\":db/ident\"}\n\n!block[Shirt-size options react to gender ☝️]{.alert .alert-info}"}
    {:db/id 17592186046196,
     :dustingetz.reg/email "dustin@example.com",
     :dustingetz.reg/name "Dustin Getz",
     :dustingetz.reg/age 102,
     :dustingetz.reg/birthdate #inst "2018-09-13T00:00:00.000-00:00",
     :dustingetz.reg/gender {:db/ident :dustingetz.gender/male},
     :dustingetz.reg/shirt-size {:db/ident :dustingetz.shirt-size/mens-large}}]

   :tutorial.race/genders
   [[{:db/ident :dustingetz.gender/male} {:db/ident :dustingetz.gender/female}]
    {:db/id 17592186046200,
     :fiddle/ident :tutorial.race/genders,
     :fiddle/query
     "[:find\n [(pull ?e [:db/ident]) ...]\n :where\n [?e :db/ident ?i]\n [(namespace ?i) ?ns]\n [(ground \"dustingetz.gender\") ?ns]]",
     :fiddle/type :query,
     :hyperfiddle/owners
     [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"]}]

   :tutorial.race/shirt-sizes
   [{:db/id 17592186046202,
     :fiddle/ident :tutorial.race/shirt-sizes,
     :fiddle/links
     [{:db/id 17592186046318,
       :link/class [:hf/iframe],
       :link/fiddle
       {:db/id 17592186046200,
        :fiddle/ident :tutorial.race/genders,
        :fiddle/query
        "[:find\n [(pull ?e [:db/ident]) ...]\n :where\n [?e :db/ident ?i]\n [(namespace ?i) ?ns]\n [(ground \"dustingetz.gender\") ?ns]]",
        :fiddle/type :query},
       :link/path :dustingetz.reg/gender,
       :link/rel :hf/iframe}],
     :fiddle/query
     "[:find\n [(pull ?e [:db/ident {:dustingetz.reg/gender [:db/ident]}]) ...]\n :in $ ?gender\n :where\n [?e :db/ident ?i]\n [?e :dustingetz.reg/gender ?gender]\n [(namespace ?i) ?ns]\n [(ground \"dustingetz.shirt-size\") ?ns]]",
     :fiddle/renderer
     "(let [{:keys [:hypercrud.browser/fiddle]} ctx]\n  [:div props\n   [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]\n   [hyperfiddle.ui/table \n    (fn [ctx]\n      [[hyperfiddle.ui/field [:db/ident] ctx hyperfiddle.ui/hyper-control]\n       [hyperfiddle.ui/field [:dustingetz.reg/gender] ctx hyperfiddle.ui/hyper-control\n        {:options :tutorial.race/genders}]]) \n    ctx]])",
     :fiddle/type :query,
     :hyperfiddle/owners
     [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"]}
    [{:db/ident :dustingetz.shirt-size/womens-medium, :dustingetz.reg/gender {:db/ident :dustingetz.gender/female}}
     {:db/ident :dustingetz.shirt-size/womens-small, :dustingetz.reg/gender {:db/ident :dustingetz.gender/female}}
     {:db/ident :dustingetz.shirt-size/womens-large, :dustingetz.reg/gender {:db/ident :dustingetz.gender/female}}]]

   :dustingetz.tutorial/blog
   [{:fiddle/ident :dustingetz.tutorial/blog,
     :hyperfiddle/owners
     [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"],
     :db/id 17592186047369,
     :fiddle/type :query,
     :fiddle/links
     [{:db/id 17592186047370,
       :link/fiddle
       {:db/id 17592186047371,
        :fiddle/ident 'dustingetz.tutorial/view-post,
        :fiddle/type :entity},
       :link/path :dustingetz.post/slug,
       :link/rel :hf/self}
      {:db/id 17592186047372,
       :link/class [:hf/new],
       :link/fiddle
       {:db/id 17592186047373,
        :fiddle/ident 'dustingetz.tutorial.blog/new-post,
        :fiddle/type :entity},
       :link/path :dustingetz.post/slug,
       :link/rel :hf/new,
       :link/tx-fn :user/new-post}],
     :fiddle/query
     "[:find\n (pull ?e [:dustingetz.post/published-date\n           :dustingetz.post/title\n           :dustingetz.post/slug\n           :db/id])\n :where\n [?e :dustingetz.post/slug]]"}
    [[{:db/id 17592186047105,
       :dustingetz.post/published-date #inst "2018-11-21T00:00:00.000-00:00",
       :dustingetz.post/title "large strings and high churn attrs blow out indexes",
       :dustingetz.post/slug :large-strings-and-high-churn-attrs-blow-out-indexes}]
     [{:db/id 17592186047142,
       :dustingetz.post/published-date #inst "2018-11-22T15:57:34.277-00:00",
       :dustingetz.post/title "automatic CRUD links",
       :dustingetz.post/slug :automatic-CRUD-links}]]]

   :hfnet.tank/index
   [{:db/id 17592186045830,
     :fiddle/ident :hfnet.tank/index,
     :fiddle/markdown
     "!block{style=\"margin-bottom: 2em;\"}\n\n# Hyperfiddle Tank\n\nThis is a pastebin like environment for learning Hyperfiddle. \n\n1. Please namespace :idents with your username\n1. Schema changes are allowed\n1. To make a fiddle interactive, change !span[tank]{.i}.hyperfiddle.net to !span[demo]{.i}.hyperfiddle.net \n\n!block{style=\"margin-bottom: 2em;\"}\n\n### Showcase\n* [:clojurians](//demo.hyperfiddle.net/:clojurians/)\n* [:seattle/neighborhoods](//demo.hyperfiddle.net/:seattle!neighborhoods/)\n* [gender-shirtsize](//demo.hyperfiddle.net/:dustingetz!gender-shirtsize/)\n* [:tutorial.blog](//demo.hyperfiddle.net/:tutorial.blog/)\n* [markdown-pipeline](//demo.hyperfiddle.net/:dustingetz!markdown-pipeline/#:hf.src!markdown)\n\n!block{style=\"margin-bottom: 2em;\"}\n",
     :fiddle/query
     "[:find \n (pull ?e [:db/id :fiddle/ident])\n (max ?tx)\n ?entrypoint\n :where\n [?e :fiddle/ident]  \n [(hyperfiddle.query/entrypoint-fiddle? $ ?e) ?entrypoint]\n (not [?e :dustingetz.post/hidden true])\n [?e _ _ ?tx]]",
     :fiddle/renderer
     "(let [show-all (reagent.core/atom false)\n      needle (reagent.core/atom \"\")\n      regex? (reagent.core/atom false)]\n  (fn [val ctx props]\n    (let [[err re] (try [nil (re-pattern @needle)]\n                     (catch :default e\n                       [e #\"\"]))]\n      [:main.container props\n       [hyperfiddle.ui/markdown (-> ctx :hypercrud.browser/fiddle deref :fiddle/markdown) ctx]\n       [:h3 \"Recently modified fiddles\"]\n       #_[contrib.ui/easy-checkbox-boolean \"Show all fiddles? \" show-all]\n       [:div.form-group {:class (when (and @regex? err) \"has-error\")}\n        #_[:label.col-sm-2.control-label {:for \"hyperfiddle/entry-point-needle\"} \"Filter\"]\n        [:input.col-sm-10.form-control\n         {:id \"hyperfiddle/entry-point-needle\"\n          :style {:display \"inline-block\"} ; bootstrap styles are half imported, wonky hacks\n          :type \"text\"\n          :on-change #(reset! needle (.. % -target -value))\n          :auto-complete \"off\"\n          #_#_:auto-focus true  ; page sroll pos gets lost, otherwise this is great\n          :placeholder \"filter\"}]\n        #_[:span.col-sm-3\n           [contrib.ui/easy-checkbox-boolean \"Regex? \" regex?]]]\n       (when (and @regex? err) [:span (ex-message err)])\n\n       [:hr]\n\n       [:div.scroll-wrapper\n        [:table.table-hover\n         [:tbody\n          (->> @(:hypercrud.browser/data ctx)\n               #_((if @show-all identity (partial filter #(-> % :fiddle/ident namespace nil?))))\n               (filter (fn [[fiddle tx]]\n                         (let [s (str (:fiddle/ident fiddle))]\n                           (if @regex?\n                             (re-seq re s)\n                             (cuerdas.core/includes? s @needle)))))\n               (sort (fn [[a b] [a' b']]\n                       (if (= b b') \n                         (compare (clojure.string/replace (str (:fiddle/ident a)) #\"/\" \".\")\n                                  (clojure.string/replace (str (:fiddle/ident a')) #\"/\" \".\"))\n                         (> b b'))\n                       ))\n               (map (fn [[fiddle tx is-entrypoint]]\n                      [:tr {:key (str (hash (:fiddle/ident fiddle)))}\n                       [:td\n                        (if is-entrypoint\n                          [hyperfiddle.ui/anchor ctx {:route [(:fiddle/ident fiddle)]} (str (:fiddle/ident fiddle))]\n                          (str (:fiddle/ident fiddle)))\n                        ]]))\n               (doall))]]]])))",
     :fiddle/type :query,
     :hyperfiddle/owners
     [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"]}
    [[{:db/id 17592186045419, :fiddle/ident :karl.hardenstine/fiddles} 13194139534712 true]
     [{:db/id 17592186045426, :fiddle/ident :khardenstine/fiddles} 13194139534711 true]
     [{:db/id 17592186047604, :fiddle/ident :bdevel/post-show} 13194139536499 false]
     [{:db/id 17592186047615, :fiddle/ident :dustingetz/datomic-pull-tx} 13194139536512 true]
     [{:db/id 17592186047618, :fiddle/ident :fsbl0/app} 13194139536533 true]
     [{:db/id 17592186047663, :fiddle/ident :dustingetz/datomic-find-specs} 13194139536562 true]]]

   :hyperfiddle/ide
   [{:db/id 17592186061847,
     :fiddle/renderer
     "hyperfiddle.ide.fiddles.fiddle-src/fiddle-src-renderer",
     :fiddle/links
     [{:db/id 17592186061848,
       :link/class [:hf/remove],
       :link/rel :hf/remove
       :link/path :fiddle/ident}
      {:db/id 17592186061849,
       :link/class [:hf/remove],
       :link/path :fiddle/links,
       :link/rel :hf/remove}
      {:db/id 17592186061850,
       :link/class [:hf/iframe],
       :link/fiddle
       {:db/id 17592186045605,
        :fiddle/ident
        :hyperfiddle.ide/fiddle-options,
        :fiddle/query
        "[:find (pull ?link [:db/id :fiddle/ident])\n :where (or [?link :fiddle/ident] [?link :fiddle/type])]",
        :fiddle/type :query},
       :link/path :hyperfiddle/ide
       :link/rel :hf/iframe}
      {:db/id 17592186061851,
       :link/class [:hf/new],
       :link/fiddle
       {:db/id 17592186056398,
        :fiddle/ident :hyperfiddle.ide/new-fiddle,
        :fiddle/type :entity},
       :link/path :link/fiddle,
       :link/rel :hf/affix}
      {:db/id 17592186061852,
       :link/class [:hf/new],
       :link/fiddle
       {:db/id 17592186058175,
        :fiddle/ident :hyperfiddle.ide/new-link,
        :fiddle/type :entity},
       :link/path :fiddle/links,
       :link/rel :hf/affix}],
     :fiddle/type :entity,
     :fiddle/pull
     "; synchronized with hyperfiddle.fiddle/browser-pull\n[:db/id\n :fiddle/ident\n {:fiddle/links [:db/id\n                 :link/class\n                 {:link/fiddle [:db/id\n                                :fiddle/ident               ; routing\n                                :fiddle/query               ; validation\n                                :fiddle/type                ; validation\n                                ]}\n                 :link/formula\n                 :link/path\n                 :link/rel\n                 :link/tx-fn]}\n :fiddle/markdown\n :fiddle/pull\n :fiddle/pull-database\n :fiddle/query\n :fiddle/renderer\n :fiddle/type\n :fiddle/hydrate-result-as-fiddle\n *                                                          ; For hyperblog, so we can access :hyperblog.post/title etc from the fiddle renderer\n ]",
     :fiddle/ident :hyperfiddle/ide,
     :hyperfiddle/owners [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"]
     :hyperfiddle/starred true}

    {:db/id 17592186061847,
     :fiddle/renderer "hyperfiddle.ide.fiddles.fiddle-src/fiddle-src-renderer",
     :fiddle/links
     [{:db/id 17592186061848, :link/class [:hf/remove], :link/rel :hf/remove :link/path :fiddle/ident}
      {:db/id 17592186061849, :link/class [:hf/remove], :link/path :fiddle/links, :link/rel :hf/remove}
      {:db/id 17592186061850,
       :link/class [:hf/iframe],
       :link/fiddle
       {:db/id 17592186045605,
        :fiddle/ident :hyperfiddle.ide/fiddle-options,
        :fiddle/query "[:find (pull ?link [:db/id :fiddle/ident])\n :where (or [?link :fiddle/ident] [?link :fiddle/type])]",
        :fiddle/type :query},
       :link/path :hyperfiddle/ide
       :link/rel :hf/iframe}
      {:db/id 17592186061851,
       :link/class [:hf/new],
       :link/fiddle {:db/id 17592186056398, :fiddle/ident :hyperfiddle.ide/new-fiddle, :fiddle/type :entity},
       :link/path :link/fiddle,
       :link/rel :hf/affix}
      {:db/id 17592186061852,
       :link/class [:hf/new],
       :link/fiddle {:db/id 17592186058175, :fiddle/ident :hyperfiddle.ide/new-link, :fiddle/type :entity},
       :link/path :fiddle/links,
       :link/rel :hf/affix}],
     :fiddle/type :entity,
     :fiddle/pull
     "; synchronized with hyperfiddle.fiddle/browser-pull\n[:db/id\n :fiddle/ident\n {:fiddle/links [:db/id\n                 :link/class\n                 {:link/fiddle [:db/id\n                                :fiddle/ident               ; routing\n                                :fiddle/query               ; validation\n                                :fiddle/type                ; validation\n                                ]}\n                 :link/formula\n                 :link/path\n                 :link/rel\n                 :link/tx-fn]}\n :fiddle/markdown\n :fiddle/pull\n :fiddle/pull-database\n :fiddle/query\n :fiddle/renderer\n :fiddle/type\n :fiddle/hydrate-result-as-fiddle\n *                                                          ; For hyperblog, so we can access :hyperblog.post/title etc from the fiddle renderer\n ]",
     :fiddle/ident :hyperfiddle/ide,
     :hyperfiddle/owners [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"]
     :hyperfiddle/starred true}
    ]
   ::new-link1
   [{:db/id 17592186058175,
     :fiddle/renderer
     "(let [{:keys [:hypercrud.browser/fiddle]} ctx]\n  [:div props\n   [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]])",
     :fiddle/type :entity,
     :fiddle/pull "[:link/path\n :db/id]",
     :fiddle/markdown
     "#### new-link\n\n!field(:link/path){placeholder=\":district/region\"}",
     :fiddle/ident :hyperfiddle.ide/new-link}
    {:db/id "hyperfiddle.tempid--2017569654"}]

   ::new-link2
   [{:db/id 17592186058175,
     :fiddle/renderer
     "(let [{:keys [:hypercrud.browser/fiddle]} ctx]\n  [:div props\n   [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]])",
     :fiddle/type :entity,
     :fiddle/pull "[:link/path\n :db/id]",
     :fiddle/markdown
     "#### new-link\n\n!field(:link/path){placeholder=\":district/region\"}",
     :fiddle/ident :hyperfiddle.ide/new-link}
    {:db/id 17592186062067, :link/path :woo}]

   ::test-links-table-validation
   [{:db/id 17592186061847,
     :fiddle/renderer
     "hyperfiddle.ide.fiddles.fiddle-src/fiddle-src-renderer",
     :fiddle/links
     [{:db/id 17592186061848,
       :link/class [:hf/remove]
       :link/path :fiddle/ident}
      {:db/id 17592186061849,
       :link/class [:hf/remove],
       :link/path :fiddle/links}
      {:db/id 17592186061850,
       :link/class [:hf/iframe],
       :link/fiddle
       {:db/id 17592186045605,
        :fiddle/ident
        :hyperfiddle.ide/fiddle-options,
        :fiddle/query
        "[:find [(pull ?link [:db/id :fiddle/ident]) ...]\n #_(pull ?link [:db/id :fiddle/ident])\n :where \n (or [?link :fiddle/ident] \n     [?link :fiddle/type])]",
        :fiddle/type :query},
       :link/path :hyperfiddle/ide}
      {:db/id 17592186061851,
       :link/class [:hf/new],
       :link/fiddle
       {:db/id 17592186056398,
        :fiddle/ident :hyperfiddle.ide/new-fiddle,
        :fiddle/type :entity},
       :link/path :link/fiddle}
      {:db/id 17592186061852,
       :link/class [:hf/new],
       :link/fiddle
       {:db/id 17592186058175,
        :fiddle/ident :hyperfiddle.ide/new-link,
        :fiddle/type :entity},
       :link/path :fiddle/links}],
     :fiddle/type :entity,
     :fiddle/pull
     "; synchronized with hyperfiddle.fiddle/browser-pull\n[:db/id\n :fiddle/ident\n {:fiddle/links [:db/id\n                 :link/class\n                 {:link/fiddle [:db/id\n                                :fiddle/ident               ; routing\n                                :fiddle/query               ; validation\n                                :fiddle/type                ; validation\n                                ]}\n                 :link/formula\n                 :link/path\n                 :link/rel\n                 :link/tx-fn]}\n :fiddle/markdown\n :fiddle/pull\n :fiddle/pull-database\n :fiddle/query\n :fiddle/renderer\n :fiddle/type\n :fiddle/hydrate-result-as-fiddle\n *                                                          ; For hyperblog, so we can access :hyperblog.post/title etc from the fiddle renderer\n ]",
     :fiddle/ident :hyperfiddle/ide,
     :hyperfiddle/starred true}

    {:db/id 17592186061847,
     :fiddle/renderer
     "hyperfiddle.ide.fiddles.fiddle-src/fiddle-src-renderer",
     :fiddle/links
     [{:db/id 17592186061848,
       :link/class [:hf/remove]
       :link/path :fiddle/ident}
      {:db/id 17592186061849,
       :link/class [:hf/remove],
       :link/path :fiddle/links}
      {:db/id 17592186061850,
       :link/class [:hf/iframe],
       :link/fiddle
       {:db/id 17592186045605,
        :fiddle/ident
        :hyperfiddle.ide/fiddle-options,
        :fiddle/query
        "[:find [(pull ?link [:db/id :fiddle/ident]) ...]\n #_(pull ?link [:db/id :fiddle/ident])\n :where \n (or [?link :fiddle/ident] \n     [?link :fiddle/type])]",
        :fiddle/type :query},
       :link/path :hyperfiddle/ide}
      {:db/id 17592186061851,
       :link/class [:hf/new],
       :link/fiddle
       {:db/id 17592186056398,
        :fiddle/ident :hyperfiddle.ide/new-fiddle,
        :fiddle/type :entity},
       :link/path :link/fiddle}
      {:db/id 17592186061852,
       :link/class [:hf/new],
       :link/fiddle
       {:db/id 17592186058175,
        :fiddle/ident :hyperfiddle.ide/new-link,
        :fiddle/type :entity},
       :link/path :fiddle/links}],
     :fiddle/type :entity,
     :fiddle/pull
     "; synchronized with hyperfiddle.fiddle/browser-pull\n[:db/id\n :fiddle/ident\n {:fiddle/links [:db/id\n                 :link/class\n                 {:link/fiddle [:db/id\n                                :fiddle/ident               ; routing\n                                :fiddle/query               ; validation\n                                :fiddle/type                ; validation\n                                ]}\n                 :link/formula\n                 :link/path\n                 :link/rel\n                 :link/tx-fn]}\n :fiddle/markdown\n :fiddle/pull\n :fiddle/pull-database\n :fiddle/query\n :fiddle/renderer\n :fiddle/type\n :fiddle/hydrate-result-as-fiddle\n *                                                          ; For hyperblog, so we can access :hyperblog.post/title etc from the fiddle renderer\n ]",
     :fiddle/ident :hyperfiddle/ide,
     :hyperfiddle/starred true}]


   :seattle/neighborhoods
   [{:db/id 17592186045743,
     :fiddle/ident :seattle/neighborhoods,
     :fiddle/links
     [{:db/id 17592186045747,
       :link/class [:hf/iframe],
       :link/fiddle
       {:db/id 17592186045750,
        :fiddle/ident :seattle/districts,
        :fiddle/query
        "[:find \n [(pull ?e [:district/name :db/id]) ...]\n :where \n [?e :district/name]]",
        :fiddle/type :query},
       :link/path :seattle/neighborhoods,
       :link/rel :hf/iframe}
      {:db/id 17592186045752,
       :link/class [:hf/new],
       :link/fiddle
       {:db/id 17592186045754,
        :fiddle/ident :seattle/neighborhood-new,
        :fiddle/type :entity},
       :link/path :seattle/neighborhoods,
       :link/rel :hf/new}
      {:db/id 17592186045756,
       :link/fiddle
       {:db/id 17592186045758,
        :fiddle/ident :seattle/neighborhood-edit,
        :fiddle/type :entity},
       :link/path :seattle/neighborhoods,
       :link/rel :hf/self}
      {:db/id 17592186045760,
       :link/fiddle
       {:db/id 17592186045762,
        :fiddle/ident :seattle/district,
        :fiddle/type :entity},
       :link/path :district/region,
       :link/rel :hf/self}],
     :fiddle/query
     "[:find \n [(pull ?e [:neighborhood/name \n            {:neighborhood/district \n             [:db/id \n              :district/name\n              {:district/region \n               [:db/ident]}]}\n            :db/id\n            :hyperfiddle/owners]) ...]\n :where \n [?e :neighborhood/name ?name]\n [(< ?name \"C\")]]",
     :fiddle/renderer
     "(let [{:keys [:hypercrud.browser/fiddle]} ctx]\n  [:div.container-fluid props\n   [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]\n   [hyperfiddle.ui/table\n    (fn [ctx] \n      [(hyperfiddle.ui/field [:neighborhood/name] ctx hyperfiddle.ui/hyper-control)\n       (hyperfiddle.ui/field [:neighborhood/district] ctx hyperfiddle.ui/hyper-control\n                             {:options :seattle/districts\n                              :option-label :district/name})\n       (hyperfiddle.ui/field [:neighborhood/district :district/region] ctx \n                             hyperfiddle.ui/hyper-control)\n       (hyperfiddle.ui/field [:db/id] ctx hyperfiddle.ui/hyper-control)])\n    ctx]])",
     :fiddle/type :query,
     :hyperfiddle/owners
     [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"]}

    [{:db/id 17592186045522,
      :neighborhood/name "Admiral (West Seattle)",
      :neighborhood/district {:db/id 17592186045521, :district/name "Southwest", :district/region {:db/ident :region/sw}}}
     nil                                                   ; sneaky nil, can happen in sparse cases (this one is injected manually)
     {:db/id 17592186045524,
      :neighborhood/name "Alki",
      :neighborhood/district {:db/id 17592186045521, :district/name "Southwest", :district/region {:db/ident :region/sw}}}
     {:db/id 17592186045528,
      :neighborhood/name "Belltown",
      :neighborhood/district {:db/id 17592186045527, :district/name "Downtown", :district/region {:db/ident :region/w}}}
     {:db/id 17592186045564,
      :neighborhood/name "Broadview",
      :neighborhood/district {:db/id 17592186045563, :district/name "Northwest", :district/region {:db/ident :region/sw}}}
     {:db/id 17592186045551,
      :neighborhood/name "Beacon Hill",
      :neighborhood/district {:db/id 17592186045530, :district/name "Greater Duwamish", :district/region {:db/ident :region/s}}}
     {:db/id 17592186045536,
      :neighborhood/name "Ballard",
      :neighborhood/district {:db/id 17592186045535, :district/name "Ballard", :district/region {:db/ident :region/nw}}}]]

   :seattle/districts
   [{:db/id 17592186045750,
     :fiddle/ident :seattle/districts,
     :fiddle/query
     "[:find \n [(pull ?e [:district/name :db/id]) ...]\n :where \n [?e :district/name]]",
     :fiddle/type :query,
     :hyperfiddle/owners
     [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"]}
    [{:db/id 17592186045538, :district/name "Northeast"}
     {:db/id 17592186045570, :district/name "Central"}
     {:db/id 17592186045643, :district/name "North"}
     {:db/id 17592186045518, :district/name "East"}
     {:db/id 17592186045521, :district/name "Southwest"}
     {:db/id 17592186045586, :district/name "Delridge"}
     {:db/id 17592186045620, :district/name "Magnolia/Queen Anne"}
     {:db/id 17592186045557, :district/name "Southeast"}
     {:db/id 17592186045527, :district/name "Downtown"}
     {:db/id 17592186045530, :district/name "Greater Duwamish"}
     {:db/id 17592186045594, :district/name "Lake Union"}
     {:db/id 17592186045563, :district/name "Northwest"}
     {:db/id 17592186045535, :district/name "Ballard"}]]

   :dustingetz.seattle/communities
   [{:db/id 17592186047478,
     :fiddle/ident :dustingetz.seattle/communities,
     :fiddle/links
     [{:db/id 17592186047481,
       :link/class [:hf/iframe],
       :link/fiddle
       {:db/id 17592186045750,
        :fiddle/ident :seattle/districts,
        :fiddle/query
        "[:find \n [(pull ?e [:district/name :db/id]) ...]\n :where \n [?e :district/name]]",
        :fiddle/type :query},
       :link/path :district/region}],
     :fiddle/query
     "[:find \n [(pull ?e [*\n            \n            {:community/type [:db/ident]\n             :community/orgtype [:db/ident]\n             :community/neighborhood \n             [:neighborhood/name \n              {:neighborhood/district \n               [:db/id \n                :district/name\n                {:district/region \n                 [:db/ident]}]}\n              :db/id\n              :hyperfiddle/owners]}])\n  ...]\n :where \n [?e :community/name ?name]\n [(< ?name \"B\")]]\n\n",
     :fiddle/type :query,
     :hyperfiddle/owners
     [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"]}
    [{:db/id 17592186045537,
      :community/name "At Large in Ballard",
      :community/url "http://blog.seattlepi.com/ballard/",
      :community/neighborhood
      {:db/id 17592186045536,
       :neighborhood/name "Ballard",
       :neighborhood/district {:db/id 17592186045535, :district/name "Ballard", :district/region {:db/ident :region/nw}}},
      :community/category ["human interest" "news"],
      :community/orgtype {:db/ident :community.orgtype/commercial},
      :community/type [{:db/ident :community.type/blog}]}
     {:db/id 17592186045523,
      :community/name "Admiral Neighborhood Association",
      :community/url "http://groups.yahoo.com/group/AdmiralNeighborhood/",
      :community/neighborhood
      {:db/id 17592186045522,
       :neighborhood/name "Admiral (West Seattle)",
       :neighborhood/district {:db/id 17592186045521, :district/name "Southwest", :district/region {:db/ident :region/sw}}},
      :community/category ["neighborhood association"],
      :community/orgtype {:db/ident :community.orgtype/community},
      :community/type [{:db/ident :community.type/email-list}]}
     {:db/id 17592186045540,
      :community/name "Aurora Seattle",
      :community/url "http://www.auroraseattle.com/",
      :community/neighborhood
      {:db/id 17592186045539,
       :neighborhood/name "Regional Sites",
       :neighborhood/district {:db/id 17592186045538, :district/name "Northeast", :district/region {:db/ident :region/ne}}},
      :community/category ["news" "planning" "traffic"],
      :community/orgtype {:db/ident :community.orgtype/community},
      :community/type [{:db/ident :community.type/blog}]}
     {:db/id 17592186045525,
      :community/name "Alki News",
      :community/url "http://groups.yahoo.com/group/alkibeachcommunity/",
      :community/neighborhood
      {:db/id 17592186045524,
       :neighborhood/name "Alki",
       :neighborhood/district {:db/id 17592186045521, :district/name "Southwest", :district/region {:db/ident :region/sw}}},
      :community/category ["members of the Alki Community Council and residents of the Alki Beach neighborhood"],
      :community/orgtype {:db/ident :community.orgtype/community},
      :community/type [{:db/ident :community.type/email-list}]}
     {:db/id 17592186045526,
      :community/name "Alki News/Alki Community Council",
      :community/url "http://alkinews.wordpress.com/",
      :community/neighborhood
      {:db/id 17592186045524,
       :neighborhood/name "Alki",
       :neighborhood/district {:db/id 17592186045521, :district/name "Southwest", :district/region {:db/ident :region/sw}}},
      :community/category ["council meetings" "news"],
      :community/orgtype {:db/ident :community.orgtype/community},
      :community/type [{:db/ident :community.type/blog}]}
     {:db/id 17592186045529,
      :community/name "All About Belltown",
      :community/url "http://www.belltown.org/",
      :community/neighborhood
      {:db/id 17592186045528,
       :neighborhood/name "Belltown",
       :neighborhood/district {:db/id 17592186045527, :district/name "Downtown", :district/region {:db/ident :region/w}}},
      :community/category ["community council"],
      :community/orgtype {:db/ident :community.orgtype/community},
      :community/type [{:db/ident :community.type/website}]}
     {:db/id 17592186045532,
      :community/name "All About South Park",
      :community/url "http://www.allaboutsouthpark.com/",
      :community/neighborhood
      {:db/id 17592186045531,
       :neighborhood/name "South Park",
       :neighborhood/district {:db/id 17592186045530, :district/name "Greater Duwamish", :district/region {:db/ident :region/s}}},
      :community/category ["neighborhood info"],
      :community/orgtype {:db/ident :community.orgtype/community},
      :community/type [{:db/ident :community.type/website}]}
     {:db/id 17592186045534,
      :community/name "ArtsWest",
      :community/url "http://www.artswest.org/?q=node/28",
      :community/neighborhood
      {:db/id 17592186045533,
       :neighborhood/name "West Seattle",
       :neighborhood/district {:db/id 17592186045521, :district/name "Southwest", :district/region {:db/ident :region/sw}}},
      :community/category ["arts"],
      :community/orgtype {:db/ident :community.orgtype/community},
      :community/type [{:db/ident :community.type/website}]}
     {:db/id 17592186045520,
      :community/name "15th Ave Community",
      :community/url "http://groups.yahoo.com/group/15thAve_Community/",
      :community/neighborhood
      {:db/id 17592186045519,
       :neighborhood/name "Capitol Hill",
       :neighborhood/district {:db/id 17592186045518, :district/name "East", :district/region {:db/ident :region/e}}},
      :community/category ["15th avenue residents"],
      :community/orgtype {:db/ident :community.orgtype/community},
      :community/type [{:db/ident :community.type/email-list}]}]]

   :dustingetz/counter
   [{:fiddle/ident :dustingetz/counter,
     :fiddle/renderer
     "(let [{:keys [:hypercrud.browser/fiddle]} ctx]\n  [:div.container-fluid props\n   [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]\n   [hyperfiddle.ui/result val ctx {}]\n   [:hr]\n   [:label \"Age: \" (:dustingetz.reg/age val)]\n   [:p]\n\n   ; Right way to transact, needs some work\n   [hyperfiddle.ui/link :user/inc ctx \"inc\"]\n\n   ; Wrong way to transact\n   #_[:button\n      {:on-click\n       #(->> (let [n (:dustingetz.reg/age val)\n                   e [:dustingetz.reg/email \"dustin@example.com\"]]\n               [[:db.fn/cas e :dustingetz.reg/age n (inc n)]])\n             (hyperfiddle.ui.util/with-tx! ctx \"$\"))}\n      \"Inc!\"]])",
     :hyperfiddle/owners
     [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"],
     :db/id 17592186046889,
     :fiddle/type :query,
     :fiddle/links
     [{:db/id 17592186047243,
       :link/path :dustingetz.reg/age,
       :link/rel :hf/rel,
       :link/tx-fn :user/inc}],
     :fiddle/markdown
     "### :dustingetz/counter\n\nExample of incrementing a value in Datomic (Make sure auto-transact is on!)\n",
     :fiddle/query
     "[:find\n (pull ?e [:db/id \n           :dustingetz.reg/email \n           :dustingetz.reg/age]) \n .\n :where\n [?e :dustingetz.reg/email \"dustin@example.com\"]]"}

    {:db/id 17592186046196, :dustingetz.reg/email "dustin@example.com", :dustingetz.reg/age 102}]

   :dustingetz/slack-storm
   [{:fiddle/ident :dustingetz/slack-storm,
     :fiddle/renderer
     "(let [{:keys [:hypercrud.browser/fiddle]} ctx]\n  [:main.container props\n   [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]\n   [:h3 [hyperfiddle.ui/link :dustingetz.storm/new-storm ctx \"new storm\"]]\n   [:p]\n   [:p \"Recent storms\"]\n   [:p]\n   (->> val\n        (sort-by second) reverse      ;(sort-by (comp #(compare %2 %1) second))\n        (map (fn [[post _ :as row]]\n               [:<> {:key (str (:db/id post))}\n                [:dt (:dustingetz.storm/channel post) ]\n                [:dd [user/render-post-link ctx row]]]))\n        (into [:dl]))])",
     :hyperfiddle/owners
     [#uuid "5b0dd2d7-24a4-4122-bd8e-168817f2e0e7"],
     :db/id 17592186046327,
     :fiddle/type :query,
     :fiddle/links
     [{:db/id 17592186047088,
       :link/fiddle
       {:db/id 17592186047002,
        :fiddle/ident :dustingetz.storm/view,
        :fiddle/type :entity},
       :link/path :dustingetz.post/slug,
       :link/rel :hf/rel}
      {:db/id 17592186047120,
       :link/fiddle
       {:db/id 17592186047089,
        :fiddle/ident :dustingetz.storm/new-storm,
        :fiddle/type :entity},
       :link/formula
       "(constantly (hyperfiddle.api/tempid-detached \"$\" ctx))",
       :link/path :dustingetz/slack-storm,
       :link/rel :hf/self}],
     :fiddle/markdown
     "# Slack Storm\n\n!span[Create documentation from Slack logs]{.lead}",
     :hyperfiddle/starred true,
     :fiddle/query
     "[:find\n (pull ?e [:db/id\n           :dustingetz.post/title\n           :dustingetz.post/slug\n           :dustingetz.storm/channel\n           :dustingetz.post/published-date])\n (max ?tx)\n :where\n [?e :dustingetz.slack-converter/raw-source _ ?tx]\n (not [?e :dustingetz.post/hidden true])\n #_[?e :hyperfiddle/owners]] ; owned by me?"}

    [[{:db/id 17592186047000,
       :dustingetz.post/title "is js/console.log syntax future proof?",
       :dustingetz.post/slug :asdf,
       :dustingetz.storm/channel "#clojurescript",
       :dustingetz.post/published-date #inst "2018-11-19T00:00:00.000-00:00"}
      13194139535895]
     [{:db/id 17592186047105,
       :dustingetz.post/title "large strings and high churn attrs blow out indexes",
       :dustingetz.storm/channel "#datomic",
       :dustingetz.post/published-date #inst "2018-11-21T00:00:00.000-00:00"}
      13194139536000]]]

   :cookbook/markdown-table
   [{:db/id 17592186046741,
     :fiddle/ident :cookbook/markdown-table,
     :fiddle/markdown
     "### table with body renderers\n\ntable:\n:::\n!field(:task/completed) \n!field[hyperfiddle.ui.controls/tristate-boolean](:task/completed)\n!field[hyperfiddle.ui.controls/edn](:task/completed)\n!field[(comp str identity)](:task/title)\n:::",
     :fiddle/query
     "[:find \n [(pull ?e [:db/id \n           :task/title \n           :task/completed]) ...] \n :where\n [?e :task/title]]",
     :fiddle/renderer
     "(let [{:keys [:hypercrud.browser/fiddle]} ctx]\n  [:div.container-fluid props\n   [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]])",
     :fiddle/type :query,
     :hyperfiddle/owners
     [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"]}
    [{:db/id 17592186046744, :task/title "Feed baby", :task/completed true}
     {:db/id 17592186046745, :task/title "Mow the lawn"}
     {:db/id 17592186046746, :task/title "Do the dishes", :task/completed true}]]

   :dustingetz.test/findcoll-ident-cardinality-many
   [{:db/id 17592186047485,
     :fiddle/ident :dustingetz/issue-868,
     :fiddle/query
     "[:find\n [(pull ?e [:db/id #_{:db/cardinality [:db/ident]}])  ...]\n :where\n [:db.part/db :db.install/attribute ?e]\n [?e :db/cardinality :db.cardinality/many]]",
     :fiddle/type :query,
     :hyperfiddle/owners
     [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"]}
    [{:db/ident :db.install/partition}
     {:db/ident :db.install/valueType}
     {:db/ident :db.install/attribute}
     {:db/ident :db.install/function}
     {:db/ident :db.excise/attrs}
     {:db/ident :db.alter/attribute}
     {:db/ident :fiddle/links}
     {:db/ident :link/class}
     {:db/ident :hyperfiddle/owners}
     {:db/ident :community/category}
     {:db/ident :community/type}
     {:db/ident :dustingetz.nav/children}
     {:db/ident :dustingetz.post/related}]]

   :dustingetz.test/schema-ident-findcoll
   [{:db/id 17592186045743,
     :fiddle/ident :seattle/neighborhoods,
     :fiddle/links
     [{:db/id 17592186045747,
       :link/class [:hf/iframe],
       :link/fiddle
       {:db/id 17592186045750,
        :fiddle/ident :seattle/districts,
        :fiddle/query
        "[:find \n [(pull ?e [:district/name :db/id]) ...]\n :where \n [?e :district/name]]",
        :fiddle/type :query},
       :link/path :seattle/neighborhoods}
      {:db/id 17592186045752,
       :link/class [:hf/new],
       :link/fiddle
       {:db/id 17592186045754,
        :fiddle/ident :seattle/neighborhood-new,
        :fiddle/type :entity},
       :link/path :neighborhood/name}
      {:db/id 17592186045756,
       :link/fiddle
       {:db/id 17592186045758,
        :fiddle/ident :seattle/neighborhood-edit,
        :fiddle/type :entity},
       :link/path :neighborhood/name}
      {:db/id 17592186045760,
       :link/fiddle
       {:db/id 17592186045762,
        :fiddle/ident :seattle/district,
        :fiddle/type :entity},
       :link/path :district/region}],
     :fiddle/query
     "[:find\n [(pull ?e [:db/ident]) ...]\n :where\n [:db.part/db :db.install/attribute ?e]]",
     :fiddle/renderer
     "(let [{:keys [:hypercrud.browser/fiddle]} ctx]\n  [:div.container-fluid props\n   [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]\n   [hyperfiddle.ui/table\n    (fn [ctx] \n      [(hyperfiddle.ui/field [:neighborhood/name] ctx hyperfiddle.ui/hyper-control)\n       (hyperfiddle.ui/field [:neighborhood/district] ctx hyperfiddle.ui/hyper-control\n                             {:options :seattle/districts\n                              :option-label :district/name})\n       (hyperfiddle.ui/field [:neighborhood/district :district/region] ctx \n                             hyperfiddle.ui/hyper-control)\n       #_(hyperfiddle.ui/field [:db/id] ctx hyperfiddle.ui/hyper-control)])\n    ctx]])",
     :fiddle/type :query,
     :hyperfiddle/owners
     [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"]}
    [{:db/ident :db.sys/partiallyIndexed}
     {:db/ident :db.sys/reId}
     {:db/ident :db/ident}
     {:db/ident :db.install/partition}
     {:db/ident :db.install/valueType}
     {:db/ident :db.install/attribute}
     {:db/ident :db.install/function}
     {:db/ident :db/excise}
     {:db/ident :db.excise/attrs}
     {:db/ident :db.excise/beforeT}
     {:db/ident :db.excise/before}
     {:db/ident :db.alter/attribute}
     {:db/ident :fressian/tag}
     {:db/ident :db/valueType}
     {:db/ident :db/cardinality}
     {:db/ident :db/unique}
     {:db/ident :db/isComponent}
     {:db/ident :db/index}
     {:db/ident :db/noHistory}
     {:db/ident :db/lang}
     {:db/ident :db/code}
     {:db/ident :db/txInstant}
     {:db/ident :db/fulltext}
     {:db/ident :db/fn}
     {:db/ident :db/doc}
     {:db/ident :attribute/ident}
     {:db/ident :attribute/renderer}
     {:db/ident :fiddle/hydrate-result-as-fiddle}
     {:db/ident :fiddle/ident}
     {:db/ident :fiddle/links}
     {:db/ident :fiddle/markdown}
     {:db/ident :fiddle/pull}
     {:db/ident :fiddle/pull-database}
     {:db/ident :fiddle/query}
     {:db/ident :fiddle/renderer}
     {:db/ident :zzz.fiddle/request}
     {:db/ident :fiddle/type}
     {:db/ident :link/class}
     {:db/ident :link/fiddle}
     {:db/ident :link/formula}
     {:db/ident :link/path}
     {:db/ident :link/rel}
     {:db/ident :link/tx-fn}
     {:db/ident :hyperfiddle/owners}
     {:db/ident :community/name}
     {:db/ident :community/url}
     {:db/ident :community/neighborhood}
     {:db/ident :community/category} {:db/ident :community/orgtype}
     {:db/ident :community/type} {:db/ident :neighborhood/name} {:db/ident :neighborhood/district}
     {:db/ident :district/name} {:db/ident :district/region} {:db/ident :helloworld/email}
     {:db/ident :dustingetz.reg/email} {:db/ident :dustingetz.reg/name} {:db/ident :dustingetz.reg/age}
     {:db/ident :dustingetz.reg/birthdate} {:db/ident :dustingetz.reg/gender} {:db/ident :dustingetz.reg/shirt-size} {:db/ident :dustingetz.post/title} {:db/ident :dustingetz.post/sort-index} {:db/ident :dustingetz.post/published} {:db/ident :dustingetz.post/hidden} {:db/ident :dustingetz.post/draft-placeholder} {:db/ident :dustingetz.nav/children} {:db/ident :dustingetz.post/related} {:db/ident :dustingetz.post/published-date} {:db/ident :dustingetz.task/completed} {:db/ident :dustingetz.task/title} {:db/ident :dustingetz.slack-converter/raw-source} {:db/ident :dustingetz/next} {:db/ident :dustingetz/prev} {:db/ident :dustingetz.storm/ident} {:db/ident :dustingetz.post/slug} {:db/ident :dustingetz.storm/channel} {:db/ident :dustingetz.streaker/weight} {:db/ident :dustingetz.streaker/date} {:db/ident :dustingetz.streaker/miles-ran} {:db/ident :dustingetz.streaker/comment} {:db/ident :dustingetz.streaker/mood} {:db/ident :dustingetz.streaker/teatotal} {:db/ident :dustingetz.streaker/eating} {:db/ident :dustingetz.streaker/pt} {:db/ident :dustingetz.streaker/gym} {:db/ident :dustingetz.streaker/yoga} {:db/ident :dustingetz.streaker.lifting/bench-1rm} {:db/ident :dustingetz.streaker.lifting/squat-1rm} {:db/ident :dustingetz.streaker/food-log} {:db/ident :didier/team-name} {:db/ident :dustingetz/foo} {:db/ident :dustingetz/foo1} {:db/ident :fiddle/uuid} {:db/ident :hyperfiddle/starred} {:db/ident :dustingetz.gist/src-clojure} {:db/ident :dustingetz.task/parent}
     ]
    ]
   :dustingetz.seattle/communities2
   [{:db/id 17592186047512,
     :fiddle/ident :dustingetz.seattle/communities2,
     :fiddle/query
     "[:find\n (pull ?e [:db/id *])\n :where\n [?e :community/name ?name]\n [(< ?name \"A\")]]",
     :fiddle/type :query}
    [[{:db/id 17592186045520,
       :community/name "15th Ave Community",
       :community/url "http://groups.yahoo.com/group/15thAve_Community/",
       :community/neighborhood {:db/id 17592186045519},
       :community/category ["15th avenue residents"],
       :community/orgtype {:db/id 17592186045432},
       :community/type [{:db/id 17592186045436}]}]]]

   :foo/clojurians
   [{:db/id 17592186048012,
     :fiddle/ident :foo/clojurians,
     :fiddle/links
     [{:db/id 17592186048013,
       :link/fiddle
       {:db/id 17592186048014,
        :fiddle/ident :foo.clojurians/user-profile,
        :fiddle/type :entity},
       :link/path :channel/creator}
      {:db/id 17592186048015,
       :link/fiddle
       {:db/id 17592186048016,
        :fiddle/ident :foo.clojurians/channel,
        :fiddle/type :entity},
       :link/path :channel/name}],
     :fiddle/query
     "[:in $clojurians\n :find\n (count ?msg)\n (pull $clojurians ?chan \n       [:channel/name\n        {:channel/creator \n         [:user/name]}])\n :where\n [$clojurians ?msg :message/channel ?chan]\n [$clojurians ?chan :channel/name ?cname]\n [(< ?cname \"d\")]]",
     :fiddle/type :query}

    [[430 #:channel{:name "clojurescript", :creator #:user{:name "gjnoonan"}}]
     [41 #:channel{:name "cljsrn", :creator #:user{:name "mfikes"}}]
     [3 #:channel{:name "clojars", :creator #:user{:name "tcrawley"}}]
     [1252 #:channel{:name "clojure", :creator #:user{:name "gjnoonan"}}]
     [69 #:channel{:name "community-development", :creator #:user{:name "gjnoonan"}}]
     [1 #:channel{:name "clojure-brasil", :creator #:user{:name "pbalduino"}}]
     [1 #:channel{:name "clojure-china", :creator #:user{:name "yusup"}}]
     [26 #:channel{:name "core-async", :creator #:user{:name "warn4n"}}]
     [102 #:channel{:name "clojure-dev", :creator #:user{:name "alexmiller"}}]
     [146 #:channel{:name "cursive", :creator #:user{:name "pupeno"}}]
     [57 #:channel{:name "clojure-greece", :creator #:user{:name "gtsiftsis"}}]
     [1 #:channel{:name "clojure-india", :creator #:user{:name "pastafari"}}]
     [134 #:channel{:name "clojure-italy", :creator #:user{:name "reborg"}}]
     [3 #:channel{:name "aleph", :creator #:user{:name "pmagnus"}}]
     [115 #:channel{:name "arachne", :creator #:user{:name "surreal.analysis"}}]
     [30 #:channel{:name "clojure-nl", :creator #:user{:name "borkdude"}}]
     [3 #:channel{:name "architecture", :creator #:user{:name "greywolve"}}]
     [1 #:channel{:name "aws", :creator #:user{:name "alandipert"}}]
     [2 #:channel{:name "bangalore-clj", :creator #:user{:name "kumarshantanu"}}]
     [20 #:channel{:name "clojure-russia", :creator #:user{:name "nicola"}}]
     [907 #:channel{:name "beginners", :creator #:user{:name "arathunku"}}]
     [8 #:channel{:name "clojure-sanfrancisco", :creator #:user{:name "paul.legato"}}]
     [198 #:channel{:name "boot", :creator #:user{:name "martinklepsch"}}]
     [215 #:channel{:name "clojure-spec", :creator #:user{:name "martintrojer"}}]
     [4 #:channel{:name "bristol-clojurians", :creator #:user{:name "msgodf"}}]
     [216 #:channel{:name "clojure-uk", :creator #:user{:name "tiltyard"}}]
     [212 #:channel{:name "cider", :creator #:user{:name "bozhidar"}}]
     [60 #:channel{:name "clara", :creator #:user{:name "devn"}}]
     [146 #:channel{:name "cljs-dev", :creator #:user{:name "dnolen"}}]]]

   :foo.clojurians/channel
   [{:db/id 17592186048016,
     :fiddle/ident :foo.clojurians/channel,
     :fiddle/pull
     "[:db/id\n :channel/name\n :channel/created\n {(:message/_channel :limit 11) \n  [:message/text\n   {:message/user [:user/name]}]}]",
     :fiddle/pull-database "$clojurians",
     :fiddle/type :entity}
    {:db/id 17592186046158,
     :channel/name "clojurescript",
     :channel/created 1425233874,
     :message/_channel
     [#:message{:text
                "<@U051SS2EU> that's right, so I misinterpreted it and actually the doc does not mention that. So I was using it with `into` and everything was (of course) stuck :wink:",
                :user #:user{:name "richiardiandrea"}}
      #:message{:text "<@U0C8489U6> I dug up the convo <https://dev.clojure.org/jira/browse/ASYNC-159>", :user #:user{:name "noisesmith"}}
      #:message{:text "it is still a bit counter intuitive imho, as you would expect a closed chan to return `nil`, but well...it is what it is",
                :user #:user{:name "richiardiandrea"}}
      #:message{:text
                "I'm getting some strange behavior with cljs.\n```\n(gobj/get cljs-react-material-ui.reagent \"app-bar\")\n(gobj/get cljs-react-material-ui.reagent \"paper\")\n```\nWith these two statements, the first returns nil and the second returns the object I want. I aliased the ns to `rui` and\n```\nrui/app-bar\nrui/paper\n```\nboth of these statements return the objects I want. When I `console.dir` the namespace, `app-bar` isn't in it, but I can still access it",
                :user #:user{:name "radomski"}}
      #:message{:text "What could cause something like this to happen?", :user #:user{:name "radomski"}}
      #:message{:text "My suspicion is the google closure compiler. I have advanced compilation turned on", :user #:user{:name "radomski"}}
      #:message{:text "in the library, both objects are also defined the same way", :user #:user{:name "radomski"}}
      #:message{:text "Alright I found out in order to do dynamic access, I need to use `app_bar` instead of `app-bar` anybody know why?",
                :user #:user{:name "radomski"}}
      #:message{:text "(defn app-bar [...] ...) creates a thing called app_bar", :user #:user{:name "noisesmith"}}
      #:message{:text "see the `munge` and `demunge` functions", :user #:user{:name "noisesmith"}}
      #:message{:text
                "I discovered that when I started dumping vars to the console. I feel really stupid now because I totally knew js doesn't support kabab cased names",
                :user #:user{:name "radomski"}}]}]}
   )


(def schema
  (contrib.datomic/indexed-schema
     '({:db/ident :attribute/ident, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "FK to schema, they can't be directly on $ schema because attribute renderers are a \"source code\" concern. TODO: move these off domain and into the fiddle repo."}
         {:db/ident :attribute/renderer, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Default attribute renderer, a CLJS var like `hyperfiddle.ui.controls/code`."}
         {:db/ident :community/category, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/many}, :db/fulltext true, :db/doc "All community categories"}
         {:db/ident :community/name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/fulltext true, :db/doc "A community's name"}
         {:db/ident :community/neighborhood, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "A community's neighborhood"}
         {:db/ident :community/orgtype, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "A community orgtype enum value"}
         {:db/ident :community/type, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "Community type enum values"}
         {:db/ident :community/url, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "A community's url"}
         {:db/ident :db/cardinality, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Property of an attribute. Two possible values: :db.cardinality/one for single-valued attributes, and :db.cardinality/many for many-valued attributes. Defaults to :db.cardinality/one."}
         {:db/ident :db/code, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/fulltext true, :db/doc "String-valued attribute of a data function that contains the function's source code."}
         {:db/ident :db/doc, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/fulltext true, :db/doc "Documentation string for an entity."}
         {:db/ident :db/excise, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :db/fn, :db/valueType {:db/ident :db.type/fn}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "A function-valued attribute for direct use by transactions and queries."}
         {:db/ident :db/fulltext, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Property of an attribute. If true, create a fulltext search index for the attribute. Defaults to false."}
         {:db/ident :db/ident, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "Attribute used to uniquely name an entity."}
         {:db/ident :db/index, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Property of an attribute. If true, create an AVET index for the attribute. Defaults to false."}
         {:db/ident :db/isComponent, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Property of attribute whose vtype is :db.type/ref. If true, then the attribute is a component of the entity referencing it. When you query for an entire entity, components are fetched automatically. Defaults to nil."}
         {:db/ident :db/lang, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Attribute of a data function. Value is a keyword naming the implementation language of the function. Legal values are :db.lang/java and :db.lang/clojure"}
         {:db/ident :db/noHistory, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Property of an attribute. If true, past values of the attribute are not retained after indexing. Defaults to false."}
         {:db/ident :db/txInstant, :db/valueType {:db/ident :db.type/instant}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Attribute whose value is a :db.type/instant. A :db/txInstant is recorded automatically with every transaction."}
         {:db/ident :db/unique, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Property of an attribute. If value is :db.unique/value, then attribute value is unique to each entity. Attempts to insert a duplicate value for a temporary entity id will fail. If value is :db.unique/identity, then attribute value is unique, and upsert is enabled. Attempting to insert a duplicate value for a temporary entity id will cause all attributes associated with that temporary id to be merged with the entity already in the database. Defaults to nil."}
         {:db/ident :db/valueType, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Property of an attribute that specifies the attribute's value type. Built-in value types include, :db.type/keyword, :db.type/string, :db.type/ref, :db.type/instant, :db.type/long, :db.type/bigdec, :db.type/boolean, :db.type/float, :db.type/uuid, :db.type/double, :db.type/bigint,  :db.type/uri."}
         {:db/ident :db.alter/attribute, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "System attribute with type :db.type/ref. Asserting this attribute on :db.part/db with value v will alter the definition of existing attribute v."}
         {:db/ident :db.excise/attrs, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}}
         {:db/ident :db.excise/before, :db/valueType {:db/ident :db.type/instant}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :db.excise/beforeT, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :db.install/attribute, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "System attribute with type :db.type/ref. Asserting this attribute on :db.part/db with value v will install v as an attribute."}
         {:db/ident :db.install/function, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "System attribute with type :db.type/ref. Asserting this attribute on :db.part/db with value v will install v as a data function."}
         {:db/ident :db.install/partition, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "System attribute with type :db.type/ref. Asserting this attribute on :db.part/db with value v will install v as a partition."}
         {:db/ident :db.install/valueType, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "System attribute with type :db.type/ref. Asserting this attribute on :db.part/db with value v will install v as a value type."}
         {:db/ident :db.sys/partiallyIndexed, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "System-assigned attribute set to true for transactions not fully incorporated into the index"}
         {:db/ident :db.sys/reId, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "System-assigned attribute for an id e in the log that has been changed to id v in the index"}
         {:db/ident :didier/team-name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :district/name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "A unique district name (upsertable)"}
         {:db/ident :district/region, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "A district region enum value"}
         {:db/ident :dustingetz/foo, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz/foo1, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz/next, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz/prev, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.gist/src-clojure, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.nav/children, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "Each nav item has optional children"}
         {:db/ident :dustingetz.post/draft-placeholder, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Show in rendered view, but disabled (no link)"}
         {:db/ident :dustingetz.post/hidden, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Omit from user-facing view but show in admin dashboards"}
         {:db/ident :dustingetz.post/published, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.post/published-date, :db/valueType {:db/ident :db.type/instant}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.post/related, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "This post links to these posts, e.g. a table of contents, or \"if you liked this you might also like\""}
         {:db/ident :dustingetz.post/slug, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "Human readable unique identifier for this post "}
         {:db/ident :dustingetz.post/sort-index, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.post/title, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.reg/age, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.reg/birthdate, :db/valueType {:db/ident :db.type/instant}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.reg/email, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}}
         {:db/ident :dustingetz.reg/gender, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.reg/name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.reg/shirt-size, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.slack-converter/raw-source, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.storm/channel, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.storm/ident, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}}
         {:db/ident :dustingetz.streaker/comment, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.streaker/date, :db/valueType {:db/ident :db.type/instant}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.streaker/diet, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.streaker/eating, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.streaker/exercise, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/many}}
         {:db/ident :dustingetz.streaker/food-log, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.streaker/gym, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Generally PIT"}
         {:db/ident :dustingetz.streaker/intention, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.streaker/miles-ran, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Miles run, should be float."}
         {:db/ident :dustingetz.streaker/mood, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.streaker/pt, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "personal trainer"}
         {:db/ident :dustingetz.streaker/teatotal, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.streaker/weight, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Should be decimal, but js/jvm coercion issues"}
         {:db/ident :dustingetz.streaker/weight-6am, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.streaker/yoga, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.streaker.lifting/bench-1rm, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "One rep max"}
         {:db/ident :dustingetz.streaker.lifting/squat-1rm, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "One rep max"}
         {:db/ident :dustingetz.task/completed, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.task/parent, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :dustingetz.task/title, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :fiddle/hydrate-result-as-fiddle, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Experimental. When set, data-sync will interpret this fiddle's result as a fiddle - like a higher order fiddle - this is a recursion mechanic."}
         {:db/ident :fiddle/ident, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "Fiddle identifier used in URLs. Warning: changing this breaks fiddle URLs."}
         {:db/ident :fiddle/links, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/isComponent true, :db/doc "Links to other fiddles that are available from this fiddle"}
         {:db/ident :fiddle/markdown, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Markdown expression for fiddle view, optional"}
         {:db/ident :fiddle/pull, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Datomic pull expression for the entity addressed by the URL"}
         {:db/ident :fiddle/pull-database, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Argument to `datomic.api/pull`, defaults to $"}
         {:db/ident :fiddle/query, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Datomic query datalog. \n\nWarning: no support yet for rules, d/history, d/log or other datomic API access."}
         {:db/ident :fiddle/renderer, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Reagent expression for the fiddle view"}
         {:db/ident :fiddle/type, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Which Datomic query API"}
         {:db/ident :fiddle/uuid, :db/valueType {:db/ident :db.type/uuid}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "For naming anonymous fiddles"}
         {:db/ident :fressian/tag, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Keyword-valued attribute of a value type that specifies the underlying fressian type used for serialization."}
         {:db/ident :helloworld/email, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "hello world email "}
         {:db/ident :hyperfiddle/archived, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :hyperfiddle/deprecated, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :hyperfiddle/owners, :db/valueType {:db/ident :db.type/uuid}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "Entity owners uuids, used by ACLs"}
         {:db/ident :hyperfiddle/starred, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :link/class, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/many}, :db/doc "semantic selector, like html css classes"}
         {:db/ident :link/fiddle, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "link target"}
         {:db/ident :link/formula, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "deprecated; function hook to influence target query inputs wiring, this is fully managed now"}
         {:db/ident :link/path, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "todo rename; specifies the attribute for which this link is valid"}
         {:db/ident :link/rel, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "archived"}
         {:db/ident :link/tx-fn, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "names a hyperfiddle.api/tx-fn multimethod which builds a transaction"}
         {:db/ident :neighborhood/district, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "A neighborhood's district"}
         {:db/ident :neighborhood/name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "A unique neighborhood name (upsertable)"}
         {:db/ident :person/name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}}
         {:db/ident :person/parents, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}}
         {:db/ident :putterson.todone/done, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}}
         {:db/ident :putterson.todone/title, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}})
     ))

(def schemas {"$" (exception/success schema)
              "$clojurians"
              (exception/success
                (contrib.datomic/indexed-schema
                  '({:db/ident :channel/created, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :channel/creator, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :channel/name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}}
                    {:db/ident :channel/slack-id, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}}
                    {:db/ident :event/subtype, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :event/ts, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :message/channel, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Channel the message was posted in."}
                    {:db/ident :message/day, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "The day this message is categorized under, e.g. 2017-11-20."}
                    {:db/ident :message/key, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "Key consisting of channel id + ts. Useful for upserts and refs."}
                    {:db/ident :message/text, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Message text (markdown)"}
                    {:db/ident :message/thread-inst, :db/valueType {:db/ident :db.type/instant}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Same as :message/thread-ts, but parsed to java.util.Date. This is lossy: Date has milisecond precision, ts has microseconds."}
                    {:db/ident :message/thread-ts, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Thread parent message timestamp (seconds since epoch up to 6 decimals). Stored as string because it is used by slack as a kind of identifier. Unique per channel."}
                    {:db/ident :message/ts, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Message timestamp (seconds since epoch up to 6 decimals). Stored as string because it is used by slack as a kind of identifier. Unique per channel."}
                    {:db/ident :message/user, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "User who posted the message"}
                    {:db/ident :user/admin?, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :user/name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "A user's public handle."}
                    {:db/ident :user/owner?, :db/valueType {:db/ident :db.type/boolean}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :user/profile, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/doc "Reference to a user's profile, which contains email, avatar, display name, etc."}
                    {:db/ident :user/real-name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :user/slack-id, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "Internal user identifier used by slack. Alphanumeric, starts with U."}
                    {:db/ident :user-profile/avatar-hash, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :user-profile/display-name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :user-profile/display-name-normalized, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :user-profile/email, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :user-profile/first-name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :user-profile/image-192, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :user-profile/image-24, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :user-profile/image-32, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :user-profile/image-48, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :user-profile/image-512, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :user-profile/image-72, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :user-profile/image-original, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :user-profile/last-name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :user-profile/real-name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :user-profile/real-name-normalized, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
                    {:db/ident :user-profile/title, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}})))
              "$soup"
              (exception/success
                (contrib.datomic/indexed-schema
                  [{:db/ident :a/j
                    :db/cardinality {:db/ident :db.cardinality/one}
                    :db/valueType {:db/ident :db.type/string}}
                   {:db/ident :a/k
                    :db/cardinality {:db/ident :db.cardinality/one}
                    :db/valueType {:db/ident :db.type/string}}
                   {:db/ident :a/s
                    :db/cardinality {:db/ident :db.cardinality/one}
                    :db/valueType {:db/ident :db.type/ref}}
                   {:db/ident :a/t
                    :db/cardinality {:db/ident :db.cardinality/one}
                    :db/valueType {:db/ident :db.type/ref}}
                   {:db/ident :a/u
                    :db/cardinality {:db/ident :db.cardinality/many}
                    :db/valueType {:db/ident :db.type/ref}}
                   {:db/ident :a/v
                    :db/cardinality {:db/ident :db.cardinality/many}
                    :db/valueType {:db/ident :db.type/string}}
                   {:db/ident :a/x
                    :db/cardinality {:db/ident :db.cardinality/one}
                    :db/valueType {:db/ident :db.type/ref}}
                   {:db/ident :a/y
                    :db/cardinality {:db/ident :db.cardinality/one}
                    :db/valueType {:db/ident :db.type/string}}
                   {:db/ident :a/z
                    :db/cardinality {:db/ident :db.cardinality/one}
                    :db/valueType {:db/ident :db.type/string}}
                   {:db/ident :a/comp-one
                    :db/cardinality {:db/ident :db.cardinality/one}
                    :db/valueType {:db/ident :db.type/ref}
                    :db/isComponent true}
                   {:db/ident :a/comp-many
                    :db/cardinality {:db/ident :db.cardinality/one}
                    :db/valueType {:db/ident :db.type/ref}
                    :db/isComponent true}
                   {:db/ident :b/x
                    :db/cardinality {:db/ident :db.cardinality/one}
                    :db/valueType {:db/ident :db.type/string}}
                   {:db/ident :b/y
                    :db/cardinality {:db/ident :db.cardinality/one}
                    :db/valueType {:db/ident :db.type/string}}]))})
