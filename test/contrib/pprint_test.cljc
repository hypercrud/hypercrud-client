(ns contrib.pprint-test
  (:require
    [clojure.pprint]))


(def stage
  {nil
   {#uri "datomic:free://datomic:4334/hyperfiddle-blog-source"
    [[:db/retract 17592186045579 :fiddle/type :blank]
     [:db/add 17592186045579 :fiddle/type :entity]
     [:db/retract 17592186045579 :fiddle/pull "[:db/id *]"]
     [:db/add 17592186045579 :fiddle/pull "[:db/id :post/title]"]
     [:db/add 17592186046902 :link/disabled? true]
     [:db/add 17592186046578 :link/disabled? true]
     [:db/retract 17592186045579 :fiddle/css "h1 { margin-top: 4em; }\nh2 { margin-top: 4em; }\nh3 { margin-top: 40px; }\nh4 { margin-top: 20px; }\n.lead { margin-top: 1em; margin-bottom: 0; }\n.lead .p { font-size: 1.5em; line-height: 1.2; }\n\n.ui.user .-index { margin-bottom: 300px;  margin-left: auto !important; margin-right: auto !important; }\n.headline { font-size: 2.0em; }\nh1:first-of-type { margin-top: .5em; }\ndiv.call-to-action { margin-top: 1em; margin-bottom: 10em; }\ndiv.intro-cards { margin-top: 5em; margin-bottom: 1em; }\na.btn + a.btn { margin-left: 1em; }\n.subhead { font-size: .8em; font-style: italic; font-weight: 300; }\n.pink-anchor { border-bottom: 1px dotted hotpink; }\n.pink-embed { border: 2px dashed hotpink; }\n.pink-button { border: 1px dotted hotpink; }\n\n/* hacks for slider */\n.slick-slide > div > * { display: inline-block; }\n.slick-slide img { width: 100%; display: inline-block; }\n.slick-slide { padding-right: 0em; padding-left: 0em; }\n.examples .slick-slide a { text-align: center; }\n\n/* user portals in the way, the link-css and fiddle-ident-css might be the same thing */                         \n.ui .fiddle-src .hyperfiddle-form-cell { display: block; } /* suspected dead css? */"]
     [:db/add 17592186045579 :fiddle/css "h1 { margin-top: 4em; }\nh2 { margin-top: 4em; }\nh3 { margin-top: 40px; }\nh4 { margin-top: 20px; }\n.lead { margin-top: 1em; margin-bottom: 0; }\n.lead .p { font-size: 1.5em; line-height: 1.2; }\n\n.ui.user .-index { margin-bottom: 300px;  margin-left: auto !important; margin-right: auto !important; }\n.headline { font-size: 2.0em; }\nh1:first-of-type { margin-top: .5em; } a\ndiv.call-to-action { margin-top: 1em; margin-bottom: 10em; }\ndiv.intro-cards { margin-top: 5em; margin-bottom: 1em; }\na.btn + a.btn { margin-left: 1em; }\n.subhead { font-size: .8em; font-style: italic; font-weight: 300; }\n.pink-anchor { border-bottom: 1px dotted hotpink; }\n.pink-embed { border: 2px dashed hotpink; }\n.pink-button { border: 1px dotted hotpink; }\n\n/* hacks for slider */\n.slick-slide > div > * { display: inline-block; }\n.slick-slide img { width: 100%; display: inline-block; }\n.slick-slide { padding-right: 0em; padding-left: 0em; }\n.examples .slick-slide a { text-align: center; }\n\n/* user portals in the way, the link-css and fiddle-ident-css might be the same thing */                         \n.ui .fiddle-src .hyperfiddle-form-cell { display: block; } /* suspected dead css? */"]]}})

(def stage-str "{nil\n   {#uri \"datomic:free://datomic:4334/hyperfiddle-blog-source\"\n    [[:db/retract 17592186045579 :fiddle/type :blank]\n     [:db/add 17592186045579 :fiddle/type :entity]\n     [:db/retract 17592186045579 :fiddle/pull \"[:db/id *]\"]\n     [:db/add 17592186045579 :fiddle/pull \"[:db/id :post/title]\"]\n     [:db/add 17592186046902 :link/disabled? true]\n     [:db/add 17592186046578 :link/disabled? true]\n     [:db/retract 17592186045579 :fiddle/css \"h1 { margin-top: 4em; }\\nh2 { margin-top: 4em; }\\nh3 { margin-top: 40px; }\\nh4 { margin-top: 20px; }\\n.lead { margin-top: 1em; margin-bottom: 0; }\\n.lead .p { font-size: 1.5em; line-height: 1.2; }\\n\\n.ui.user .-index { margin-bottom: 300px;  margin-left: auto !important; margin-right: auto !important; }\\n.headline { font-size: 2.0em; }\\nh1:first-of-type { margin-top: .5em; }\\ndiv.call-to-action { margin-top: 1em; margin-bottom: 10em; }\\ndiv.intro-cards { margin-top: 5em; margin-bottom: 1em; }\\na.btn + a.btn { margin-left: 1em; }\\n.subhead { font-size: .8em; font-style: italic; font-weight: 300; }\\n.pink-anchor { border-bottom: 1px dotted hotpink; }\\n.pink-embed { border: 2px dashed hotpink; }\\n.pink-button { border: 1px dotted hotpink; }\\n\\n/* hacks for slider */\\n.slick-slide > div > * { display: inline-block; }\\n.slick-slide img { width: 100%; display: inline-block; }\\n.slick-slide { padding-right: 0em; padding-left: 0em; }\\n.examples .slick-slide a { text-align: center; }\\n\\n/* user portals in the way, the link-css and fiddle-ident-css might be the same thing */                         \\n.ui .fiddle-src .hyperfiddle-form-cell { display: block; } /* suspected dead css? */\"]\n     [:db/add 17592186045579 :fiddle/css \"h1 { margin-top: 4em; }\\nh2 { margin-top: 4em; }\\nh3 { margin-top: 40px; }\\nh4 { margin-top: 20px; }\\n.lead { margin-top: 1em; margin-bottom: 0; }\\n.lead .p { font-size: 1.5em; line-height: 1.2; }\\n\\n.ui.user .-index { margin-bottom: 300px;  margin-left: auto !important; margin-right: auto !important; }\\n.headline { font-size: 2.0em; }\\nh1:first-of-type { margin-top: .5em; } a\\ndiv.call-to-action { margin-top: 1em; margin-bottom: 10em; }\\ndiv.intro-cards { margin-top: 5em; margin-bottom: 1em; }\\na.btn + a.btn { margin-left: 1em; }\\n.subhead { font-size: .8em; font-style: italic; font-weight: 300; }\\n.pink-anchor { border-bottom: 1px dotted hotpink; }\\n.pink-embed { border: 2px dashed hotpink; }\\n.pink-button { border: 1px dotted hotpink; }\\n\\n/* hacks for slider */\\n.slick-slide > div > * { display: inline-block; }\\n.slick-slide img { width: 100%; display: inline-block; }\\n.slick-slide { padding-right: 0em; padding-left: 0em; }\\n.examples .slick-slide a { text-align: center; }\\n\\n/* user portals in the way, the link-css and fiddle-ident-css might be the same thing */                         \\n.ui .fiddle-src .hyperfiddle-form-cell { display: block; } /* suspected dead css? */\"]]}}")

;(deftest pprint-datoms-1
;  []
;  (is (= (pprint-datoms-str stage) stage-str)))


(def code-form
  '(fn [ctx]
     (let [hide-datomic (reagent.core/atom true)
           db-attr? #(<= (:db/id %) 62)
           do-filter-reactive (fn [xs]
                                (as-> xs xs
                                      (if @hide-datomic (remove db-attr? xs) xs)))])))

#_(deftest pprint-performance-1 []

                                (time
                                  (with-out-str
                                    (clojure.pprint/pprint code-form)))

                                (time
                                  (with-out-str
                                    (packed-printer/pprint code-form)))

                                )
