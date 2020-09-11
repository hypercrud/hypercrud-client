 (ns hyperfiddle.def.ide)

(comment
  ;; ?
  (hf-def/fiddle :hyperfiddle.ide/schema.attribute
                 :links {":hyperfiddle.ide/schema.attribute"
                         [[:hf/iframe ~:hyperfiddle/topnav]
                          [:hf/iframe ~:hyperfiddle.ide/schema-editor.attribute
                           :formula (constantly (second @(:hypercrud.browser/route ctx)))]]}

                 :renderer
                 [:<>
                  [hyperfiddle.ui/ui-from-link
                   (hyperfiddle.data/select ctx :hyperfiddle/topnav) ctx {}]
                  [hyperfiddle.ui/ui-from-link
                   (hyperfiddle.data/select ctx :hyperfiddle.ide/schema-editor.attribute) ctx {}]])

  ;; needed?
  (hf-def/fiddle :hyperfiddle.ide.user/databases
                 :query '[:find [(pull $domains ?db [:database/uri :db/id]) ...]
                          :in $domains
                          :where
                          [(ground hyperfiddle.api/*subject*) ?user-id]
                          [$domains ?db :database/uri]
                          [$domains ?db :hyperfiddle/owners ?user-id]]

                 :markdown "Databases"

                 :renderer
                 (let [{:keys [:hypercrud.browser/fiddle]} ctx
                       dbs                                 (hypercrud.browser.context/data ctx)]
                   (when-not (empty? dbs)
                     [:div props
                      [hyperfiddle.ui/markdown (:fiddle/markdown @fiddle) ctx]
                      [:ul
                       (->> (sort-by :database/uri dbs)
                            (map (fn [db]
                                   [:li {:key (hash (:db/id db))} (str (:database/uri db))]))
                            (doall))]])))

  (hf-def/fiddle :hyperfiddle.ide.schema/editor$ [:ide-dbname]
                 :with {:ident (keyword "hyperfiddle.ide.schema" (str "editor" ~:ide-dbname))
                        :$db   (symbol ~:ide-dbname)}

                 :query
                 '[:in ~:$db :find (pull ~:$db ?attr
                                         [:db/id
                                          :db/ident
                                          {:db/valueType [:db/ident]}
                                          {:db/cardinality [:db/ident]}
                                          {:db/unique [:db/ident]}
                                          :db/isComponent :db/fulltext :db/doc])
                   :where [~:$db :db.part/db :db.install/attribute '?attr]
; binding for ui

                   [~:$db ?attr :db/ident ?ident]]

                 :renderer hyperfiddle.ide.fiddles.schema-editor/renderer

                 :links
                 {~(str ~:ide-dbname " :db/ident")
                  [[~(:hyperfiddle.ide.schema/attribute$ ~:ide-dbname)]
                   [:hf/new ~(:hyperfiddle.ide.schema/editor.attribute$ ~:ide-dbname)]]})

  (hf-def/fiddle :hyperfiddle.ide.schema/attribute$ [:user-dbname]
                 :with {:ident      (keyword "hyperfiddle.ide.schema" (str "attribute" ~db))
                        :ide-dbname (~user-db->ide ~db)}

                 :links
                 {~:ident [[:hf/iframe ~:hyperfiddle/topnav]
                           [:hf/iframe ~(:hyperfiddle.ide.schema/editor.attribute$ ~:ide-dbname)
                            :formula (constantly (first (:hyperfiddle.route/datomic-args @(:hypercrud.browser/route ctx))))]]}

                 :renderer
                 [:<>
                  [hyperfiddle.ui/ui-from-link
                   (hyperfiddle.data/select ctx :hyperfiddle/topnav) ctx {}]
                  [hyperfiddle.ui/ui-from-link
                   (hyperfiddle.data/select ctx :hyperfiddle.ide.schema/editor.attribute$ ~:ide-dbname) ctx {}]]

                 :fiddle/css "
    .-hyperfiddle-ide-schema-editor-attribute {
      flex: 1 1;
      overflow: auto;
    }
  ")

  (hf-def/fiddle :hyperfiddle.ide.schema/editor.attribute$ [:ide-dbname]
                 :with {:ident (contrib.data/qualify 'hyperfiddle.ide.schema (str "editor.attribute" ~:ide-dbname))}

                 :pull ~:ide-dbname
                 [:db/id                                             ; for smart-identity tempid stability
                  :db/ident
                  {:db/valueType [:db/ident]}
                  {:db/cardinality [:db/ident]}
                  :db/doc
                  {:db/unique [:db/ident]}
                  :db/isComponent
                  :db/fulltext]

                 :renderer hyperfiddle.ide.fiddles.schema-attribute/renderer

                 :links
                 {~:ident [[#{:hf/iframe :cardinality-options}
                            ~(:hyperfiddle.ide.schema/options.cardinality$ ~:ide-dbname)]
                           [#{:hf/iframe :unique-options}
                            ~(:hyperfiddle.ide.schema/options.unique$ ~:ide-dbname)]
                           [#{:hf/iframe :valueType-options}
                            ~(:hyperfiddle.ide.schema/options.valueType$ ~:ide-dbname)]]})

  (hf-def/fiddle :hyperfiddle.ide.schema/options.cardinality$ [:ide-dbname]
                 :with {:$db (symbol ~:ide-dbname)}
                 :query
                 '[:in ~:$db :find (list 'pull ~:$db '?e [:db/ident]) :where
                   [~:$db ?e :db/ident ?ident]
                   [(namespace ?ident) ?ns]
                   [(= ?ns "db.cardinality")]])

  (hf-def/fiddle :hyperfiddle.ide.schema/options.unique$ [:ide-dbname]
                 :with {:$db (symbol ~:ide-dbname)}
                 :query '[:in ~$db :find (list 'pull ~$db '?e [:db/ident]) :where
                          [~$db ?e :db/ident ?ident]
                          [(namespace ?ident) ?ns]
                          [(= ?ns "db.unique")]])

  (hf-def/fiddle :hyperfiddle.ide.schema/options.valueType$ [:ide-dbname]
                 :with {:$db (symbol ~:ide-dbname)}
                 :query
                 '[:in ~:$db :find (pull ~:$db ?valueType [:db/ident]) :where
                   [~:$db ?db-part :db.install/valueType ?valueType]
                   [~:$db ?db-part :db/ident :db.part/db]])

  (hf-def/fiddle :hyperfiddle.ide.schema/$ []
                 :with
                 {:ident      ~(keyword "hyperfiddle.ide.schema" ~db)
                  :ide-dbname ~(~user-db->ide ~db)}

                 :links
                 {~:ident [[:hf/iframe ~(:hyperfiddle.ide.schema/editor$ ~:ide-dbname)]
                           [:hf/iframe ~:hyperfiddle/topnav]]}

                 :renderer
                 [:<>
                  [hyperfiddle.ui/ui-from-link
                   (hyperfiddle.data/select ctx :hyperfiddle/topnav) ctx {}]
                  [hyperfiddle.ui/ui-from-link
                   (hyperfiddle.data/select ctx :hyperfiddle.ide.schema/editor$
                                            ~:ide-dbname) ctx {}]]

                 :css "
     .-hyperfiddle-ide-schema-editor {
       flex: 1 1;
       overflow: auto;
     }
  ")
  )

