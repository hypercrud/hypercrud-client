(defproject hyperfiddle "_"
  :dependencies ~(->> "deps.edn"
                      slurp
                      read-string
                      :deps
                      (mapv (fn [[k v]]
                              [k (:mvn/version v)]))
                      (into '[[org.clojure/core.async "0.7.559"]
                              [com.datomic/client-impl-shared "0.8.69"]]))

  :profiles {:dev  {:source-paths ["dev"]
                    :repl-options {:init-ns dev}}

             :test {:dependencies ~(->> "deps.edn"
                                        slurp
                                        read-string
                                        :aliases
                                        :test-clj
                                        :extra-deps
                                        (mapv (fn [[k v]]
                                                [k (:mvn/version v)])))}})
