(ns hyperfiddle.service.routes
  (:require [hyperfiddle.api :as hf]))

(def routes
  ["/" {"favicon.ico" :favicon
        "api"         {"/" {"global-basis"                                {:get  ::hf/global-basis
                                                                           #".+" ::hf/not-found
                                                                           true  ::hf/not-allowed}
                            ["hydrate-requests/" [#"[^/]*" :local-basis]] {:post ::hf/hydrate-requests
                                                                           #".+" ::hf/not-found
                                                                           true  ::hf/not-allowed}
                            ["hydrate-route/" [#"[^/]*" :local-basis] "/" [#"[^/]*" :partition-id] "/" [#".*" :encoded-route]]
                            {:get  ::hf/hydrate-route
                             :post ::hf/hydrate-route
                             true  ::hf/not-allowed}
                            ["local-basis/" [#"[^/]*" :global-basis] "/" [#".*" :encoded-route]]
                            {:get  ::hf/local-basis
                             :post ::hf/local-basis
                             true  ::hf/not-allowed}
                            "sync"                                        {:post ::hf/sync
                                                                           #".+" ::hf/not-found
                                                                           true  ::hf/not-allowed}
                            "transact"                                    {:post ::hf/transact
                                                                           #".+" ::hf/not-found
                                                                           true  ::hf/not-allowed}
                            "subscribe"                                   {:get ::hf/subscribe
                                                                           true ::hf/not-allowed}
                            true                                          ::hf/not-found}

                       [[#"[^/]*" :version] "/"] {true ::hf/force-refresh}
                       true                      ::hf/not-found}

        "static/"                 {[:build "/" [#".+" :resource-name]] {:get ::hf/static-resource
                                                                        true ::hf/not-allowed}
                                   true                                ::hf/not-found}
        "auth0"                   {:get  ::hf/auth0-redirect
                                   #".+" ::hf/not-found
                                   true  ::hf/not-allowed}
        "ws-auth/exchange-token" {:get ::hf/ws-exchange-token}
        "logout"                  {:post ::hf/logout
                                   #".+" ::hf/not-found
                                   true  ::hf/not-allowed}
        true                      {:get ::hf/ssr
                                   true ::hf/not-allowed}}])
