(ns hyperfiddle.test-runner
  (:require
    [doo.runner :refer-macros [doo-tests]]

    ; NOT clj tests, this is only for CLJS tests
    contrib.char$-test
    contrib.data-test
    contrib.datomic.client.pull-test
    contrib.datomic.client.query-test
    contrib.datomic-test
    contrib.datomic-tx-test
    contrib.ednish-test
    contrib.eval-test
    contrib.match-test
    contrib.pprint-test
    contrib.reactive-test
    contrib.rfc3986-test
    contrib.string-test
    hypercrud.browser.context-test
    hypercrud.browser.router-bidi-test
    hyperfiddle.fiddle-test
    hyperfiddle.io.basis-test
    hyperfiddle.readers-test
    hyperfiddle.route-test
    hyperfiddle.ui.docstring-test
    ))

(defn run []
  (doo-tests
    'contrib.char$-test
    'contrib.data-test
    'contrib.datomic.client.pull-test
    'contrib.datomic.client.query-test
    'contrib.datomic-test
    'contrib.datomic-tx-test
    'contrib.ednish-test
    'contrib.eval-test
    'contrib.match-test
    'contrib.pprint-test
    'contrib.reactive-test
    'contrib.rfc3986-test
    'contrib.string-test
    'hypercrud.browser.context-test
    'hypercrud.browser.router-bidi-test
    'hyperfiddle.fiddle-test
    'hyperfiddle.io.basis-test
    'hyperfiddle.readers-test
    'hyperfiddle.route-test
    'hyperfiddle.ui.docstring-test
    ))
