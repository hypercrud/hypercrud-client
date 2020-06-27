(ns hypercrud.browser.base-test
  (:require
    [clojure.test :refer [deftest is]]
    [contrib.do :refer [! via* *this Do-via from-result]]
    [hyperfiddle.def :as hf-def]
    [hypercrud.browser.base :refer [eval-fiddle+]]))

#?(:clj
   ; Crashes in test-cljs
   (hf-def/fiddle ::submission-master
     :eval (user.hello-world/submissions)                   ; undefined, doesn't matter, ever evaled
     :shape '[:find [(pull ?e [:dustingetz/email
                               {:dustingetz/gender [:db/ident]}
                               {:dustingetz/shirt-size [:db/ident]}]) ...]
              :where [?e]]
     :links {":dustingetz/email"                              [[~::submission-detail]
                                                               [:hf/new ~::submission-detail :tx-fn ":user.hello-world/new-submission"]]
             ":hypercrud.browser.base-test/submission-master" [:hf/iframe ~::genders]
             ":dustingetz/gender"                             [:hf/iframe ~::shirt-sizes]}
     :renderer user.hello-world/render-submission-master))

#?(:clj
   (deftest eval-fiddle+|1

     (def fiddle (hf-def/get-fiddle ::submission-master))
     (def route {:hyperfiddle.route/fiddle ::submission-master})
     (def o (from-result (hypercrud.browser.base/eval-fiddle+ fiddle route)))

     (is (= (select-keys o [:fiddle/type :fiddle/ident])
           {:fiddle/type  :eval
            :fiddle/ident ::submission-master}))

     (is (= (:fiddle/links o)
           [{:link/fiddle #:fiddle{:ident :hypercrud.browser.base-test/submission-detail},
             :link/path   ":dustingetz/email",
             :db/id       0}
            {:link/tx-fn  ":user.hello-world/new-submission",
             :link/fiddle #:fiddle{:ident :hypercrud.browser.base-test/submission-detail},
             :link/path   ":dustingetz/email",
             :link/class  [:hf/new],
             :db/id       0}
            {:link/fiddle #:fiddle{:ident :hypercrud.browser.base-test/genders},
             :link/path   ":hypercrud.browser.base-test/submission-master",
             :link/class  [:hf/iframe],
             :db/id       0}
            {:link/fiddle #:fiddle{:ident :hypercrud.browser.base-test/shirt-sizes},
             :link/path   ":dustingetz/gender",
             :link/class  [:hf/iframe],
             :db/id       0}]))
     ))