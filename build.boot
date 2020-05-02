(set-env!
  :dependencies '[[adzerk/boot-cljs "2.1.5" :scope "test"]
                  [adzerk/boot-test "1.2.0" :scope "test"]
                  [adzerk/bootlaces "0.1.13" :scope "test"]
                  [crisptrutski/boot-cljs-test "0.3.4" :scope "test"]
                  [deps-to-boot "1.1.0" :scope "test"]
                  [seancorfield/boot-tools-deps "0.4.7" :scope "test"]
                  [sparkfund/boot-lein "0.4.0" :scope "test"]])

(require '[adzerk.boot-cljs :refer :all]
         '[adzerk.boot-test :as boot-test]
         '[adzerk.bootlaces :refer [push-snapshot]]
         '[sparkfund.boot-lein :as boot-lein]
         '[boot-tools-deps.core :refer [deps]]
         '[crisptrutski.boot-cljs-test :as boot-cljs-test]
         '[deps-to-boot.core :as deps-to-boot :refer [boot-deps]])

(->> (let [env (boot-deps)]
       (assoc env
         ::boot-lein/project-clj {:dependencies (->> (keys (:aliases deps-to-boot/deps-edn))
                                                     (map boot-deps)
                                                     (cons env)
                                                     (mapcat :dependencies)
                                                     (sort-by (comp str first))
                                                     vec)}))
     (apply concat)
     (apply merge-env!))

(def +version+ "0.3.0-SNAPSHOT")

(task-options!
  push #(into % {:repo "deploy-clojars" :ensure-version +version+})
  pom {:project 'com.hyperfiddle/hyperfiddle :version +version+})

; shadowing test-cljs breaks the test-cljs CLI e.g. boot test-cljs -n "contrib"
(deftask test-cljs [x exit? bool "Throw exception on error or inability to run tests."]
         (comp
           (deps :quick-merge true :aliases [:test-cljs])
           (boot-cljs-test/test-cljs :ids ["hyperfiddle/tests"]
                                     :js-env :node
                                     :exit? exit?)))

(deftask test-clj []
         (comp
           (deps :quick-merge true :aliases [:test-clj])
           (boot-test/test :include #".*-test")))
