(set-env!
  :dependencies '[[adzerk/bootlaces "0.1.13" :scope "test"]])

(require
  '[adzerk.bootlaces :refer [push-snapshot]])

(def +version+ "0.3.0-SNAPSHOT")

(task-options!
  push #(into % {:repo "deploy-clojars" :ensure-version +version+})
  pom {:project 'com.hyperfiddle/hyperfiddle :version +version+})
