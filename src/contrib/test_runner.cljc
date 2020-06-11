(ns contrib.test-runner
  (:require
    [contrib.data :refer [update-existing]]
    [clojure.tools.namespace.find :as nsfind]
    [clojure.test :as clj-test]
    [cljs.test :as cljs-test]
    [clojure.test.junit :as junit]
    [clojure.java.io :as io]
    [cljs-test-runner.main :as cljs-test-runner]))

(defn parse-args
  [args]
  (let [symbol->string (fn [x] (if (symbol? x) (str x) x))
        args (map read-string args)
        args (zipmap (take-nth 2 args) (map symbol->string (take-nth 2 (rest args))))]
    (-> args
        (update-existing :ns-regex #(java.util.regex.Pattern/compile %)))))

(defn test-clj
  [{:keys [output ns-regex]}]

  (apply require :reload
    (nsfind/find-namespaces-in-dir (io/file "test") nsfind/clj))

  (let [namespaces (filter #(re-matches ns-regex (name (ns-name %))) (all-ns))]
    (with-open [w (if output
                    (io/writer output :append false)
                    (io/writer *out* :append true))]
      (binding [clojure.test/*test-out* w]
        (junit/with-junit-output
          (apply clj-test/run-tests namespaces))))))


; -namespace-regex "hyper.*|contrib.*" --env node
; [env dir out watch ns-symbols ns-regexs var include exclude verbose compile-opts doo-opts]
(defn test-cljs
  [{:keys [env ns-regex output]}]
  (with-open [w (if output
                  (io/writer output :append false)
                  (io/writer *out* :append true))]
    (cljs-test-runner/-main "-r" ns-regex "-x" env)))

(defn -main
  [target & args]
  (let [{:keys [output test ns-regex]
         :or {ns-regex #".*"}
         :as args}
        (parse-args args)]
    (condp = target
      "clj"  (test-clj args)
      "cljs" (test-cljs args)
      (ex-info (str "Unsupported target: " target {:target target})))))
