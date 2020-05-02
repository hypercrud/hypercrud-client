(ns contrib.config)


#?(:cljs (if (= *target* "nodejs")
           (require '[goog.object])))

(defn get-env [s]
  #?(:clj #_(contrib.data/tap
              #(println (str "env read " s "=" %)))
           (or (System/getenv s)
             ; Return "" instead of nil due to https://clojure.atlassian.net/browse/CLJ-1138
             ; Execution error (ExceptionInfo) at clojure.tools.reader.impl.errors/throw-ex (errors.clj:34).
             ; No dispatch macro for e.
             "")
     :cljs (if (= *target* "nodejs")
             (goog.object/get js/process.env s))))

(def env-edn-reader (fn [s] (get-env (str s))))
(def env-clj-reader (fn [s] `(get-env ~(str s))))

(comment
  (get-env "HOME")
  (get-env "UNDEFINED_VAR____")

  (require 'clojure.tools.reader.edn)
  (def opts {:readers {'env contrib.config/env-edn-reader}})
  (= [(clojure.tools.reader.edn/read-string opts "{:foo #env HOME}")
      (clojure.tools.reader.edn/read-string opts "{:foo #env UNDEFINED_VAR____}")]
    [{:foo "/Users/dustin"} {:foo ""}])

  (require 'clojure.tools.reader)
  (= (eval
       (binding [clojure.tools.reader/*data-readers* {'env contrib.config/env-clj-reader}]
         [(clojure.tools.reader/read-string "{:foo #env HOME}")
          (clojure.tools.reader/read-string "{:foo #env UNDEFINED_VAR____}")]))
    [{:foo "/Users/dustin"} {:foo ""}])

  )
