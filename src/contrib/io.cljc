(ns contrib.io
  #?(:clj
     (:require
       [contrib.reader]
       [contrib.data :refer [tap]]
       [rksm.subprocess :as subprocess]
       [clojure.java.io :as io]
       [taoensso.timbre :refer [warn]])))


#?(:clj
   (do
     ; These are re-exported here to facilitate :refer :all
     (defn get-file [path] (slurp path))
     (defn get-edn [path]
       #_(tap
         #(warn "get-edn: " (pr-str %)))
       (-> path get-file contrib.reader/read-edn-string!))

     (def get-resource io/resource)

     (defn as-file [f] (io/as-file f))

     (defn file-exists [f] (.exists (as-file f)))

     (defn sh-async [& args] (apply subprocess/async-proc args))

     (defn sh [& args]
       (let [p (apply sh-async args)]
         (if (= 0 (subprocess/wait-for p))
           (subprocess/stdout p)
           (throw (Error. (subprocess/stderr p))))))

     (defn sh-stop [p]
       (subprocess/signal p "TERM")
       nil)

     (defn worked [& args]
       (try
         (-> (apply sh-async args)
             subprocess/process-obj
             (.waitFor 1 java.util.concurrent.TimeUnit/SECONDS))
         (catch Error _ false)))))

(comment
  (get-edn "./hyperfiddle.edn")
  )
