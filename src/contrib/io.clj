(ns contrib.io
  (:require
    [contrib.reader]
    [contrib.data :refer [tap]]
    [contrib.dataflow :as df]
    [rksm.subprocess :as subprocess]
    [clojure.java.io :as io]
    [taoensso.timbre :refer [warn]]))


; These are re-exported here to facilitate :refer :all
(defn get-file [path] (slurp path))

(defn get-edn [path]
  #_(tap
      #(warn "get-edn: " (pr-str %)))
  (-> path get-file contrib.reader/read-edn-string!))

(comment
  (get-edn "./hyperfiddle.edn")
  )

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
    (catch Error _ false)))

; Reactive file system

(def watchers (atom {}))

(defn watch-dir [dir f!]
  (let [watch (.. java.nio.file.FileSystems getDefault newWatchService)]

    (when-let [watcher (get @watchers dir)]
      (.close watcher))

    (swap! watchers assoc dir watch)

    (.register dir watch
      (into-array java.nio.file.StandardWatchEventKinds$StdWatchEventKind
        [java.nio.file.StandardWatchEventKinds/ENTRY_MODIFY])
      (into-array java.nio.file.WatchEvent$Modifier
        [com.sun.nio.file.SensitivityWatchEventModifier/HIGH]))

    (.start
      (Thread.
        (fn []
          (loop []
            (when-let [key (.take watch)]
              (f! key)
              (.reset key)
              (recur))))))))

(defn watch-file
  ([path f!]
   (let [file (clojure.java.io/file path)]
     (watch-dir
       (.getParent (java.nio.file.Paths/get (.toURI file))) #_(java.nio.file.Paths/get (.toURI file))
       (fn [watch-key]
         (doseq [event (.pollEvents watch-key)]
           (when (= (.toPath file) (.context event))
             (f! path)))))))
  ([path]
   (let [origin (df/input path)]
     (watch-file path (fn [path] (df/put origin path)))     ; change callback gets path
     origin)))

(comment
  (require '[contrib.config :refer [get-edn]])
  (def >config (->> (watch-file "hyperfiddle.edn") (df/map get-edn)))
  (df/consume println >config)
  )
