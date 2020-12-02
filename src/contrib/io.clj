(ns contrib.io
  (:require
    [contrib.reader]
    [contrib.data :refer [tap]]
    [hyperfiddle.fabric :as df]
    [rksm.subprocess :as subprocess]
    [clojure.java.io :as io]
    [taoensso.timbre :refer [warn]])
  (:import
    (java.nio.file Path FileSystems Paths WatchEvent$Modifier StandardWatchEventKinds StandardWatchEventKinds$StdWatchEventKind)
    (com.sun.nio.file SensitivityWatchEventModifier)))

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

(defn watch-dir
  ([^Path dir f!]
   (let [watch (.. FileSystems getDefault newWatchService)]

     (when-let [watcher (get @watchers dir)]
       (.close watcher))

     (swap! watchers assoc dir watch)

     (.register dir watch
       (into-array StandardWatchEventKinds$StdWatchEventKind [StandardWatchEventKinds/ENTRY_MODIFY])
       (into-array WatchEvent$Modifier [SensitivityWatchEventModifier/HIGH])) ; 2 seconds is highest freq modeled

     (.start
       (Thread.
         (fn []
           (loop []
             (when-let [key (.take watch)]
               (f! key)
               (.reset key)
               (recur))))))))
  #_([^Path dir]
   (let [origin (df/input dir)]
     (watch-dir dir (fn [watch-key] (df/put origin watch-key)))
     origin)))

(comment
  ; How do you even get a path
  (Paths/get "/tmp/foo") => crash
  (def >dir (watch-dir "./")) => not a Path
  (df/on >dir println )
  )

(defn watch-file
  ([path f!]
   (let [file (clojure.java.io/file path)
         dir (.getParent (Paths/get (.toURI file)))]        ; java.nio.file.Path
     (watch-dir dir
       (fn [watch-key]
         (doseq [event (.pollEvents watch-key)]
           (when (= (.toPath file) (.context event))
             (f! path)))))))
  ([path]
   (let [origin (df/input)]
     (watch-file path (fn [path] (df/put origin path)))     ; change callback gets path
     origin)))

(comment
  (require '[contrib.config :refer [get-edn]])
  (def >config (-> (watch-file "hyperfiddle.edn") (df/fmap get-edn)))
  (df/on >config println)
  )
