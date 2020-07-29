(ns contrib.reader                                          ; depends on hyperfiddle, but shouldn't (this ns should not exist at all)
  (:refer-clojure :exclude [read-string])
  (:require
    [contrib.try$ :refer [try-either]]
    [clojure.tools.reader]
    [clojure.tools.reader.edn]
    [hyperfiddle.readers]))                                 ; yolo

; cljs.tagged-literals is the cljs compile-time stuff, I chose
; not to call into that namespace, I don't understand the implications rn.

(defn read-string                                           ; i dont like this collision with clojure.core.read-string it is confusing
  "read-clj-string"
  ([s]
   {:pre [s]}
   (read-string {} s))
  ([opts s]
   {:pre [s]}
   ; Can't this be done with (set! *data-readers*) in main?
   (binding [clojure.tools.reader/*data-readers* (merge
                                                   hyperfiddle.readers/hf-edn-readers
                                                   clojure.tools.reader/*data-readers*)]
     (clojure.tools.reader/read-string opts s))))

(defn read-string+ [& args] (try-either (apply read-string args)))
(def memoized-read-string+ (memoize read-string+))

(defn read-edn-string!
  ([s]
   (read-edn-string! {:eof nil} s))
  ([opts s]
   (try
     (clojure.tools.reader.edn/read-string
      (update opts :readers merge hyperfiddle.readers/hf-edn-readers)
      s)
     (catch #?(:clj Exception :cljs js/Error) e
       (throw (ex-info "Unable to read string" {:str s} e))))))

(defn read-edn-string+ [& args] (try-either (apply read-edn-string! args)))

(def memoized-read-edn-string+ (memoize read-edn-string+))
