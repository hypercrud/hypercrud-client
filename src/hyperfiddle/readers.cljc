(ns hyperfiddle.readers                                     ; reader-config
  (:require
    [#?(:cljs cljs.reader :clj clojure.tools.reader.default-data-readers)]
    [contrib.config]                                        ; load reader onto classpath
    [contrib.datomic]
    [contrib.uri]
    #?(:cljs [contrib.uuid])
    #?(:cljs [goog.math])
    [hypercrud.types.DbName]
    [hypercrud.types.DbRef]
    [hypercrud.types.ThinEntity]
    [hypercrud.types.EntityRequest]
    [hypercrud.types.Err]
    [hypercrud.types.QueryRequest]
    )
  #?(:clj
     (:import
       [java.lang Long])))


(def schema-edn-reader (fn [s] (contrib.datomic/->Schema s)))
(def schema-clj-reader (fn [s] `(schema-edn-reader ~s)))

(defn long-edn-reader [s]
  ; #long "65332980922449989"
  ; Wrapped in string because the tag operates on a processed platform value
  ; (so Javascript has already damaged the long)
  #?(:cljs (.fromString goog.math.Long s))
  #?(:clj (Long/parseLong s)))

(def long-clj-reader (fn [s] `(long-edn-reader ~s)))

#?(:cljs
   (defn- long-edn-writer ^String [^goog.math.Long o]
     (str "#long " (pr-str (.toString o)))))

#?(:cljs
   (extend-type goog.math.Long
     IPrintWithWriter
     (-pr-writer [o writer _] (-write writer (long-edn-writer o)))))

(def hf-edn-readers
  {'inst #?(:cljs cljs.reader/read-date :clj clojure.tools.reader.default-data-readers/read-instant-date)
   'uuid #?(:cljs contrib.uuid/read-uuid :clj clojure.tools.reader.default-data-readers/default-uuid-reader)
   ;'queue #?(:cljs cljs.reader/read-queue) ; queue not supported on JVM: https://dev.clojure.org/jira/browse/CLJ-976
   ;'js #?(:cljs cljs.tagged-literals/read-js) ; compiler, not reader ?
   'dbname hypercrud.types.DbName/dbname-edn-reader
   'entity hypercrud.types.ThinEntity/entity-edn-reader
   'schema hyperfiddle.readers/schema-edn-reader
   'uri contrib.uri/uri-edn-reader
   'env contrib.config/env-edn-reader
   'long hyperfiddle.readers/long-edn-reader
   'hypercrud.types.DbRef.DbRef hypercrud.types.DbRef/map->DbRef
   'hypercrud.types.EntityRequest.EntityRequest hypercrud.types.EntityRequest/map->EntityRequest
   'hypercrud.types.Err.Err hypercrud.types.Err/map->Err
   'hypercrud.types.QueryRequest.QueryRequest hypercrud.types.QueryRequest/map->QueryRequest
   'hypercrud.types.QueryRequest.EvalRequest hypercrud.types.QueryRequest/map->EvalRequest
   })
