(ns hypercrud.transit
  (:require
    [cats.core :as cats]
    [cats.monad.either :as either #?@(:cljs [:refer [Left Right]])]
    [cats.monad.exception :as exception #?@(:cljs [:refer [Failure Success]])]
    [cognitect.transit :as t]
    #?(:cljs [com.cognitect.transit.types])
    [contrib.datomic :refer [->Schema #?(:cljs Schema)]]
    [contrib.uri :refer [->URI #?(:cljs URI)]]
    [hypercrud.types.DbName :refer [->DbName #?(:cljs DbName)]]
    [hypercrud.types.DbRef :refer [->DbRef #?(:cljs DbRef)]]
    [hypercrud.types.EntityRequest :refer [->EntityRequest #?(:cljs EntityRequest)]]
    [hypercrud.types.Err :refer [->Err #?(:cljs Err)]]
    [hypercrud.types.QueryRequest :refer [->QueryRequest #?(:cljs QueryRequest)
                                          ->EvalRequest #?(:cljs EvalRequest)]]
    [hypercrud.types.ThinEntity :refer [->ThinEntity #?(:cljs ThinEntity)]]
    [contrib.orderedmap :refer [with-order ordered-map]])
  #?(:clj
     (:import
       (cats.monad.either Left Right)
       (cats.monad.exception Failure Success)
       (clojure.lang ExceptionInfo)
       (contrib.datomic Schema)
       (hypercrud.types.DbName DbName)
       (hypercrud.types.DbRef DbRef)
       (hypercrud.types.EntityRequest EntityRequest)
       (hypercrud.types.Err Err)
       (hypercrud.types.QueryRequest QueryRequest EvalRequest)
       (hypercrud.types.ThinEntity ThinEntity)
       (java.io ByteArrayInputStream ByteArrayOutputStream))))


(def read-handlers
  (atom
    {"schema-v" (t/read-handler #(apply ->Schema %))
     "DbName" (t/read-handler ->DbName)
     "DbRef" (t/read-handler #(apply ->DbRef %))
     "EReq" (t/read-handler #(apply ->EntityRequest %))
     "EvalReq" (t/read-handler #(apply ->EvalRequest %))
     "err" (t/read-handler ->Err)
     "QReq" (t/read-handler #(apply ->QueryRequest %))
     "entity" (t/read-handler #(apply ->ThinEntity %))
     "r" (t/read-handler ->URI)
     "left" (t/read-handler #(either/left %))
     "right" (t/read-handler #(either/right %))
     "failure" (t/read-handler #(exception/failure %))
     "success" (t/read-handler #(exception/success %))
     "left-v" (t/read-handler #(apply either/left %))
     "right-v" (t/read-handler #(apply either/right %))
     "failure-v" (t/read-handler #(apply exception/failure %))
     "success-v" (t/read-handler #(apply exception/success %))
     "ex-info" (t/read-handler #(apply ex-info %))
     "sorted-map" (t/read-handler #(into (sorted-map) %))
     "ordered-map" (t/read-handler #(apply with-order %))
     }))

(def write-handlers
  (atom
    {Schema (t/write-handler (constantly "schema-v") (fn [^Schema v] (vector (.-schema-by-attr v))))
     DbName (t/write-handler (constantly "DbName") (fn [v] (:dbname v)))
     DbRef (t/write-handler (constantly "DbRef") (fn [v] [(:dbname v) (:branch v)]))
     EntityRequest (t/write-handler (constantly "EReq") (fn [v] [(:e v) (:db v) (:pull-exp v)]))
     Err (t/write-handler (constantly "err") #(:msg %))
     QueryRequest (t/write-handler (constantly "QReq") (fn [v] [(:query v) (:params v) (:opts v)]))
     EvalRequest (t/write-handler (constantly "EvalReq") (fn [v] [(:form v) (:pid v) (:route v)]))
     ThinEntity (t/write-handler (constantly "entity") (fn [^ThinEntity v] [(.-dbname v) (.-id v)]))
     Left (t/write-handler (constantly "left-v") (fn [v] (vector (cats/extract v))))
     Right (t/write-handler (constantly "right-v") (fn [v] (vector (cats/extract v))))
     Failure (t/write-handler (constantly "failure-v") (fn [v] (vector (cats/extract v))))
     Success (t/write-handler (constantly "success-v") (fn [v] (vector (cats/extract v))))
     ExceptionInfo (t/write-handler (constantly "ex-info") (fn [ex] [(ex-message ex) (ex-data ex) (ex-cause ex)]))
     #?@(:cljs [URI (t/write-handler (constantly "r") (fn [v] (.-uri-str v)))])
     #?@(:clj [clojure.lang.PersistentTreeMap (t/write-handler (constantly "sorted-map") (fn [v] (into {} v)))])
     #?@(:clj [contrib.orderedmap.PersistentOrderedMap (t/write-handler (constantly "ordered-map") (fn [omap] [(.backing-map omap) (.order omap)]))])
     }))

(def ^:dynamic *string-encoding* "UTF-8")

(defn register-handlers [c tag rep-fn from-rep]
  (swap! read-handlers assoc tag (t/read-handler from-rep))
  (swap! write-handlers assoc c (t/write-handler (constantly tag) rep-fn)))

(defn decode
  "Transit decode an object from `s`."
  [s & {:keys [type opts]
        :or {type :json opts {:handlers @read-handlers}}}]
  #?(:clj  (let [in (ByteArrayInputStream. (.getBytes s *string-encoding*))
                 rdr (t/reader in type opts)]
             (t/read rdr))
     :cljs (let [rdr (t/reader type opts)]
             (t/read rdr s))))

(defn encode
  "Transit encode `x` into a String."
  [x & {:keys [type opts]
        :or {type :json opts {:handlers @write-handlers}}}]
  #?(:clj  (let [out (ByteArrayOutputStream.)
                 writer (t/writer out type opts)]
             (t/write writer x)
             (.toString out))
     :cljs (let [wrtr (t/writer type opts)]
             (t/write wrtr x))))

#?(:cljs (extend-type com.cognitect.transit.types/UUID IUUID)) ; https://github.com/hyperfiddle/hyperfiddle/issues/728
