(ns hypercrud.transit
  (:refer-clojure :exclude [bigdec])
  (:require
    [cats.core :as cats]
    [cats.monad.either :as either #?@(:cljs [:refer [Left Right]])]
    [cats.monad.exception :as exception #?@(:cljs [:refer [Failure Success]])]
    [cognitect.transit :as t]
    #?(:cljs [com.cognitect.transit.types])
    [contrib.datomic :refer [->Schema #?(:cljs Schema)]]
    [contrib.uri :refer [->URI #?(:cljs URI)]]
    [contrib.big-decimal :as bigdec :refer [bigdec #?(:cljs BigDecimal)]]
    [contrib.orderedmap :refer [with-order]])
  #?(:clj
     (:import
       (cats.monad.either Left Right)
       (cats.monad.exception Failure Success)
       (clojure.lang ExceptionInfo)
       (contrib.datomic Schema)
       (java.io ByteArrayInputStream ByteArrayOutputStream)
       (java.math BigDecimal))))


(def read-handlers
  (atom
    {"schema-v" (t/read-handler #(apply ->Schema %))
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
     "big-decimal" (t/read-handler #(apply bigdec %))
     }))

(def write-handlers
  (atom
    {Schema (t/write-handler (constantly "schema-v") (fn [^Schema v] (vector (.-schema-by-attr v))))
     Left (t/write-handler (constantly "left-v") (fn [v] (vector (cats/extract v))))
     Right (t/write-handler (constantly "right-v") (fn [v] (vector (cats/extract v))))
     Failure (t/write-handler (constantly "failure-v") (fn [v] (vector (cats/extract v))))
     Success (t/write-handler (constantly "success-v") (fn [v] (vector (cats/extract v))))
     ExceptionInfo (t/write-handler (constantly "ex-info") (fn [ex] [(ex-message ex) (ex-data ex) (ex-cause ex)]))
     #?@(:clj [Exception (t/write-handler (constantly "ex-info") (fn [ex] [(ex-message ex) (assoc (ex-data ex) :ex-type (str (type ex))) (ex-cause ex)]))])
     #?@(:cljs [URI (t/write-handler (constantly "r") (fn [^js v] (.-uri-str v)))])
     #?@(:clj [clojure.lang.PersistentTreeMap (t/write-handler (constantly "sorted-map") (fn [v] (into {} v)))])
     contrib.orderedmap.PersistentOrderedMap (t/write-handler
                                              (constantly "ordered-map")
                                              (fn [^contrib.orderedmap.PersistentOrderedMap omap]
                                                [(.-backing-map omap) (.-order omap)]))
     BigDecimal (t/write-handler (constantly "big-decimal")
                                 (fn [^BigDecimal v] [(bigdec/to-string v)]))
     }))

(def ^:dynamic *string-encoding* "UTF-8")

(defn register-handlers [c tag rep-fn from-rep]
  (swap! read-handlers assoc tag (t/read-handler from-rep))
  (swap! write-handlers assoc c (t/write-handler (constantly tag) rep-fn)))

(defn with-refresh
  "Recompute and remember the return value of `buildf` if `get-new-valuef` return
  a different value than the previous call."
  [get-new-valuef buildf]
  (let [state (volatile! nil)] ; don't need STM
    (fn []
      (if-not (= @state (get-new-valuef))
        (vreset! state (buildf))
        @state))))

(def ^:private default-reader (with-refresh #(deref read-handlers) #(t/reader :json {:handlers @read-handlers})))
(def ^:private default-writer (with-refresh #(deref write-handlers) #(t/writer :json {:handlers @write-handlers})))

(defn decode
  "Transit decode an object from `s`."
  [s & {:keys [type opts]}]
  #?(:clj  (let [type (or type :json)
                 opts (or opts {:handlers @read-handlers})
                 in   (ByteArrayInputStream. (.getBytes s *string-encoding*))
                 rdr  (t/reader in type opts)]
             (t/read rdr))
     :cljs (let [rdr (if (or type opts)
                       (t/reader type (update opts :handlers merge @read-handlers))
                       (default-reader))]
             (t/read rdr s))))

(defn encode
  "Transit encode `x` into a String."
  [x & {:keys [type opts]}]
  #?(:clj  (let [type   (or type :json)
                 opts   (or opts {:handlers @write-handlers})
                 out    (ByteArrayOutputStream.)
                 writer (t/writer out type opts)]
             (t/write writer x)
             (.toString out))
     :cljs (let [wrtr (if (or type opts)
                        (t/writer type (update opts :handlers merge @write-handlers))
                        (default-writer))]
             (t/write wrtr x))))

#?(:cljs (extend-type com.cognitect.transit.types/UUID IUUID)) ; https://github.com/hyperfiddle/hyperfiddle/issues/728

