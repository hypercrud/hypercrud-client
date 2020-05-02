(ns hypercrud.types.DbName)


(defrecord DbName [dbname])

(defn- impl-print ^String [^DbName o]
  (str "#dbname" (pr-str (.-dbname o))))

#?(:cljs
   (extend-type DbName
     IPrintWithWriter
     (-pr-writer [o writer _] (-write writer (impl-print o)))))

#?(:clj
   (defmethod print-method DbName [o ^java.io.Writer w]
     (.write w (impl-print o))))

#?(:clj
   (defmethod print-dup DbName [o ^java.io.Writer w]
     (.write w (impl-print o))))

(def dbname-edn-reader #(->DbName %))
(def dbname-clj-reader (fn [s] `(dbname-edn-reader ~s)))
