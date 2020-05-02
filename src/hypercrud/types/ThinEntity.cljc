(ns hypercrud.types.ThinEntity
  #?(:clj
     (:import [clojure.lang Counted IFn IHashEq ILookup Seqable]
              [clojure.core.protocols IKVReduce])))


(declare impl-hash)
(declare impl-print)

; dbname = $, for URIs
(deftype ThinEntity [dbname id]                             ; id can be db/ident and lookup ref
  #?@(:clj  [Object
             (equals [o other]
               (and (instance? ThinEntity other)
                    (= (.-dbname o) (.-dbname ^ThinEntity other))
                    (= (.-id o) (.-id ^ThinEntity other))))

             IHashEq
             (hasheq [o] (impl-hash o))

             Seqable
             (seq [o] (seq {:db/id id}))                    ; lookup ref?

             Counted
             (count [o] (count {:db/id id}))

             ILookup
             (valAt [o k] (get {:db/id id} k))
             (valAt [o k not-found] (get {:db/id id} k not-found))

             IKVReduce
             (kv_reduce [o f init] (reduce-kv {:db/id id} f init))

             IFn
             (invoke [o k] ({:db/id id} k))
             (invoke [o k not-found] ({:db/id id} k not-found))]

      :cljs [IPrintWithWriter
             (-pr-writer [o writer _] (-write writer (impl-print o)))

             IEquiv
             (-equiv [o other]
                     (and (instance? ThinEntity other)
                          (= (.-dbname o) (.-dbname other))
                          (= (.-id o) (.-id other))))

             IHash
             (-hash [o] (impl-hash o))

             IIterable
             (-iterator [o] (-iterator {:db/id id}))

             ISeqable
             (-seq [o] (seq {:db/id id}))

             ICounted
             (-count [o] (count {:db/id id}))

             ILookup
             (-lookup [o k] (get {:db/id id} k))
             (-lookup [o k not-found] (get {:db/id id} k not-found))

             IFind
             (-find [o k] (-find {:db/id id} k))

             IKVReduce
             (-kv-reduce [o f init] (reduce-kv {:db/id id} f init))

             IFn
             (-invoke [o k] ({:db/id id} k))
             (-invoke [o k not-found] ({:db/id id} k not-found))]))

(defn- impl-hash [^ThinEntity o]
  (hash [(.-dbname o) (.-id o)]))

(defn- impl-print ^String [^ThinEntity o]
  (str "#entity" (pr-str [(.-dbname o) (.-id o)])))

#?(:clj
   (defmethod print-method ThinEntity [o ^java.io.Writer w]
     (.write w (impl-print o))))

#?(:clj
   (defmethod print-dup ThinEntity [o ^java.io.Writer w]
     (.write w (impl-print o))))

(def entity-edn-reader (fn [[dbname id]] (hypercrud.types.ThinEntity/->ThinEntity dbname id)))
(def entity-clj-reader (fn [[dbname id :as args]] `(entity-edn-reader ~args)))

(defn thinentity? [o] (instance? ThinEntity o))             ; this is hard to implement portably outside this file
