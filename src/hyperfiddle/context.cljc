(ns hyperfiddle.context
  (:require
   [hyperfiddle.runtime :as runtime]))

; #::{:level #:find{:rel :rel-entry :coll :tuple :scalar}
;     :value "the value"
;     :schema {... schema ...}}

(declare focus, focus*, focus-in)

(defn local-schema
  [ctx]
  (condp = (::type ctx)
    :find/scalar (->> ctx ::schema (get (::attribute ctx)))))

(defn eldest
  [ctx key]
  (when ctx
    (or (eldest (::parent ctx) key)
        (get ctx key))))

(defn youngest
  [ctx key]
  (when ctx
    (if (contains? ctx key)
      (get ctx key)
      (recur (::parent ctx) key))))

(defn ancestory
  [ctx key]
  (loop [ctx ctx
         hier []]
    (if-let [val (key ctx)]
      (recur (::parent ctx) (conj hier val))
      hier)))

(defn identifier?
  [{:keys [::schema ::attribute]}]
  (or
   (= :db/id attribute)
   (= :db/ident attribute)
   (-> schema (get attribute) :db/unqiue (= :db.unique/identity))))

(defn entity
  [ctx]
  (condp = (::type ctx)
    :find/rel-entry (some entity (focus* ctx))
    :find/tuple (some entity (vals (focus* ctx)))
    :find/scalar (when (identifier? ctx) (::value ctx))))

(defn with-entity
  [ctx]
  (assoc ctx ::entity (entity ctx)))

(defmulti focus
  (fn [ctx element]
    [(::type ctx)
     (cond
       (int? element) :focus/index
       (keyword? element) :focus/attribute)]))

(defmethod focus [:find/rel :focus/index]
  [ctx index]
  {::value (-> ctx ::value (nth index))
   ::type :find/rel-entry
   ::parent ctx})

(defmethod focus [:find/coll :focus/index]
  [ctx index]
  (with-entity
    {::value (-> ctx ::value (nth index))
     ::type :find/tuple
     ::parent ctx}))

(defmethod focus [:find/tuple :focus/attribute]
  [{:keys [::value] :as ctx} attribute]
  {::value (-> value (get attribute))
   ::attribute attribute
   ::type :find/scalar
   ::schema (-> (youngest ctx ::schema) (get attribute))
   ::parent ctx})

(defmethod focus :default
  [ctx element]
  (throw
    (ex-info "Invalid focus type / element combination"
             {::type (::type ctx)
              ::element (cond
                          (int? element) :focus/index
                          (keyword? element) :focus/attribute)})))

(defn focus-in
  [ctx path]
  (loop [ctx ctx
         path path]
    (if (empty? path)
      ctx
      (recur (focus ctx (first path)) (rest path)))))

(defmulti focus*
  (fn [ctx]
    (::type ctx)))

(defmethod focus* :find/rel
  [ctx]
  (map (fn [i] (focus ctx i)) (-> ctx ::value count range)))

(defmethod focus* :find/rel-entry
  [ctx]
  (map (fn [i] (focus ctx i)) (-> ctx ::value count range)))

(defmethod focus* :find/coll
  [ctx]
  (map (fn [i] (focus ctx i)) (-> ctx ::value count range)))

(defmethod focus* :find/tuple
  [ctx]
  (into {} (map (fn [a] [a (focus ctx a)]) (-> ctx ::value keys))))

(defmethod focus* :find/scalar
  [ctx]
  (throw (ex-info "Can't focus* a scalar" {::value (::value ctx)})))

(defmethod focus* :default
  [ctx]
  (throw (ex-info "Unexpected Type" {::context ctx})))

(defn safe-deref
  [any]
  (when any @any))

(defn ctx0-adapter
  [{rt :runtime pid :partition-id :as ctx0}]
  {::runtime rt
   ::partition-id pid
   ::type :find/coll
   ::fiddle (safe-deref (:hypercrud.browser/fiddle ctx0))
   ::value (safe-deref (:hypercrud.browser/result ctx0))
   ::schema
   (assoc
    (.-schema-by-attr (safe-deref (runtime/get-schema+ rt pid "$")))
    :db/id
    #:db{:valueType {:db/ident :db.type/long} :cardinality {:db/ident :db.cardinality/one}
         :unique {:db/ident :db.unique/identity}})})
