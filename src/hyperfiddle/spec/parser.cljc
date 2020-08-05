(ns hyperfiddle.spec.parser
  (:require [clojure.spec.alpha :as s]))

(defn spec-name [spec]
  (::s/name (meta spec)))

(defmulti parse-spec first)

(defn parse
  "Parse a potentially nested spec to a tree structure where nodes might have
  names and leaves are predicates.
  TODO: support recursive specs
  TODO: support s/or and s/and"
  [spec]
  (when spec
    (if (s/spec? spec)
      (parse-spec (list `s/def (spec-name spec) (s/form spec)))
      (if-let [spec (s/get-spec spec)]
        (parse spec)
        (if (seq? spec)
          (parse-spec spec)
          (parse-spec (list spec)))))))

(defmethod parse-spec `s/def [[_ name value]]
  (merge {:name name}
         (parse value)))

(defmethod parse-spec `s/fspec [[_ & args-seq]]
  (let [{:keys [args ret]} args-seq]
    {:type     :hyperfiddle.spec/fn
     :args-seq args-seq
     :args     (some-> args parse)
     :ret      (some-> ret parse)}))

(defmethod parse-spec `s/coll-of [[_ name & args-seq]]
  (let [{:keys [] :as args-map} args-seq]
    {:type     :hyperfiddle.spec/coll
     :args     args-map
     :args-seq args-seq
     :children [(parse name)]}))

(defmethod parse-spec `s/keys [[_ & {:keys [req req-un opt opt-un] :as args}]]
  (let [keys (distinct (concat req req-un opt opt-un))]
    {:type     :hyperfiddle.spec/keys
     :keys     (set keys)
     :args     args
     :children (mapv parse keys)}))

(defmethod parse-spec `s/cat [[_ & kvs]]
  (let [names (partition 2 kvs)]
    {:type     :hyperfiddle.spec/cat
     :names    (map first names)
     :args     kvs
     :count    (count names)
     :children (map (fn [[name spec]]
                      (assoc (parse spec) :name name))
                    names)}))

(defmethod parse-spec `s/alt [[_ & kvs]]
  (let [names (partition 2 kvs)]
    {:type     :hyperfiddle.spec/alt
     :names    (map first names)
     :args     kvs
     :count    (count names)
     :children (map (fn [[name spec]]
                      (assoc (parse spec) :name name))
                    names)}))

(defmethod parse-spec `s/and [[_ & children]]
  {:type     :hyperfiddle.spec/and
   :children (map parse children)})

; no spec defaults to predicate
; spec with {:args ()} defaults to predicate

(defmethod parse-spec :default [form]
  {:type      :hyperfiddle.spec/predicate
   :predicate (first form)})
