(ns hyperfiddle.spec.serializer
  (:require [clojure.spec.alpha :as s]))

(defmulti serialize-spec (fn [_ node] (:type node)))

(defn serialize
  "Serialize a spec tree to clojure.spec forms (code). Returns a pair of `[<named
  definitions>, <last form on the stack>]`"
  ([node]
   (serialize () node))
  ([acc {:keys [name] :as node}]
   (let [[acc spec] (serialize-spec acc node)]
     (if name
       (let [def (list `s/def name spec)]
         [(conj acc def) spec])
       [acc spec]))))

(defmethod serialize-spec :hyperfiddle.spec/coll [acc {:keys [children args-seq]}]
  (let [{:keys [name] :as child} (first children)
        [acc spec]               (serialize acc child)
        child-spec               (or name spec)]
    [acc
     (seq (into [`s/coll-of child-spec] args-seq))]))

(defmethod serialize-spec :hyperfiddle.spec/keys [acc {:keys [children args]}]
  (let [specs (map (partial serialize (empty acc)) children)
        accs  (mapcat first specs)]
    [(reduce conj acc accs)
     (seq (into [`s/keys] (mapcat identity args)))]))

(defmethod serialize-spec :hyperfiddle.spec/and [acc {:keys [children]}]
  (let [specs (map (partial serialize (empty acc)) children)
        accs  (mapcat first specs)]
    [(reduce conj acc accs)
     (seq (into [`s/and] (map second specs)))]))

(defmethod serialize-spec :hyperfiddle.spec/predicate [acc {:keys [predicate]}]
  [acc predicate])
