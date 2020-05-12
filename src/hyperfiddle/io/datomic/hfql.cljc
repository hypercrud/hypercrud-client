(ns hyperfiddle.io.datomic.hfql
  (:require
    [datomic.api :as d]
    [backtick :refer [template]]
    [hyperfiddle.config]
    [hyperfiddle.domain]
    [clojure.spec.alpha :as s]))

(s/def :hyperfiddle/root
  (s/spec (s/+ (s/or :_ :pull/pattern
                     :hyperfiddle/query symbol?))))

(s/def :pull/pattern
  (s/or :datomic.pull/pattern :datomic.pull/pattern
        :hyperfiddle.pull/pattern :hyperfiddle.pull/pattern))

(s/def :datomic.pull/pattern
  (s/spec (s/+ :datomic.pull/attr)))

(s/def :hyperfiddle.pull/pattern
  (s/or
    :hyperfiddle.pull/pull-query-map :hyperfiddle.pull/pull-query-map
    :hyperfiddle.pull/attr+ (s/spec (s/+ :pull/attr))))

(s/def :pull/attr
  (s/or :datomic.pull/attr :datomic.pull/attr
        :hyperfiddle.pull/attr :hyperfiddle.pull/attr))

(s/def :datomic.pull/attr
  (s/or :datomic.pull/attr keyword?
        :datomic.pull/wildcard :datomic.pull/wildcard
        :datomic.pull/map :datomic.pull/map
        :datomic.pull/attr-expr :datomic.pull/attr-expr))

(s/def :hyperfiddle.pull/attr
  (s/or :hyperfiddle.pull/map :hyperfiddle.pull/map
        :hyperfiddle.pull/pull-query-map :hyperfiddle.pull/pull-query-map))

(s/def :datomic.pull/wildcard
  #{"*" '*})


(s/def :hyperfiddle.pull/pull-query-map
  (s/map-of (s/or :attr symbol?
                  :limit-expr :datomic.pull/limit-expr)
            (s/or :_ :pull/pattern
                  :recursion-limit :datomic.pull/recursion-limit
                  :_ nil?)
            :conform-keys true))

(s/def :pull/map
  (s/or :hyperfiddle.pull/map :hyperfiddle.pull/map
        :datomic.pull/map :datomic.pull/map))

(s/def :hyperfiddle.pull/map
  (s/map-of (s/or :attr keyword?
                  :limit-expr :datomic.pull/limit-expr)
            (s/or :_ :pull/pattern
                  :recursion-limit :datomic.pull/recursion-limit)
            :conform-keys true))

(s/def :datomic.pull/map
  (s/map-of (s/or :attr keyword?
                  :limit-expr :datomic.pull/limit-expr)
            (s/or :_ :datomic.pull/pattern
                  :recursion-limit :datomic.pull/recursion-limit)
            :conform-keys true))

(s/def :datomic.pull/attr-expr
  (s/or :limit-expr :datomic.pull/limit-expr
        :default-expr :datomic.pull/default-expr))

(s/def :datomic.pull/limit-expr
  (s/cat :key #{"limit" 'limit}
         :attr :datomic.pull/attr-name
         :limit (s/alt :pos-int pos-int?
                       :nil nil?)))

(s/def :datomic.pull/default-expr
  (s/cat :key #{"default" 'default}
         :attr :datomic.pull/attr-name
         :val any?))

(s/def :datomic.pull/recursion-limit
  (s/or :pos-int pos-int?
        :ellipsis #{'...}))

; -------------------------------------------------------------------------------------------------
; PARSING                                                                                       |||
;                                                                                               |||
;                                                                                               |||
; -------------------------------------------------------------------------------------------------

(defrecord Result [hyperfiddle datomic])

(defn unweave
  [type acc v]
  (if (instance? Result v)
    (-> acc
        (update :datomic conj (:datomic v))
        (update :hyperfiddle conj (:hyperfiddle v)))
    ;(-> acc
    ;    (update :datomic (partial m :datomic) (:datomic v))
    ;    (update :hyperfiddle (partial m :hyperfiddle) (:hyperfiddle v)))
    (update acc type conj v)))


;; All of these keys are applied to identity when transforming AST
(def identities
  #{:pull/pattern
    :datomic.pull/pattern
    :hyperfiddle.pull/pattern
    :pull/attr+
    :datomic.pull/attr
    :hyperfiddle.pull/attr
    :datomic.pull/wildcard
    :pull/map-entry
    :datomic.pull/attr-expr
    :datomic.pull/limit-expr
    :datomic.pull/default-expr
    :datomic.pull/recursion-limit
    :attr
    :_})

(def transformer-map
  {:hyperfiddle/query
   (fn [sym]
     {:as sym
      :query (-> sym resolve deref)})

   :hyperfiddle.pull/attr+
   {:pre
    (fn [pull-attrs]
      (let [[dt hf] (partition-by #(= (first %) :hyperfiddle.pull/attr) pull-attrs)]
        [:hyperfiddle.pull/attr+
         (map->Result
           {:hyperfiddle (vec hf)
            :datomic     (vec dt)})]))

    :post
    (fn [{:keys [hyperfiddle datomic]}]
      (reduce
        (partial unweave :hyperfiddle)
        (reduce (partial unweave :datomic) (map->Result {:hyperfiddle [] :datomic []}) datomic)
        hyperfiddle))}

   :hyperfiddle.pull/pull-query-map
   (fn [m]
     ()
     (let [pull (-> m vals first)]
       (merge
         {:as    (-> m keys first)
          :query (-> m keys first resolve deref)}
         (cond
           (map? pull)
           {:pull         (:datomic pull)
            :dependencies (reduce merge (:hyperfiddle pull))}

           pull
           {:pull pull}

           :else
           {}))))

   :hyperfiddle.pull/map
   (fn [m]
     (reduce
       (partial merge-with merge)
       (map->Result
         {:hyperfiddle {}
          :datomic     {}})
       (map
         (fn [[k {:keys [hyperfiddle datomic]}]]
           {:hyperfiddle [k hyperfiddle]
            :datomic     [k datomic]})
         m)))

   :datomic.pull/map
   (fn [x] (map->Result {:datomic x}))

   :map-entry
   (fn [{:keys [key val]}] {key val})})


(def transformer
  (into transformer-map (map (fn [k] [k identity]) identities)))

(defn step-transform
  [transformer step form]
  (clojure.walk/postwalk
    (fn [form]
      (if (vector? form)
        (let [[tag & args :as form] form]
          (if-let [transformer (get-in transformer [tag step])]
            (apply transformer args)
            form))
        form))
    form))

(defn transform
  [transformer form]
  (let [transformer (into {} (map (fn [[k v]] [k (if (map? v) v {:post v})]) transformer))]
    (->> form
         (step-transform transformer :pre)
         (step-transform transformer :post))))

; -------------------------------------------------------------------------------------------------
; PREPROCESSING                                                                                 |||
;                                                                                               |||
;                                                                                               |||
; -------------------------------------------------------------------------------------------------;

; this is to remove cases of (:db/ident {:hf/tx :hf/retract}) from being parsed by datomic
; TODO investigate ambiguity with function invocations
; TODO something smarter

(defn first-on-lists
  [intermediate]
  (clojure.walk/prewalk
    (fn [v] (if (list? v)
              (first v)
              v))
    intermediate))

(defn preprocess
  [intermediate]
  (-> intermediate
      first-on-lists))

; -------------------------------------------------------------------------------------------------
; PARSING                                                                                       |||
;                                                                                               |||
;                                                                                               |||
; -------------------------------------------------------------------------------------------------

(defn parse
  [hfql]
  (->> hfql (s/conform :hyperfiddle/root) (transform transformer) preprocess))

; -------------------------------------------------------------------------------------------------
; INTERPRETING                                                                                  |||
;                                                                                               |||
;                                                                                               |||
; -------------------------------------------------------------------------------------------------

(defn datomic-pull-to-lookup-ref [pulled-tree]
  ; TODO do something smart to compute a datomic identity
  (:db/ident pulled-tree))

(defn interpret
  [$ forms & args]
  (if (vector? forms)
    (mapv (partial interpret $) forms)
    (let [{:keys [as query pull dependencies]} forms
          base (->> args (apply query $))
          base (if pull (d/pull-many $ pull base) base)]
      {as (reduce
            (fn [acc [path form]]
              (mapv
                (fn [v]
                  (reduce
                    (fn [acc form]
                      (let [built (interpret $ form (datomic-pull-to-lookup-ref (get v path)))]
                        (update acc path merge built)))
                    v
                    form))
                acc))
            base
            dependencies)})))