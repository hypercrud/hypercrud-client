(ns hyperfiddle.data
  (:require
    [cats.core :refer [mlet return]]
    [cats.monad.either :refer [left right]]
    [clojure.spec.alpha :as s]
    [contrib.ct :refer [unwrap]]
    [contrib.reactive :as r]
    [cuerdas.core :as str]
    [hypercrud.browser.base :as base]
    [hypercrud.browser.context :as context]
    [hyperfiddle.fiddle]
    [hyperfiddle.api :as hf]))


(defn select-many "All links we can reach (for this entire dimension)"
  [ctx ?corcs]
  {:pre [#_(:hypercrud.browser/element ctx)                 ; database color
         (:hypercrud.browser/link-index ctx)
         (s/assert :hypercrud/context ctx)
         #_(:hypercrud.browser/eav ctx)]                    ; it can be Reaction of [nil nil nil]
   :post [#_(r/reactive? %)]}                               ; its also a vector, associative by index
  (->> (context/links-in-dimension
         ctx (contrib.data/xorxs ?corcs #{}))
       #_(map second)))

(defn validate-one+r [corcs r-links]                        ; Right[Reaction] or Left[Error] - broken, error should react too
  (let [n @(r/fmap count r-links)]
    (condp = n
      1 (right (r/fmap first r-links))
      0 (left (str/format "no match for class: %s" (pr-str corcs)))
      (left (str/format "Too many (%s) links matched for criteria: %s" n (pr-str corcs))))))

(defn ^:export select+ [ctx & [?corcs]]                     ; Right[Reaction[Link]] or Left[String]
  {:pre [(s/assert :hypercrud/context ctx)]}
  (->> (r/track select-many ctx ?corcs)
       (validate-one+r ?corcs)
       #_#_r/apply-inner-r deref))

(defn ^:export select "reactive" [ctx & [?corcs]]
  {:pre [(s/assert :hypercrud/context ctx)]
   :post [(r/reactive? %)]}
  (->> (select+ ctx ?corcs)
       (unwrap (constantly (r/pure nil)))))

(defn select-many-here' [ctx & [?corcs]]                    ; There's not a lot of value of shorting the reaction sooner than the link-index; it all must be checked
  (let [cs (contrib.data/xorxs ?corcs #{})
        a (or (context/a ctx) (hf/fiddle ctx))]
    ; links "here" means all possible links that apply. Can be lots of things!
    ; :db/id and :db/ident sys links
    ; :db/id and :db/ident parent attr links are good here too
    ; and attr tagged :db.unique/identity
    (cond
      ; If we are an identity, check here, + identity siblings, + parent
      ; In other words, go up to parent then check all child who are identity to get them all.
      (context/identity? ctx a)
      (let [ctx (context/unwind ctx 1)]
        (->> (for [[a ctx] (context/spread-attributes ctx)
                   :when (context/identity? ctx a) #_(context/attr? ctx :db.unique/identity)]
               (select-many ctx (conj cs a)))
             (sequence cat)
             (concat (select-many ctx (conj cs (context/a ctx)))) ; hf/fiddle
             doall))

      ; Otherwise just check here.
      :else
      (select-many ctx (conj cs a)))))

(defn ^:export select-many-here "reactive"                  ; collapses if eav is part of corcs
  [ctx & [?corcs]]
  {:pre [(s/assert :hypercrud/context ctx)]
   :post [(r/reactive? %)]}
  (r/track select-many-here' ctx ?corcs))

(defn ^:export select-here+ [ctx & [?corcs]]
  {:pre [(s/assert :hypercrud/context ctx)]
   :post [#_(r/reactive? %)]}                               ; Left[String]], Right[R[T]]
  (->> (select-many-here ctx ?corcs)
       (validate-one+r ?corcs)
       #_#_r/apply-inner-r deref))

(defn ^:export browse+ "Navigate a link by hydrating its context accounting for dependencies in scope.
  returns Either[Loading|Error,ctx]."
  [ctx & [?corcs]]
  {:pre [(s/assert :hypercrud/context ctx)]}
  ; No focusing, can select from root, and data-from-link manufactures a new context
  (mlet [link-ref (select+ ctx ?corcs)                      ; even one hflink can explode to two iframes
         [link-ctx route] (context/refocus-build-route-and-occlude+ ctx link-ref)]
    (->> (context/build-pid-from-link ctx link-ctx route)
         (context/set-partition ctx)
         (base/browse-partition+))))

(defn ^:export browse [ctx & [?corcs]]                      ; returns a ctx, not reactive, has reactive keys
  {:pre [(s/assert :hypercrud/context ctx)]}
  (->> (browse+ ctx ?corcs) (unwrap (constantly nil))))

(defn spread-links-here [ctx & [?corcs]]
  (->> (select-many-here ctx ?corcs)
       (r/unsequence #_:db/id hash)))                       ; hf-def fiddles don't have db/id

;(defn spread-links-in-dimension [ctx & [?corcs]]            ; rename: layer
;  (->> (r/track select-many ctx ?corcs)
;       (r/unsequence #_:db/id hash)))