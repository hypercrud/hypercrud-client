(ns contrib.reactive
  (:refer-clojure :exclude [atom comp constantly partial])
  (:require [cats.core :as cats]
            [contrib.data :as util]
    #?(:cljs [reagent.core :as reagent])
    #?(:cljs [reagent.ratom :refer [IReactiveAtom]]))
  #?(:cljs (:require-macros [contrib.reactive]))
  #?(:clj
     (:import (clojure.lang IAtom IDeref))))


; reactivity is currently never needed on the jvm
; so fill their implementations with naive/unreactive implementations

(defn reactive? [v]
  #?(:clj  (instance? IDeref v)
     :cljs (satisfies? IReactiveAtom v)))

(defn atom [x & rest]
  (apply #?(:clj clojure.core/atom :cljs reagent/atom) x rest))

(defn cursor [src path]
  {:pre [(reactive? src)]}
  ; todo support more than just IDeref
  #?(:clj  (delay (get-in @src path))
     :cljs (reagent/cursor src path)))

(defn partial [f & args]
  (apply #?(:clj clojure.core/partial :cljs reagent/partial) f args))

(defn last-arg-first [f & args]
  (apply f (last args) (drop-last 1 args)))

#?(:cljs
   (deftype Constantly [v]
     Fn
     IFn
     (-invoke [_] v)
     (-invoke [_ a] v)
     (-invoke [_ a b] v)
     (-invoke [_ a b c] v)
     (-invoke [_ a b c d] v)
     (-invoke [_ a b c d e] v)
     (-invoke [_ a b c d e f] v)
     (-invoke [_ a b c d e f g] v)
     (-invoke [_ a b c d e f g h] v)
     (-invoke [_ a b c d e f g h i] v)
     (-invoke [_ a b c d e f g h i j] v)
     (-invoke [_ a b c d e f g h i j k] v)
     (-invoke [_ a b c d e f g h i j k l] v)
     (-invoke [_ a b c d e f g h i j k l m] v)
     (-invoke [_ a b c d e f g h i j k l m n] v)
     (-invoke [_ a b c d e f g h i j k l m n o] v)
     (-invoke [_ a b c d e f g h i j k l m n o p] v)
     (-invoke [_ a b c d e f g h i j k l m n o p q] v)
     (-invoke [_ a b c d e f g h i j k l m n o p q r] v)
     (-invoke [_ a b c d e f g h i j k l m n o p q r s] v)
     (-invoke [_ a b c d e f g h i j k l m n o p q r s t] v)
     (-invoke [_ a b c d e f g h i j k l m n o p q r s t rest] v)
     IEquiv
     (-equiv [_ other] (and (instance? Constantly other) (= v (.-v other))))
     IHash
     (-hash [_] (hash v))))

(defn constantly [v]
  #?(:clj  (clojure.core/constantly v)
     :cljs (->Constantly v)))

#?(:cljs
   (deftype Comp [cf fs]
     Fn
     IFn
     (-invoke [_]
       (cf))
     (-invoke [_ a]
       (cf a))
     (-invoke [_ a b]
       (cf a b))
     (-invoke [_ a b c]
       (cf a b c))
     (-invoke [_ a b c d]
       (cf a b c d))
     (-invoke [_ a b c d e]
       (cf a b c d e))
     (-invoke [_ a b c d e f]
       (cf a b c d e f))
     (-invoke [_ a b c d e f g]
       (cf a b c d e f g))
     (-invoke [_ a b c d e f g h]
       (cf a b c d e f g h))
     (-invoke [_ a b c d e f g h i]
       (cf a b c d e f g h i))
     (-invoke [_ a b c d e f g h i j]
       (cf a b c d e f g h i j))
     (-invoke [_ a b c d e f g h i j k]
       (cf a b c d e f g h i j k))
     (-invoke [_ a b c d e f g h i j k l]
       (cf a b c d e f g h i j k l))
     (-invoke [_ a b c d e f g h i j k l m]
       (cf a b c d e f g h i j k l m))
     (-invoke [_ a b c d e f g h i j k l m n]
       (cf a b c d e f g h i j k l m n))
     (-invoke [_ a b c d e f g h i j k l m n o]
       (cf a b c d e f g h i j k l m n o))
     (-invoke [_ a b c d e f g h i j k l m n o p]
       (cf a b c d e f g h i j k l m n o p))
     (-invoke [_ a b c d e f g h i j k l m n o p q]
       (cf a b c d e f g h i j k l m n o p q))
     (-invoke [_ a b c d e f g h i j k l m n o p q r]
       (cf a b c d e f g h i j k l m n o p q r))
     (-invoke [_ a b c d e f g h i j k l m n o p q r s]
       (cf a b c d e f g h i j k l m n o p q r s))
     (-invoke [_ a b c d e f g h i j k l m n o p q r s t]
       (cf a b c d e f g h i j k l m n o p q r s t))
     (-invoke [_ a b c d e f g h i j k l m n o p q r s t rest]
       (apply cf a b c d e f g h i j k l m n o p q r s t rest))
     IEquiv
     (-equiv [_ other] (and (instance? Comp other) (= fs (.-fs other))))
     IHash
     (-hash [_] (hash fs))))

#?(:cljs (defn comp
           ([] identity)
           ([f] f)
           ([f & fs] (->Comp (apply clojure.core/comp f fs) (cons f fs))))
   :clj  (def comp clojure.core/comp))

(defn track [f & args]
  ; todo support more than just IDeref
  #?(:clj  (delay (apply f args))
     :cljs (apply reagent/track f args)))

(defn fmap [f rv]
  {:pre [f]}
  (assert (reactive? rv) (str "fmap rv: " rv " did you pass nil instead of (r/atom nil)?"))
  (track (comp f deref) rv))

(defn >>= [fr rv]
  (track (comp deref fr deref) rv))

(defmacro fmap-> [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(fmap (partial last-arg-first ~@form) ~x) (meta form))
                       `(fmap ~form ~x))]
        (recur threaded (next forms)))
      x)))

(defmacro fmap->> [x & forms]
  (loop [x x, forms forms]
    (if forms
      (let [form (first forms)
            threaded (if (seq? form)
                       (with-meta `(fmap (partial ~@form) ~x) (meta form))
                       `(fmap ~form ~x))]
        (recur threaded (next forms)))
      x)))

(letfn [(-fapply [rf rv] (@rf @rv))]
  (defn fapply [rf & rvs]
    {:pre [(reactive? rf) (every? reactive? rvs)]}
    (reduce (partial track -fapply) rf rvs)))

; Reactive[Monad[_]] => Reactive[Monad[Reactive[_]]]
; useful for reacting on the Either (left v right), but not the Right's value
; this should probably fall out eventually
(let [f (fn [rmv] (cats/fmap (clojure.core/constantly (fmap cats/extract rmv)) @rmv))]
  (defn apply-inner-r [rmv]
    (track f rmv)))

(defn unsequence
  "Expand a reference of a list into a list of references while maintaining order.

  If `key-fn` is provided, the children cursors will be pathed by the provided key-fn, NOT index.
  This is useful when the child cursors' references must be consistent across reorderings (which index does not provide).
  Like track's and fmap's `f`, `key-fn` MUST be stable across invocations to provide stable child references."
  ([rv]
   {:pre [(reactive? rv)]}
   (assert @(fmap #(or (vector? %) (nil? %)) rv) "unsequencing by index requires vector input, maybe try using a key like :db/id?")
   (->> (range @(fmap count rv))
        ; cursur indexing by index silently fails if @rv is a list here
        (map (fn [index] [(cursor rv [index]) index]))))
  ([key-fn rv]
   {:pre [(reactive? rv)]}
   (let [lookup (fmap (partial util/group-by-unique key-fn) rv)]
     (->> @(fmap (partial map key-fn) rv)
          (map (fn [key] [(cursor lookup [key]) key]))))))
