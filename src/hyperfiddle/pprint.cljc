(ns hyperfiddle.pprint
  (:require
    clojure.pprint
    [cats.core :as cats]
    [cats.monad.either :refer [#?@(:cljs [Left Right])]]
    [cats.monad.exception :refer [#?@(:cljs [Failure Success])]]
    [contrib.datomic :refer [#?(:cljs Schema)]]
    #?(:cljs [reagent.ratom]))
  #?(:clj
     (:import
       [cats.monad.either Left Right]
       [cats.monad.exception Failure Success]
       [contrib.datomic Schema])))


(defn pprint-cats-extractable [^String tag o]
  (#?(:clj .write :cljs -write) *out* tag)
  (#?(:clj .write :cljs -write) *out* " ")
  (clojure.pprint/write-out (cats/extract o)))

#?(:cljs
   (defn pprint-ratom [o s]
     (clojure.pprint/pprint-logical-block
       :prefix (str "#<" s " ") :suffix ">"
       (binding [#?@(:cljs [reagent.ratom/*ratom-context* nil])]
         (clojure.pprint/write-out (deref o))))))

(defn simple-dispatch [o]
  (condp = (type o)
    #?@(:cljs
        [reagent.ratom/RAtom (pprint-ratom o "Atom:")
         reagent.ratom/Track (pprint-ratom o "Track:")
         reagent.ratom/RCursor (pprint-ratom o "Cursor:")
         reagent.ratom/Reaction (pprint-ratom o (str "Reaction " (hash o) ":"))
         reagent.ratom/Wrapper (pprint-ratom o "Wrap:")])
    Right (pprint-cats-extractable "#right" o)
    Left (pprint-cats-extractable "#left" o)
    Failure (pprint-cats-extractable "#failure" o)
    Success (pprint-cats-extractable "#success" o)
    Schema (do (#?(:clj .write :cljs -write) *out* "#schema")
               (clojure.pprint/write-out (.-schema-by-attr #?(:clj o, :cljs ^js o))))
    (clojure.pprint/simple-dispatch o)))

(comment
  (binding [clojure.pprint/*print-pprint-dispatch* hyperfiddle.pprint/simple-dispatch]
    (clojure.pprint/pprint '...)))
