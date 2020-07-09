(ns contrib.string
  (:require
    [clojure.string :as string]
    [contrib.data :refer [orp pad]]
    [cuerdas.core :as str]))


;(defn split-last [s sep]
;  (let [[x & xs] (str/split (reverse s) sep)]
;    [x (reverse (str/join xs))]))
;
;(comment
;  (split-last "asdf#frag" "#")
;  (split-last "asdf#frag" "#"))

(defn empty->nil [s]
  (if (str/empty-or-nil? s) nil s))

(defn blank->nil
  "Nullify empty strings, identity on all other values."
  [s]
  (if (string? s)
    (when (not (str/blank? s))
      s)
    s))

(comment
  (merge-with #(or (blank->nil %1) %2)
              {:a "a" :b nil :d ""}
              {:a "aaa" :b "bbb" :c "ccc" :d "ddd"})
  (merge-with #(orp (complement blank->nil) %1 %2)
              {:a "a" :b nil :d ""}
              {:a "aaa" :b "bbb" :c "ccc" :d "ddd"})

  (orp (complement blank->nil) "" "aaa")
  )

(defn split-first [s sep]
  (if (nil? s)
    [nil nil]
    (let [[a b] (string/split s sep 2)]
      [(empty->nil a) (empty->nil b)])))

(defn abc []
  (map (comp keyword str) "abcdefghijklmnopqrstuvwxyz")     ; this version works in clojurescript
  #_(->> (range) (map (comp keyword str char #(+ % (int \a))))))

(defn or-str
  #_([& args] (apply orp seq args))                         ; can't apply macro todo
  ([a b] (orp seq a b))
  ([a b c] (orp seq a b c)))

(defn lpad-str [n zero s]
  (str (apply str (repeat (- n (count s)) zero)) s))

(defn str-last-n [n s]
  #?(:clj (.substring s (max 0 (- (.length s) n)))
     :cljs (apply str (reverse (take n (reverse s))))))
