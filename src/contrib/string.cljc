(ns contrib.string
  (:require
    [clojure.string :as string :refer [join split]]
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

(defn blank->nil [s]
  (if (and (string? s)
           (not (str/blank? s)))
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

(defn or-str [a b] (orp seq a b))

#?(:cljs                                                    ; todo clj
   (defn lpad-str [n zero s]
     (-> s
         (split "")
         reverse                                            ; make it a left pad
         (->> (pad 2 zero))                                 ; this is a right-pad
         reverse
         join)))

(defn str-last-n [n s]
  #?(:clj (.substring s (max 0 (- (.length s) n)))
     :cljs (apply str (reverse (take n (reverse s))))))
