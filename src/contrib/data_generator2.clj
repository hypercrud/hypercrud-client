(ns contrib.data-generator2
  (:require [clj-time.coerce :as tc]
            [clj-time.core :as t]
            [clojure.spec.alpha :as s]
            [clojure.string :as str]
            [clojure.test.check.generators :as gen]
            [contrib.data :as data]
            [contrib.template :refer [load-resource]]))

(defn fapply [af & avs]
  (gen/fmap #(apply af %) (apply gen/tuple avs)))

; accumulate idents generated for later lookups
; :ref directive will lookup by ident and get a random one.
(def ^:dynamic identities)
(def ^:dynamic *seed*)

(defn seed
  "Generate a useful seed based on the current UTC time. Use it to get
  deterministic dates in an acceptable time range (around today). If you don’t
  care about dates, you don’t need this function as any integer will do."
  []
  (.getTime (java.util.Date.)))

(defn- gen!
  ([generator]
   (gen! generator *seed*))
  ([generator seed] ; allow seed overload, like (inc seed) in case of conflict
   (if seed
     (gen/generate generator 50 *seed*)
     (gen/generate generator))))

(declare generator)

(defn generate
  [seed & args]
  (binding [*seed* seed]
    (if (bound? #'identities)
      (gen! (apply generator args))
      (binding [identities (atom {})]
        (gen! (apply generator args))))))

(defmulti generator
  (fn [type & _]
    (cond
      (gen/generator? type)
      ::gen

      (fn? type)
      ::fn

      (map? type)
      ::map

      (vector? type)
      ::vector

      (set? type)
      ::set

      :else
      type)))

(defmethod generator ::gen [gen] gen) ; identity

(defmethod generator ::vector
  [type & args]
  (let [[type & args] type]
    (apply generator type args)))

(defmethod generator :constant
  [_ k]
  (gen/return k))

(defmethod generator :'
  [_ k]
  (gen/return k))

(defmethod generator :constant*
  [_ & k]
  (gen/return [k]))

;; (defmethod generator :list
;;   [_ & [v & vals]]
;;   (if (nil? v)
;;     (gen/return ())
;;     (gen/fmap #(cons (generator v) %)
;;               (generator :list vals))))

(defmethod generator :?
  [_ type]
  (gen/bind gen/boolean
            (fn [coin] ;; coin flip
              (if (true? coin)
                (generator type)
                (gen/return nil)))))

(defmethod generator :*
  [_ n type]
  (gen/vector (generator type) n))

(defmethod generator :*!                                    ; pick distinct
  [_ n type]
  (gen/fmap #(into [] (distinct) %)
            (generator [:* n type])))

(defmethod generator :keyword
  [_]
  (gen/fmap keyword (generator :word)))

(defmethod generator :symbol
  [_]
  (gen/fmap symbol (generator :word)))

(defmethod generator :string
  [_ & [n]]
  (gen/fmap #(str/join \space %)
            (generator :* (or n 10) :word)))

(defmethod generator :boolean
  [_]
  gen/boolean)

(defmethod generator :long
  [_ & [min max]]
  (if (or min max)
    (gen/large-integer* (cond-> {}
                          min (assoc :min min)
                          max (assoc :max max)))
    gen/large-integer))

(defmethod generator :bigint
  [_]
  gen/size-bounded-bigint)

(defmethod generator :double
  [_ & [min max]]
  (gen/double* (cond-> {:NaN?      false
                        :infinite? false}
                 min (assoc :min min)
                 max (assoc :max max))))

(defmethod generator :bigdec
  [_ & [min max]]
  (if (or min max)
    (gen/fmap #(java.math.BigDecimal. %)
              (generator [:double min max]))
    (fapply #(java.math.BigDecimal. (/ %1 %2))
            (generator :double)
            (generator :double 1) ;; avoid divide by 0
            )))

(defmethod generator :ident
  [_ class as type & args]
  (gen/fmap (fn [ident]
              (swap! identities update class (comp set conj) (if (= :db/id as) ident [as ident]))
              ident)
            (apply generator type args)))

(defmethod generator :ref
  [_ class]
  (gen/bind (gen/return nil)
            (fn [_]
              (gen/elements (or (get @identities class) [nil])))))

(defmethod generator :instant
  [_]
  ;; GG Not a good generator. It’s supposed to generate random date in the last
  ;; week. Good enough for now.
  (let [now-time (let [now-time (.getTime (java.util.Date.))]
                   (long
                    (if (bound? #'*seed*)
                      (or *seed* now-time) ; might be bound to nil
                      now-time)))
        now      (tc/from-long now-time)]
    (s/gen (s/inst-in (-> now (t/minus (t/weeks 1)))
                      now))))

(defmethod generator :uuid
  [_]
  gen/uuid)

(defmethod generator :uri
  [_]
  (s/gen uri?))

(let [random-names (clojure.string/split-lines (load-resource "generators/names.txt"))]
  (defmethod generator :name
    [_]
    (gen/elements random-names)))

(let [random-words (clojure.string/split-lines (load-resource "generators/words.txt"))]
  (defmethod generator :word
    [_]
    (gen/elements random-words)))

(defmethod generator :street
  [_]
  (gen/bind (gen/elements #{"Rd." "Ave." "Blvd." "St."})
            (fn [prefix]
              (gen/fmap (fn [words]
                          (str prefix \space (str/join \space words)))
                        (generator :* 2 :word)))))


(let [random-words (clojure.string/split-lines (load-resource "generators/cities.txt"))]
  (defmethod generator :city
    [_]
    (gen/elements random-words)))

(defmethod generator :state
  [_]
  (gen/elements #{"Alabama" "Alaska" "American Samoa" "Arizona" "Arkansas" "California" "Colorado" "Connecticut" "Delaware" "District of Columbia" "Florida" "Georgia" "Guam" "Hawaii" "Idaho" "Illinois" "Indiana" "Iowa" "Kansas" "Kentucky" "Louisiana" "Maine" "Maryland" "Massachusetts" "Michigan" "Minnesota" "Mississippi" "Missouri" "Montana" "Nebraska" "Nevada" "New Hampshire" "New Jersey" "New Mexico" "New York" "North Carolina" "North Dakota" "Northern Mariana Islands"}))

(defmethod generator :state-abbrev
  [_]
  (gen/elements #{"AK" "AL" "AR" "AZ" "CA" "CO" "CT" "DC" "DE" "FL" "GA" "HI" "IA" "ID" "IL" "IN" "KS" "KY" "LA" "MA" "MD" "ME" "MI" "MN" "MO" "MS" "MT" "NC" "ND" "NE" "NH" "NJ" "NM" "NV" "NY" "OH" "OK" "OR" "PA" "PR" "RI" "SC" "SD" "TN" "TX" "UT" "VA" "VT" "WA" "WI" "WV" "WY"}))

(defmethod generator :address
  [_]
  (gen/let [num    (generator :long 0 999)
            street (generator :street)
            city   (generator :city)
            state  (generator :state)
            code   (generator :long 0 99999)]
    (str num \space street \space city \, \space state \space code)))

(defmethod generator :email
  [_]
  (gen/let [name   (generator :word)
            domain (generator :word)
            tld    (gen/elements #{"com" "edu" "org" "net"})]
    (str name "@" domain \. tld)))

(defmethod generator :phone
  [_]
  (gen/let [prefix (generator :long 999)
            a      (generator :long 999)
            b      (generator :long 9999)]
    (str "(" prefix ")-" a "-" b)))

(defmethod generator :ordinal-number
  [_ & [left right]]
  (gen/let [n (generator :long left right)]
    (str n (condp = (mod n 10)
             1 "st"
             2 "nd"
             3 "rd"
             "th"))))

(defmethod generator ::fn
  [f]
  (gen/return (f)))

(defmethod generator ::map
  [type]
  (->> type
       (mapcat (fn [[k v]]
                 (when-let [v (generator v)]
                   (if (gen/generator? v)
                     [k v]
                     [k (gen/return v)]))))
       (apply gen/hash-map)
       (gen/fmap #(data/filter-vals some? %))))

(defmethod generator ::set
  [type]
  (gen/elements type))
