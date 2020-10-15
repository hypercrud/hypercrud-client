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

(declare generate*)

(defn generate
  [seed & args]
  (binding [*seed* seed]
    (if (bound? #'identities)
      (gen! (apply generate* args))
      (binding [identities (atom {})]
        (gen! (apply generate* args))))))

(defmulti generate*
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

(defmethod generate* ::gen [gen] gen) ; identity

(defmethod generate* ::vector
  [type & args]
  (let [[type & args] type]
    (apply generate* type args)))

(defmethod generate* :constant
  [_ k]
  (gen/return k))

(defmethod generate* :'
  [_ k]
  (gen/return k))

(defmethod generate* :constant*
  [_ & k]
  (gen/return [k]))

;; (defmethod generate* :list
;;   [_ & [v & vals]]
;;   (if (nil? v)
;;     (gen/return ())
;;     (gen/fmap #(cons (generate* v) %)
;;               (generate* :list vals))))

(defmethod generate* :?
  [_ type]
  (gen/bind gen/boolean
            (fn [coin] ;; coin flip
              (if (true? coin)
                (generate* type)
                (gen/return nil)))))

(defmethod generate* :*
  [_ n type]
  (gen/vector (generate* type) n))

(defmethod generate* :*!                                    ; pick distinct
  [_ n type]
  (gen/fmap #(into [] (distinct) %)
            (generate* [:* n type])))

(defmethod generate* :keyword
  [_]
  (gen/fmap keyword (generate* :word)))

(defmethod generate* :symbol
  [_]
  (gen/fmap symbol (generate* :word)))

(defmethod generate* :string
  [_ & [n]]
  (gen/fmap #(str/join \space %)
            (generate* :* (or n 10) :word)))

(defmethod generate* :boolean
  [_]
  gen/boolean)

(defmethod generate* :long
  [_ & [min max]]
  (if (or min max)
    (gen/large-integer* (cond-> {}
                          min (assoc :min min)
                          max (assoc :max max)))
    gen/large-integer))

(defmethod generate* :bigint
  [_]
  gen/size-bounded-bigint)

(defmethod generate* :double
  [_ & [min max]]
  (gen/double* (cond-> {:NaN?      false
                        :infinite? false}
                 min (assoc :min min)
                 max (assoc :max max))))

(defmethod generate* :bigdec
  [_]
  (fapply #(java.math.BigDecimal. (/ %1 %2))
          (generate* :double)
          (generate* :double 1) ;; avoid divide by 0
          ))

(defmethod generate* :ident
  [_ class as type & args]
  (gen/fmap (fn [ident]
              (swap! identities update class (comp set conj) (if (= :db/id as) ident [as ident]))
              ident)
            (apply generate* type args)))

(defmethod generate* :ref
  [_ class]
  (gen/elements (or (get @identities class) [nil])))

(defmethod generate* :instant
  [_]
  ;; GG Not a good generator. It’s supposed to generate random date in the last
  ;; week. Good enough for now.
  (let [now-time (long (or *seed* (.getTime (java.util.Date.)) ))
        now      (tc/from-long now-time)]
    (s/gen (s/inst-in (-> now (t/minus (t/weeks 1)))
                      now))))

(defmethod generate* :uuid
  [_]
  gen/uuid)

(defmethod generate* :uri
  [_]
  (s/gen uri?))

(let [random-names (clojure.string/split-lines (load-resource "generators/names.txt"))]
  (defmethod generate* :name
    [_]
    (gen/elements random-names)))

(let [random-words (clojure.string/split-lines (load-resource "generators/words.txt"))]
  (defmethod generate* :word
    [_]
    (gen/elements random-words)))

(defmethod generate* :street
  [_]
  (gen/bind (gen/elements #{"Rd." "Ave." "Blvd." "St."})
            (fn [prefix]
              (gen/fmap (fn [words]
                          (str prefix \space (str/join \space words)))
                        (generate* :* 2 :word)))))


(let [random-words (clojure.string/split-lines (load-resource "generators/cities.txt"))]
  (defmethod generate* :city
    [_]
    (gen/elements random-words)))

(defmethod generate* :state
  [_]
  (gen/elements #{"Alabama" "Alaska" "American Samoa" "Arizona" "Arkansas" "California" "Colorado" "Connecticut" "Delaware" "District of Columbia" "Florida" "Georgia" "Guam" "Hawaii" "Idaho" "Illinois" "Indiana" "Iowa" "Kansas" "Kentucky" "Louisiana" "Maine" "Maryland" "Massachusetts" "Michigan" "Minnesota" "Mississippi" "Missouri" "Montana" "Nebraska" "Nevada" "New Hampshire" "New Jersey" "New Mexico" "New York" "North Carolina" "North Dakota" "Northern Mariana Islands"}))

(defmethod generate* :state-abbrev
  [_]
  (gen/elements #{"AK" "AL" "AR" "AZ" "CA" "CO" "CT" "DC" "DE" "FL" "GA" "HI" "IA" "ID" "IL" "IN" "KS" "KY" "LA" "MA" "MD" "ME" "MI" "MN" "MO" "MS" "MT" "NC" "ND" "NE" "NH" "NJ" "NM" "NV" "NY" "OH" "OK" "OR" "PA" "PR" "RI" "SC" "SD" "TN" "TX" "UT" "VA" "VT" "WA" "WI" "WV" "WY"}))

(defmethod generate* :address
  [_]
  (gen/let [num    (generate* :long 0 999)
            street (generate* :street)
            city   (generate* :city)
            state  (generate* :state)
            code   (generate* :long 0 99999)]
    (str num \space street \space city \, \space state \space code)))

(defmethod generate* :email
  [_]
  (gen/let [name   (generate* :word)
            domain (generate* :word)
            tld    (gen/elements #{"com" "edu" "org" "net"})]
    (str name "@" domain \. tld)))

(defmethod generate* :phone
  [_]
  (gen/let [prefix (generate* :long 999)
            a      (generate* :long 999)
            b      (generate* :long 9999)]
    (str "(" prefix ")-" a "-" b)))

(defmethod generate* :ordinal-number
  [_ & [left right]]
  (gen/let [n (generate* :long left right)]
    (str n (condp = (mod n 10)
             1 "st"
             2 "nd"
             3 "rd"
             "th"))))

(defmethod generate* ::fn
  [f]
  (gen/return (f)))

(defmethod generate* ::map
  [type]
  (->> type
       (mapcat (fn [[k v]]
                 (when-let [v (generate* v)]
                   (if (gen/generator? v)
                     [k v]
                     [k (gen/return v)]))))
       (apply gen/hash-map)
       (gen/fmap #(data/filter-vals some? %))))

(defmethod generate* ::set
  [type]
  (gen/elements type))
