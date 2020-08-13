(ns contrib.data-generator
  (:require
   [contrib.template :refer [load-resource]]))

(defmulti generate*
  (fn [type & _]
    (cond
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

(defmethod generate* ::vector
  [type & args]
  (let [[type & args] type]
    (if (fn? type)
      (apply type (map generate* args))
      (apply generate* type args))))

(defmethod generate* :constant
  [_ k]
  k)

(defmethod generate* :'
  [_ k]
  k)

(defmethod generate* :constant*
  [_ & k]
  (vec k))

(defmethod generate* :list
  [_ & vals]
  (into [] (map generate* vals)))

(defmethod generate* :?
  [_ type]
  (when (generate* :boolean)
    (generate* type)))

(defn generate*|list
  [n type]
  (into [] (repeatedly n #(generate* type))))

(defmethod generate* :*
  [_ n type]
  (into [] (remove nil? (generate*|list n type))))

(defmethod generate* :*!
  [_ n type]
  (into [] (distinct (generate*|list n type))))

(defmethod generate* :keyword
  [_]
  (keyword (generate* :word)))

(defmethod generate* :symbol
  [_]
  (symbol (generate* :word)))

(defmethod generate* :string
  [_ & [n]]
  (apply str (interpose \space (generate* [:* (generate* :long (or n 10)) :word]))))

(defmethod generate* :boolean
  [_]
  (> (rand) 0.5))

(defmethod generate* :long
  [_ & [left right]]
  (let [right' right
        right (or right left 100)
        left (if right' left 0)]
    (+ (long (rand right)) left)))

(defmethod generate* :bigint
  [_]
  (*' 10 (generate* :long Long/MIN_VALUE Long/MAX_VALUE)))

(defmethod generate* :float
  [_ & [left right]]
  (let [right' right
        right (or right left 100)
        left (if right' left 0)]
    (+ (* (rand) right) left)))

(defmethod generate* :bigdec
  [_]
  (java.math.BigDecimal.
   (/ (generate* :float Double/MIN_VALUE Double/MAX_VALUE)
      (generate* :float Double/MIN_VALUE Double/MAX_VALUE))))

(def ^:dynamic identities)

(defmethod generate* :ident
  [_ class as type & args]
  (let [return (volatile! nil)]
    (swap! identities
      update class
      (fn [idents]
        (loop [ident (apply generate* type args)]
          (if (contains? idents ident)
            (recur (apply generate* type args))
            (do (vreset! return ident)
                (conj (or idents #{})
                      (if (= :db/id as)
                        ident
                        [as ident])))))))
    @return))

(defmethod generate* :ref
  [_ class]
  (first (shuffle (or (get @identities class) []))))

(defmethod generate* :instant
  [_]
  (java.util.Date. (+ (.getTime (java.util.Date.)) (generate* :long -100000 0))))

(defmethod generate* :uuid
  [_]
  (java.util.UUID/randomUUID))

(defmethod generate* :uri
  [_]
  (generate* :string (generate* :long)))

(let [random-names (clojure.string/split-lines (load-resource "generators/names.txt"))]
  (defmethod generate* :name
    [_]
    (nth random-names (generate* :long 0 (count random-names)))))

(let [random-words (clojure.string/split-lines (load-resource "generators/words.txt"))]
  (defmethod generate* :word
    [_]
    (nth random-words (generate* :long 0 (count random-words)))))

(defmethod generate* :street
  [_]
  (str
    (apply str (generate* [:* 2 :word]))
    \space
    (generate* #{"Rd. Ave Blvd. St."})))

(let [random-words (clojure.string/split-lines (load-resource "generators/cities.txt"))]
  (defmethod generate* :city
    [_]
    (nth random-words (generate* :long 0 (count random-words)))))

(defmethod generate* :state
  [_]
  (generate* #{"Alabama" "Alaska" "American Samoa" "Arizona" "Arkansas" "California" "Colorado" "Connecticut" "Delaware" "District of Columbia" "Florida" "Georgia" "Guam" "Hawaii" "Idaho" "Illinois" "Indiana" "Iowa" "Kansas" "Kentucky" "Louisiana" "Maine" "Maryland" "Massachusetts" "Michigan" "Minnesota" "Mississippi" "Missouri" "Montana" "Nebraska" "Nevada" "New Hampshire" "New Jersey" "New Mexico" "New York" "North Carolina" "North Dakota" "Northern Mariana Islands"}))

(defmethod generate* :state-abbrev
  [_]
  (generate* #{"AK" "AL" "AR" "AZ" "CA" "CO" "CT" "DC" "DE" "FL" "GA" "HI" "IA" "ID" "IL" "IN" "KS" "KY" "LA" "MA" "MD" "ME" "MI" "MN" "MO" "MS" "MT" "NC" "ND" "NE" "NH" "NJ" "NM" "NV" "NY" "OH" "OK" "OR" "PA" "PR" "RI" "SC" "SD" "TN" "TX" "UT" "VA" "VT" "WA" "WI" "WV" "WY"}))

(defmethod generate* :address
  [_]
  (str (generate* :long 0 999)
       \space
       (generate* :street)
       \space
       (generate* :city)
       \, \space
       (generate* :state)
       \space
       (generate* :long 0 99999)))

(defmethod generate* :email
  [_]
  (str (generate* :word) "@" (generate* :word) \. (generate* #{"com" "edu" "org" "net"})))

(defmethod generate* :phone
  [_]
  (str \( (generate* :long 999) \) \- (generate* :long 999) \- (generate* :long 9999)))

(defmethod generate* :ordinal-number
  [_ & [left right]]
  (let [n (generate* :long left right)]
    (str n (condp = (mod n 10)
             1 "st"
             2 "nd"
             3 "rd"
            "th"))))

(defmethod generate* ::fn
  [type]
  (apply type []))

(defmethod generate* ::map
  [type]
  (into {}
    (map
      (fn [[k v]]
        (when-let [v (generate* v)]
          [k v]))
      type)))

(defmethod generate* ::set
  [type]
  (first (shuffle type)))

(defn generate
  [& args]
  (if (bound? #'identities)
    (apply generate* args)
    (binding [identities (atom {})]
      (apply generate* args))))
