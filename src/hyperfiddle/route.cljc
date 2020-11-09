(ns hyperfiddle.route
  (:require
    [cats.monad.either :as either]
    [clojure.spec.alpha :as s]
    [clojure.string :as string]
    [clojure.walk :as walk]
    [contrib.base-64-url-safe :as base-64-url-safe]
    [contrib.data :as data]
    [contrib.ednish :as ednish]
    [contrib.pprint :refer [pprint-str]]
    [contrib.reader :as reader]
    [contrib.try$ :refer [try-either]]
    [hyperfiddle.api :as hf]
    [taoensso.timbre :as timbre]))

(s/def ::fiddle (s/or
                  :ident :fiddle/ident
                  :dbid number?
                  ;:lookup ...
                  :uuid :fiddle/uuid))
(s/def ::where vector?)                                     ; todo (s/+ :contrib.datomic.client.query/clause)
(s/def ::fragment string?)

(s/def ::route (s/cat :f qualified-symbol?, :args (s/* any?)))

(defn validate-route+ [route]
  (either/right
   (if (s/valid? ::route route)
     [route nil]
     [route (ex-info (str "Invalid route\n" (s/explain-str ::route route))
                     (s/explain-data ::route route))])))

(defn decoding-error [e s]
  {::fiddle :hyperfiddle.system/decoding-error
   ::datomic-args [s (ex-message e) (pprint-str (ex-data e))]})

(def uri-query-encoders
  {})

(defn default-query-encoder
  [key]
  [(ednish/encode-uri key)
   ednish/encode-uri
   ;; (comp base-64-url-safe/encode pr-str)
   (comp reader/read-edn-string! base-64-url-safe/decode)])

(def uri-query-decoders
  (->> uri-query-encoders
       ; flip k and sk
       (map (fn [[k [sk encoder decoder]]] [sk [k encoder decoder]]))
       (into {})))

(defn default-query-decoder
  [key]
  [(ednish/decode-uri key)
   (comp base-64-url-safe/encode pr-str)
   ednish/decode-uri])

(defn url-encode [route home-route]
  ;; {:pre [(s/valid? ::route route) (s/valid? ::route home-route)]}
  (let [[f & args] route]
    (if (= route home-route)
      "/"
      (str "/"
           (ednish/encode-uri f)
           "/"
           (some->> args
                    (map-indexed (fn [i arg]
                                   (let [[_sk encoder _decoder] (or (get uri-query-encoders i) (default-query-encoder i))]
                                     (encoder arg))))
                    (string/join "/"))))))

(def url-regex #"^/([^/?#]+/?)*(\?[^#]*)?(#.*)?$")

(defn positional
  "Transform a map into a list. Expect keys to be 0,1,2,3… and contiguous.
  eg. {0 :foo, 1 :bar} => [:foo :bar]"
  [amap]
  (->> (range (inc (count amap)))
       (reduce (fn [acc idx]
                 (if (contains? amap idx)
                   (conj acc (get amap idx))
                   (reduced acc)))
               [])
       (seq)))

(defn url-decode [s home-route]
  {:pre  [(string? s) #_(s/valid? ::route home-route)]
   :post [#_(s/valid? ::route %)]}
  (-> (try-either
       (if-let [[match _ query hash] (re-find url-regex s)]
          ; is-home "/" true
          ; is-home "/?..." true
          ; is-home "/#..." true
          ; is-home "//" false
          ; is-home "//..." false
         (if (= "/" match)
           home-route
           (->> (cond-> match
                  (seq hash)  (-> (string/split #"#") first)
                  (seq query) (-> (string/split #"\?") first)
                  true        (string/split #"/"))
                (rest)
                (map-indexed vector)
                (reduce (fn [acc [k v]]
                          (let [[_ _encoder decoder] (or (get uri-query-decoders v) (default-query-decoder v))]
                            (assoc acc k (decoder (or v "")))))
                        {})
                (positional)))
         (decoding-error (ex-info "Invalid url" {}) s)))
      (either/branch
       (fn [e] (decoding-error e s))
       identity)))

(defn invert-datomic-arg [v invert-id]
  (if (hf/colored-tempid? v)
    (let [[_ db] (hf/parse-colored-tempid v)]
      (invert-id db v))                                     ; returns string, not tuple
    v))

(defn invert-datomic-args [invert-id datomic-args]
  (map #(invert-datomic-arg % invert-id) datomic-args))

(defn invert-route [[f & args] invert-id]
  (cons f (invert-datomic-args invert-id args)))

(defn legacy-route-adapter [route]
  (cond
    (nil? route)    route
    (map? route)    route
    (list? route)   route
    (vector? route) (let [[fiddle datomic-args _ fragment] route
                          m-route                          (data/dissoc-nils
                                                            {::fiddle       fiddle
                                                             ::datomic-args datomic-args
                                                             ::fragment     fragment})]
                      (timbre/warnf "Deprecated route format detected `%s` use a map instead: `%s`" (pr-str route) (pr-str m-route))
                      m-route)))

(defn fill-where [unfilled-where needle]
  (walk/prewalk (fn [sym] (if (= '% sym) needle sym)) unfilled-where))
