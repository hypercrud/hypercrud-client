(ns hyperfiddle.route
  (:require
    [cats.core :refer [>>=]]
    [cats.monad.either :as either]
    [clojure.spec.alpha :as s]
    [clojure.string :as string]
    [clojure.walk :as walk]
    [contrib.base-64-url-safe :as base-64-url-safe]
    [contrib.data]
    [contrib.ednish :as ednish :refer [decode-ednish encode-ednish]]
    [contrib.pprint :refer [pprint-str]]
    [contrib.reader :as reader]
    [contrib.rfc3986 :refer [decode-rfc3986-pchar encode-rfc3986-pchar]]
    [contrib.string :refer [empty->nil]]
    [contrib.try$ :refer [try-either]]
    [cuerdas.core :as str]
    [hypercrud.types.ThinEntity :refer [->ThinEntity #?(:cljs ThinEntity)]]
    [hyperfiddle.fiddle]                                    ; for ::fiddle spec
    [taoensso.timbre :as timbre])
  #?(:clj
     (:import (hypercrud.types.ThinEntity ThinEntity))))


(s/def ::fiddle (s/or
                  :ident :fiddle/ident
                  :dbid number?
                  ;:lookup ...
                  :uuid :fiddle/uuid))
(s/def ::datomic-args vector?)
(s/def ::where vector?)                                     ; todo (s/+ :contrib.datomic.client.query/clause)
(s/def ::fragment string?)

(s/def :hyperfiddle/route (s/cat :f qualified-symbol?, :args (s/* any?)))

(defn validate-route+ [route]
  (either/right
   (if (s/valid? :hyperfiddle/route route)
     [route nil]
     [route (ex-info (str "Invalid route\n" (s/explain-str :hyperfiddle/route route))
                     (s/explain-data :hyperfiddle/route route))])))

(defn decoding-error [e s]
  {::fiddle :hyperfiddle.system/decoding-error
   ::datomic-args [s (ex-message e) (pprint-str (ex-data e))]})

(def uri-query-encoders
  {})

(defn default-query-encoder
  [key]
  [(ednish/encode-uri key)
   (comp base-64-url-safe/encode pr-str)
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
   (comp reader/read-edn-string! base-64-url-safe/decode)])

(defn url-encode [route home-route]
  {:pre [(s/valid? :hyperfiddle/route route) (s/valid? :hyperfiddle/route home-route)]}
  (let [[f & args] route]
    (if (= route home-route)
      "/"
      (str "/"
           (ednish/encode-uri f)
           (some->> args
                    (map-indexed (fn [i arg]
                                   (let [[_sk encoder _decoder] (or (get uri-query-encoders i) (default-query-encoder i))]
                                     (str i "=" (encoder arg)))))
                    (string/join "&")
                    (str "?"))))))


(def url-regex #"^/([^/?#]*)/?(?:\?([^#]*))?(?:#(.*))?$")
;                 /|_______|/     ?|_____|     #|__|
;                      |              |           |
;       url        path[0]          query      fragment
;       hf-route   fiddle           varies     fragment

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
  {:pre  [(string? s) #_(s/valid? :hyperfiddle/route home-route)]
   :post [#_(s/valid? :hyperfiddle/route %)]}
  (-> (try-either
       (if-let [[_ s-fiddle s-query s-fragment] (re-find url-regex s)]
          ; is-home "/" true
          ; is-home "/?..." true
          ; is-home "/#..." true
          ; is-home "//" false
          ; is-home "//..." false
         (let [is-home (empty? s-fiddle)
               f       (ednish/decode-uri s-fiddle)
               args    (->> (some-> s-query (string/split #"&|;"))
                            (map (fn [s] (string/split s #"=" 2)))
                            (reduce (fn [acc [sk sv]]
                                      (let [[k _encoder decoder] (or (get uri-query-decoders sk) (default-query-decoder sk))]
                                        (assoc acc k (decoder (or sv "")))))
                                    {})
                            (positional))]
           (cond-> args
             is-home       (concat home-route) ;; pass potential extra args
             (not is-home) (->> (cons f))
             s-fragment    (with-meta {::fragment s-fragment})))
         (decoding-error (ex-info "Invalid url" {}) s)))
      (either/branch
       (fn [e] (decoding-error e s))
       identity)))

(defn invert-datomic-arg [v invert-id]
  (if (instance? ThinEntity v)
    (->ThinEntity (.-dbname v) (invert-id (.-dbname v) (.-id v)))
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
                          m-route (contrib.data/dissoc-nils
                                    {::fiddle fiddle
                                     ::datomic-args datomic-args
                                     ::fragment fragment})]
                      (timbre/warnf "Deprecated route format detected `%s` use a map instead: `%s`" (pr-str route) (pr-str m-route))
                      m-route)))

(defn fill-where [unfilled-where needle]
  (walk/prewalk (fn [sym] (if (= '% sym) needle sym)) unfilled-where))
