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

(s/def :hyperfiddle/route
  (s/keys
    :req [::fiddle]
    :opt [::datomic-args ::where ::fragment]))

(defn validate-route+ [route]
  (either/right
   (if (s/valid? :hyperfiddle/route route)
     [route nil]
     [route (ex-info (str "Invalid route\n" (s/explain-str :hyperfiddle/route route))
                     (s/explain-data :hyperfiddle/route route))])))

(defn equal-without-frag? [a b]
  (= (dissoc a ::fragment) (dissoc b ::fragment)))

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
  {:pre [#_(s/valid? :hyperfiddle/route route) (s/valid? :hyperfiddle/route home-route)]}
  (let [{:keys [::fiddle ::datomic-args ::fragment]} route]
    (if (equal-without-frag? route home-route)
      (str "/" (some->> fragment empty->nil (str "#")))
      (case fiddle
        :hyperfiddle.system/decoding-error (first datomic-args)
        (str "/"
             (ednish/encode-uri fiddle)
             "/"
             (string/join "/" (map ednish/encode-uri datomic-args))
             (some->> (dissoc route ::fiddle ::datomic-args ::fragment)
                      seq
                      (map (fn [[k v]]
                             (let [[sk encoder decoder] (or (get uri-query-encoders k) (default-query-encoder k))]
                               (str sk "=" (encoder v)))))
                      (string/join "&")
                      (str "?"))
             (if (empty->nil fragment) (str "#" (-> fragment encode-ednish encode-rfc3986-pchar))))))))


(def url-regex #"^/([^/?#]*)(?:/([^?#]*))?(?:\?([^#]*))?(?:#(.*))?$")
;                 /|_______|   /|______|      ?|_____|     #|__|
;                      |           |              |          |
;       url        path[0]      path[1 ...]    query        fragment
;       hf-route   fiddle       datomic-args   varies       fragment

(defn url-decode [s home-route]
  {:pre [(string? s) #_(s/valid? :hyperfiddle/route home-route)]
   :post [#_(s/valid? :hyperfiddle/route %)]}
  (-> (try-either
        (if-let [[_ s-fiddle s-datomic-args s-query s-fragment] (re-find url-regex s)]
          ; is-home "/" true
          ; is-home "/?..." true
          ; is-home "/#..." true
          ; is-home "//" false
          ; is-home "//..." false
          (let [is-home (and (empty? s-fiddle) (nil? s-datomic-args))]
            (-> (->> (some-> s-query (string/split #"&|;"))
                     (map (fn [s] (string/split s #"=" 2)))
                     (reduce (fn [acc [sk sv]]
                               (let [[k encoder decoder] (or (get uri-query-decoders sk) (default-query-decoder sk))]
                                 (assoc acc k (decoder (or sv "")))))
                             (if is-home
                               ; conj the url's query params onto the home-route's
                               (select-keys home-route (keys uri-query-encoders))
                               {})))
                (assoc ::fiddle (if is-home (::fiddle home-route) (ednish/decode-uri s-fiddle))
                       ::datomic-args (if is-home
                                        (::datomic-args home-route)
                                        (some-> (->> (str/split s-datomic-args "/") ; careful: (str/split "" "/") => [""]
                                                     (remove str/empty-or-nil?)
                                                     (map ednish/decode-uri)
                                                     seq)
                                                vec))
                       ::fragment (or (-> s-fragment decode-rfc3986-pchar decode-ednish empty->nil)
                                      (when is-home (::fragment home-route))))
                contrib.data/dissoc-nils))
          (decoding-error (ex-info "Invalid url" {}) s)))
      (>>= validate-route+)
      (either/branch
       (fn [[_ e]] (decoding-error e s))
        first)))

(defn invert-datomic-arg [v invert-id]
  (if (instance? ThinEntity v)
    (->ThinEntity (.-dbname v) (invert-id (.-dbname v) (.-id v)))
    v))

(defn invert-datomic-args [invert-id datomic-args]
  (mapv #(invert-datomic-arg % invert-id) datomic-args))

(defn invert-route [route invert-id]
  (if (contains? route ::datomic-args)
    (update route ::datomic-args (partial invert-datomic-args invert-id))
    route))

(defn legacy-route-adapter [route]
  (cond
    (nil? route) route
    (map? route) route
    (vector? route) (let [[fiddle datomic-args _ fragment] route
                          m-route (contrib.data/dissoc-nils
                                    {::fiddle fiddle
                                     ::datomic-args datomic-args
                                     ::fragment fragment})]
                      (timbre/warnf "Deprecated route format detected `%s` use a map instead: `%s`" (pr-str route) (pr-str m-route))
                      m-route)))

(defn fill-where [unfilled-where needle]
  (walk/prewalk (fn [sym] (if (= '% sym) needle sym)) unfilled-where))
