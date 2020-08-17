(ns hypercrud.browser.router-bidi
  (:require
    [bidi.bidi :as bidi]
    [clojure.spec.alpha :as s]
    [clojure.string :as string]
    [contrib.reader :refer [read-edn-string!]]              ; loads hyperfiddle.readers, there is a contrib->hyperfiddle dep
    [contrib.string :refer [abc empty->nil]]
    [cuerdas.core :as str]
    [hyperfiddle.route :as route]
    [hypercrud.types.ThinEntity :refer [->ThinEntity #?(:cljs ThinEntity)]])
  #?(:clj
     (:import (hypercrud.types.ThinEntity ThinEntity))))


; Bidi routing protocols
(def regex-keyword "^:[A-Za-z]+[A-Za-z0-9\\*\\+\\!\\-\\_\\?\\.]*(?:%2F[A-Za-z]+[A-Za-z0-9\\*\\+\\!\\-\\_\\?\\.]*)?")
(def regex-string "^[A-Za-z0-9\\-\\_\\.]+")
(def regex-long "^[0-9]+")

(extend-type ThinEntity

  ; must be safe to user input. if they throw, the app won't fallback to a system-route

  bidi/PatternSegment
  (segment-regex-group [_]
    ; the value mathign the placeholder can be a keyword or a long
    (str "(?:" regex-long ")|(?:" regex-keyword ")"))
  (param-key [this]
    ; Abusing .-id to store the param-key in the entity placeholder in the route
    (.-id this))
  (transform-param [this]
    (fn [v]
      (let [$ (.-dbname this)                               ; the "$" is provided by entity placeholder in the route
            e (read-edn-string! v)]                         ; the reader will need to subs ! to /
        (->ThinEntity $ e))))
  (matches? [this s]
    (let [r (re-pattern
              "[A-Za-z]+[A-Za-z0-9\\*\\+\\!\\-\\_\\?\\.]*(?:%2F[A-Za-z]+[A-Za-z0-9\\*\\+\\!\\-\\_\\?\\.]*)?")]
      (re-matches r s)))
  (unmatch-segment [this params]
    (let [entity (get params (.-id this))]
      ; lookup refs not implemented, but eid and ident work
      ; safe
      (some-> entity .-id)))

  bidi/Pattern
  (match-pattern [this env]
    ; is this even in play? I don't think I ever hit this bp
    (let [read (read-edn-string! (:remainder env))]
      (-> env
          (update-in [:route-params] assoc (.-id this) (->ThinEntity (.-dbname this) read))
          ; totally not legit to count read bc whitespace
          (assoc :remainder (subs (:remainder env) 0 (count (pr-str read)))))))
  (unmatch-pattern [this m]
    (let [param-key (.-id this)]
      (-> m :params (get param-key) .-id pr-str)))

  ;bidi/Matched
  ;(resolve-handler [this m] (bidi/succeed this m))
  ;(unresolve-handler [this m] (when (= this (:handler m)) ""))
  )


; Bidi is not a great fit for the sys router because there is only one route with varargs params
; params are dynamically typed; need to model an `any`, eager-consume the path segment,
; and figure out the edn from the path segment. How to determine #entity vs scalar?
;(def sys-router
;  [["/" :fiddle-id "/" #edn 0] :browse])

(defn ->bidi-consistency-wrapper [{:keys [handler route-params] :as ?r}]
  ; Bidi's interface is inconsistent and makes you understand two ways to identify a route
  ; "bidi/match" return val syntax, is the bad one
  ; Canonicalize on the "bidi/path-for" syntax
  (if ?r (apply conj [handler] (mapcat identity route-params))))

(defn bidi->hf [[handler & ?route-params :as ?r]]
  (if ?r
    (let [datomic-args (->> ?route-params                   ; bidi gives us alternating k/v
                            (partition-all 2)
                            (map vec)
                            (sort-by first)                 ; order by keys for hyperfiddle, router should use kw or int
                            (mapv second)                   ; drop keys; hyperfiddle params are associative by index
                            )]
      (seq
       (cond-> [(symbol handler)] ;; FIXME it should already be a symbol
         (seq datomic-args) (concat datomic-args))))))

(defn bidi-match->path-for "adapt bidi's inconsistent interface" [[h & ps :as ?r]]
  (if ?r {:handler h :route-params ps}))

(defn ->bidi [{:keys [::route/fiddle ::route/datomic-args] :as route}]
  ; this is going to generate param names of 0, 1, ... which maybe doesn't work for all routes
  ; we would need to disallow bidi keywords for this to be valid. Can bidi use ints? I think not :(
  (if route (apply conj [fiddle] (mapcat vector (abc) datomic-args))))


(defn decode [router path-and-frag]
  {:pre [(str/starts-with? path-and-frag "/")]
   :post [(s/valid? :hyperfiddle/route %)]}
  (let [[path _frag] (string/split path-and-frag #"#" 2)
        route (some-> (bidi/match-route router path) ->bidi-consistency-wrapper bidi->hf)]
    (or route
        `(hyperfiddle.system/not-found))))

(comment
  (def path-and-frag "/:hyperblog.2!tag/:hyperfiddle#:src")
  (def path-and-frag "/:hyperblog.2!tag/:hyperfiddle")
  (def router ["/"
               {"drafts/" :hyperblog/drafts
                "pairing/" :user/pairing
                ; Attempting to call unbound fn: #'hyperfiddle.readers/entity
                ;[#entity["$" :a]] :hyperblog/post
                }])
  (def path "/:hyperblog.2!tag/:hyperfiddle")

  (decode router path-and-frag)
  )

(defn encode [router route]
  {:pre [(s/valid? :hyperfiddle/route route)]
   :post [(str/starts-with? % "/")]}
  (if-let [url (apply bidi/path-for router (->bidi route))]
    (if (empty->nil (::route/fragment route))
      (str url "#" (::route/fragment route))
      url)
    ; todo attempt to write an error url in this else case
    ))
