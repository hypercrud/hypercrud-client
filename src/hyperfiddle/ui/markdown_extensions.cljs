(ns hyperfiddle.ui.markdown-extensions
  (:require
    [cats.core :refer [fmap]]
    [cats.monad.either :as either]
    [contrib.css :refer [css]]
    [contrib.ct :refer [unwrap]]
    [contrib.reactive :as r]
    [contrib.reader :refer [memoized-read-edn-string+]]
    [contrib.string :refer [blank->nil or-str]]
    [contrib.ui.remark :as remark]
    [cuerdas.core :as str]
    [goog.object]
    [hypercrud.browser.context :as context]
    [hyperfiddle.ui]
    [taoensso.timbre :as timbre]))


(defn memoized-safe-eval [_ctx code-str]
  ;; This is stubbed. If you need to evaluate code, please re-enable self-hosted
  ;; clojurescript eval.
  (either/left nil)
  #_(if (blank->nil code-str)
      (eval/eval-expr-str!+ code-str)
      (either/left nil)))

(declare markdown)                                          ; mutual recursion, it would be letfn if wasn't react components

; Content is text, or more markdown, or code
; Argument is semantic: a url, or a hyperfiddle ident (or a second optional content? Caption, row-renderer)
(defmulti md-ext (fn [k content argument props ctx] k))

(defmethod md-ext :li [_ content argument props ctx]
  [:li.p (dissoc props :children) (:children props)])

(defmethod md-ext :p [_ content argument props ctx]
  ; Really need a way to single here from below, to get rid of div.p
  ; So that means signalling via this :children value
  (if (::unp ctx)
    (into [:<>] (:children props))
    [:div.p (dissoc props :children) (:children props)]))

(defmethod md-ext :span [_ content argument props ctx]
  [:span (remark/adapt-props props)
   [markdown content (assoc ctx ::unp true)]])

(defmethod md-ext :a [_ content argument props ctx]
  [:a (merge {:href argument} (dissoc props :children))
   ; backwards compat with vanilla markdown anchors
   (or (:children props) content)])

(defmethod md-ext :f [_ content argument props ctx]
  (let [content (some->> content (memoized-safe-eval ctx) (unwrap #(timbre/warn %)))
        argument (some->> argument (memoized-safe-eval ctx) (unwrap (constantly argument)))] ; If argument doesn't parse as Clojure, treat as text

    (cond

      ; ![hyperfiddle.ui/img](https://i.imgur.com/ewdY65H.png) -> [hyperfiddle.ui/img val ctx props]
      (fn? content) (let [argument (if (symbol? argument) (str argument) argument)] ; yolo
                      [content argument ctx props])

      ;(= :div content) [:div props (markdown argument)]     ; !f[:div](yo) --- doesn't work though right now

      ; ![:img](https://i.imgur.com/ewdY65H.png)
      ; ![:video](https://i.imgur.com/ewdY65H.png)
      ; This needs type coersion on attributes like "controls"="false"
      (keyword? content) [content (merge (if argument {:src (str argument)})
                                         props)]

      :else [:code "Bad quick-element: " content])))

(defmethod md-ext :img [_ content argument {:keys [children alt src]} ctx]
  ; Jacks the image ![alt](src) syntax to repurpose to something more general. Doesn't support props!
  ; Eats img alt-text, but that can be fixed.
  (md-ext :f (or alt ":img") src {} ctx))

; Is this comment true?::
;   Div is not needed, use it with block syntax and it hits React.createElement and works
;   see https://github.com/medfreeman/remark-generic-extensions/issues/30

(defmethod md-ext :block [_ content argument props ctx]
  [:div props [markdown content (assoc ctx ::unp true)]])

; This is a custom markdown extension example.
(defmethod md-ext :figure [_ content argument props ctx]
  [:figure.figure props
   [markdown content (assoc ctx ::unp true)]                ; it's an image or pre or other block element
   [:figcaption.figure-caption [markdown argument (assoc ctx ::unp true)]]])

(defmethod md-ext :pre [_ content argument props ctx]
  ; detect ``` legacy syntax, no props or argument
  (if-let [children (:children props)]
    ; Remark generates pre>code; deep inspect and rip out the content
    ; Don't hook :code because that is used by inline snippets
    (let [content (goog.object/getValueByKeys children 0 "props" "children" 0)
          content (str/rtrim content "\n") #_"Remark yields an unavoidable newline that we don't want"]
      [contrib.ui/code {:value content :read-only true}])
    [contrib.ui/code (assoc props :value content)]))

(defmethod md-ext :render [_ content argument props ctx]
  (->> (memoized-safe-eval ctx (str "(fn [ctx] \n" content "\n)"))
       (fmap (fn [f] (f ctx)))
       (unwrap #(timbre/warn %))))

(defmethod md-ext :browse [_ content argument props ctx]
  (let [[_ srel sclass] (re-find #"([^ ]*) ?(.*)" argument)
        rel (some->> srel (memoized-safe-eval ctx) (unwrap #(timbre/warn %)))
        class (some->> sclass (memoized-safe-eval ctx) (unwrap #(timbre/warn %))) ; corcs
        ?f (some->> content (memoized-safe-eval ctx) (unwrap #(timbre/warn %)))]
    (hyperfiddle.ui/browse class ctx ?f props)))

(defmethod md-ext :link [_ content argument props ctx]
  (let [#_#_[_ rel-s class-s] (re-find #"([^ ]*) ?(.*)" argument)
        ?class (some->> argument (memoized-safe-eval ctx) (unwrap #(timbre/warn %))) ; :class is mandatory now
        ; https://github.com/medfreeman/remark-generic-extensions/issues/45
        label (or-str content (some-> ?class name))]
    (hyperfiddle.ui/link ?class ctx label props)))

(defmethod md-ext :result [_ content argument props ctx]
  (let [ctx (assoc ctx ::unp true)]
    (if-let [f (some->> content (memoized-safe-eval ctx) (unwrap #(timbre/warn %)))]
      [f ctx]
      (hyperfiddle.ui/result
        (:hypercrud.browser/result ctx) #_(hypercrud.browser.context/data ctx) ; backwards compat
        ctx
        (update props :class css "unp")))))

(defmethod md-ext :value [_ content argument props ctx]
  (let [path (unwrap #(timbre/warn %) (memoized-read-edn-string+ (str "[" argument "]")))
        ?f (some->> content (memoized-safe-eval ctx) (unwrap #(timbre/warn %)))]
    (hyperfiddle.ui/value path ctx ?f props)))

(defmethod md-ext :field [_ content argument props ctx]
  (let [path (unwrap #(timbre/warn %) (memoized-read-edn-string+ (str "[" argument "]")))
        ?f (some->> content (memoized-safe-eval ctx) (unwrap #(timbre/warn %)))]
    (hyperfiddle.ui/field path ctx ?f (-> props
                                          (update :class css "unp")
                                          ;; If you need to evaluate code, please
                                          ;; re-enable self-hosted clojurescript
                                          ;; eval
                                          #_(update :label-fn eval/eval-apply)))))

(letfn [(fields [content props ctx]
          [[markdown content (assoc ctx ::unp true)]])]
  (defmethod md-ext :table [_ content argument props ctx]
    [hyperfiddle.ui/table (r/partial fields content props) ctx #_props]))

(defmethod md-ext :list [_ content argument props ctx]
  [:ul props
   (for [[ix ctx] (hypercrud.browser.context/spread-rows ctx)]
     ^{:key ix}
     [:li [markdown content ctx]])])

(def ^:export markdown (remark/remark! (methods md-ext)))
