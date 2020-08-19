(ns hypercrud.browser.router-bidi-test
  (:require
    [bidi.bidi :as bidi]
    [clojure.test :refer [deftest is]]
    [contrib.reader]
    [hypercrud.browser.router-bidi :refer [decode]]
    [hyperfiddle.route :as route]))


(def router ["/"
             {"drafts/" :hyperblog/drafts
              "pairing/" :user/pairing
              [#entity["$" 0]] :hyperblog/post}])

(deftest bidi-raw []
  (is (= (some-> router (bidi/match-route "/:rdbms-denormalize")) {:route-params {0 #entity["$" :rdbms-denormalize]}, :handler :hyperblog/post})))

(deftest bidi-hf-1 []
  (is (= (decode router "/:rdbms-denormalize") `(hyperblog/post ~#entity["$" :rdbms-denormalize])))
  (is (= (decode router "/:rdbms-denormalize#:src") `(hyperblog/post ~#entity["$" :rdbms-denormalize])))
  (is (= (decode router "/:capitalism") `(hyperblog/post ~#entity["$" :capitalism])))
  (is (= (decode router "/:capitalism#:src") `(hyperblog/post ~#entity["$" :capitalism])))

  ; Unhandled routes are not handled here
  (is (= (decode router "/:hyperblog.2!tag/:hyperfiddle") `(hyperfiddle.system/not-found)))
  (is (= (decode router "/:hyperblog.2!tag/:hyperfiddle#:src") `(hyperfiddle.system/not-found)))
  )

(comment
  ;[#entity["$" :a] "/"] 17592186045454 ;IllegalArgumentException: No implementation of method: :unresolve-handler of protocol: #'bidi.bidi/Matched found for class: java.lang.Long

  (->> [
        (some-> router (bidi/match-route "/1/"))
        #_(some-> router (bidi/match-route "/a/"))
        #_(some-> router (bidi/match-route "/:a/"))
        (some-> router (bidi/match-route "/:highlights/"))
        (some-> router (bidi/match-route "/1/slug"))
        #_(some-> router (bidi/match-route "/:sup"))        ; routes but can't rebuild
        (some-> router (bidi/match-route "/:sup/slug"))
        (some-> router (bidi/match-route "/drafts/"))
        (some-> router (bidi/match-route "/1/slug-for-a-post"))
        (some-> router (bidi/match-route "/_/asdf"))
        ]
       (map ->bidi-consistency-wrapper)
       (map bidi->hf)
       (map ->bidi)
       (map #(if % (apply bidi/path-for router %))))

  (apply bidi/path-for router (-> router (bidi/match-route "/1/") ->bidi-consistency-wrapper bidi->hf ->bidi))
  (some-> router (bidi/match-route "/1/slug") ->bidi-consistency-wrapper bidi->hf ->bidi ((partial apply bidi/path-for router)))
  (some-> router (bidi/match-route "/:sup/a") ->bidi-consistency-wrapper bidi->hf ->bidi ((partial apply bidi/path-for router)))

  (->> [(bidi/path-for router :hyperblog/index :a #entity["$" 1])
        (bidi/path-for router :hyperblog/index :a #entity["$" :highlights])
        (bidi/path-for router :hyperblog/post :a #entity["$" 1] :b "asdf")
        (bidi/path-for router :hyperblog/post :a #entity["$" :sup])
        (bidi/path-for router :hyperblog/drafts)
        (bidi/path-for router :not-routed)

        (apply bidi/path-for router [:hyperblog/index 0 #entity["$" 17592186046269]])
        ;(apply bidi/path-for router [17592186045454 0 #entity["$" 17592186046269]])
        ])

  (->> [(some-> router (bidi/match-route "/:sup"))
        (some-> router (bidi/match-route "/:personal/"))]
       (map ->browser))

  (apply bidi/path-for router [:hyperblog/post 0 #entity["$" :sup]])
  (apply bidi/path-for router [:hyperblog/index 0 #entity["$" :personal]])
  (apply bidi/path-for router [:hyperblog/drafts])
  )
