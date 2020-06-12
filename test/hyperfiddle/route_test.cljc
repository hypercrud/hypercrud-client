(ns hyperfiddle.route-test
  (:require
    [clojure.test :refer [deftest is]]
    [contrib.reader]
    [hyperfiddle.route :as route :refer [url-decode url-encode]]))


(defn encode [route] (url-encode route {::route/fiddle :foo}))
(defn decode [s] (url-decode s {::route/fiddle :foo}))

(def route-args2 {::route/fiddle :hyperfiddle.blog/post
                  ::route/datomic-args [#entity["$" [:user/sub "google-oauth2|116635422485042503270"]] #{"events" "news"}]})
(def route-args1 {::route/fiddle :hyperfiddle.blog/post
                  ::route/datomic-args #entity["$" [:user/sub "google-oauth2|116635422485042503270"]]})
(def route-args1-seq {::route/fiddle :hyperfiddle.blog/post
                      ::route/datomic-args [#entity["$" [:user/sub "google-oauth2|116635422485042503270"]]]})

(deftest router-basic
  []
  (is (= (encode route-args2) "/:hyperfiddle.blog!post/~entity('$',(:user!sub,'google-oauth2%7C116635422485042503270'))/~%7B'news','events'%7D"))
  (is (= (decode "/17592186045933/") {::route/fiddle 17592186045933})) ; legacy, doesnt happen anymore?
  (is (= ((comp decode encode) route-args2) route-args2))
  #_(is (= ((comp decode encode) route-args1) (update route-args1 1 normalize-args)))
  (is (= ((comp decode encode) route-args1-seq) route-args1-seq))
  #_(is (= ((comp decode encode) route-args1) ((comp decode encode) route-args1-seq)))
  #?(:clj (is (not (nil? (java.net.URI. (encode route-args2))))))
  (is (= (encode {::route/fiddle :hyperfiddle.blog/post})
         "/:hyperfiddle.blog!post/"))
  (is (= (encode {::route/fiddle 17592186045502})
         "/17592186045502/"))

  (is (= (encode {::route/fiddle :hyperblog/post
                  ::route/datomic-args [#entity["$" 17592186045826]]})
         "/:hyperblog!post/~entity('$',17592186045826)"))

  (is (= (decode "/:hyperfiddle.blog!post/~entity('$',:hyperfiddle.blog!homepage)") {::route/fiddle :hyperfiddle.blog/post
                                                                                     ::route/datomic-args [#entity["$" :hyperfiddle.blog/homepage]]}))
  (is (= (decode "/:hyperfiddle.blog!post/~entity('$',:hyperfiddle.blog!homepage)?#:src") {::route/fiddle :hyperfiddle.blog/post
                                                                                           ::route/datomic-args [#entity["$" :hyperfiddle.blog/homepage]]
                                                                                           ::route/fragment ":src"}))
  (is (= (decode "/:hyperfiddle.blog!post/~entity('$',:hyperfiddle.blog!homepage)#:src") {::route/fiddle :hyperfiddle.blog/post
                                                                                          ::route/datomic-args [#entity["$" :hyperfiddle.blog/homepage]]
                                                                                          ::route/fragment ":src"}))

  (is (= "/:hyperfiddle.blog!post/~entity('$',:hyperfiddle.blog!homepage)" (encode {::route/fiddle :hyperfiddle.blog/post
                                                                                    ::route/datomic-args [#entity["$" :hyperfiddle.blog/homepage]]}))))
  ;(is (= "/:hyperfiddle.blog!post/~entity('$',:hyperfiddle.blog!homepage)?#:src" (encode [:hyperfiddle.blog/post [#entity["$" :hyperfiddle.blog/homepage]] nil ":src"])))



(deftest query-params []
  (is (= (decode "/:bar?:asdf=InF3ZXJ6eGN2Ig,,") {::route/fiddle :bar :asdf "qwerzxcv"}))
  (is (= (decode "/:bar/?:asdf=InF3ZXJ6eGN2Ig,,") {::route/fiddle :bar :asdf "qwerzxcv"}))
  (is (= (decode "/:bar?:hyperfiddle.route!where=W1s_ZSA6Zm9vID9hXV0,&:asdf=InF3ZXJ6eGN2Ig,,") {::route/fiddle :bar
                                                                                                ::route/where '[[?e :foo ?a]]
                                                                                                :asdf "qwerzxcv"}))
  (is (= (decode "/:bar?:hyperfiddle.route!where=W1s_ZSA6Zm9vID9hXV0,&:asdf=InF3ZXJ6eGN2Ig,,#blah") {::route/fiddle :bar
                                                                                                     ::route/where '[[?e :foo ?a]]
                                                                                                     :asdf "qwerzxcv"
                                                                                                     ::route/fragment "blah"})))


(deftest router-malformed-1
  [])
  ;#entity["$" [:user/sub "google-oauth2|116635422485042503270"]]
  ;(decode "/") [nil [nil]]
  ;(decode "/garbagasdf..23425649=//e")
  ;(decode "/asdf/asdf/asdf?asdf?asdf?sadf")


(deftest fragment-1 []
  (is (= "/:hyperfiddle.blog!post/~entity('$',:hyperfiddle.blog!homepage)#:src" (encode {::route/fiddle :hyperfiddle.blog/post
                                                                                         ::route/datomic-args [#entity["$" :hyperfiddle.blog/homepage]]
                                                                                         ::route/fragment ":src"})))
  (is (= "/:hyperblog!post/~entity('$',:capitalism)#:src" (encode {::route/fiddle :hyperblog/post
                                                                   ::route/datomic-args [#entity["$" :capitalism]]
                                                                   ::route/fragment ":src"}))))
