(ns hyperfiddle.route-test
  (:require
    [clojure.test :refer [deftest is]]
    [contrib.reader]
    [hyperfiddle.route :as route :refer [url-decode url-encode]]))


(defn encode [route] (url-encode route `(foo)))
(defn decode [s] (url-decode s `(foo)))

(def route-args2 `(hyperfiddle.blog/post [:user/sub "google-oauth2|116635422485042503270"] #{"events" "news"}))
(def route-args1 `(hyperfiddle.blog/post [:user/sub "google-oauth2|116635422485042503270"]))
(def route-args1-seq `(hyperfiddle.blog/post [:user/sub "google-oauth2|116635422485042503270"]))

(deftest router-basic
  []
  (is (= (encode route-args2) "/hyperfiddle.blog!post/(:user!sub,'google-oauth2%7C116635422485042503270')/~%7B'news','events'%7D"))
  (is (= ((comp decode encode) route-args2) route-args2))
  (is (= ((comp decode encode) route-args1-seq) route-args1-seq))
  #?(:clj (is (not (nil? (java.net.URI. (encode route-args2))))))
  (is (= (encode `(hyperfiddle.blog/post))
         "/hyperfiddle.blog!post/"))

  (is (= (encode `(hyperblog/post 17592186045826))
         "/hyperblog!post/17592186045826"))

  (is (= (decode "/hyperfiddle.blog!post/:hyperfiddle.blog!homepage")
         `(hyperfiddle.blog/post :hyperfiddle.blog/homepage)))

  (is (= "/hyperfiddle.blog!post/:hyperfiddle.blog!homepage"
         (encode `(hyperfiddle.blog/post :hyperfiddle.blog/homepage)))))
;; => #'hyperfiddle.route-test/router-basic
;; => #'hyperfiddle.route-test/router-basic

(deftest query-params []
  (is (= (decode "/bar/'qwerzxcv'") '(bar "qwerzxcv")))
  ;
  ; (is (= (decode "/:bar?:hyperfiddle.route!where=W1s_ZSA6Zm9vID9hXV0,&utm=asdfdsaf") {::route/fiddle :bar
  ;                                                                                     ::route/where '[[?e :foo ?a]]
  ;                                                                                     'utm #?(:clj 'j�_vƟ :cljs "jÇ_vÆ")}))
  (is (= (decode "/bar/((%3Fe,:foo,%3Fa))/'qwerzxcv'#blah")
         '(bar [[?e :foo ?a]] "qwerzxcv"))))


(deftest router-malformed-1
  [])
  ;[:user/sub "google-oauth2|116635422485042503270"]
  ;(decode "/") [nil [nil]]
  ;(decode "/garbagasdf..23425649=//e")
  ;(decode "/asdf/asdf/asdf?asdf?asdf?sadf")

