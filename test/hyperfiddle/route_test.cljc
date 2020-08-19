(ns hyperfiddle.route-test
  (:require
    [clojure.test :refer [deftest is]]
    [contrib.reader]
    [hyperfiddle.route :as route :refer [url-decode url-encode]]))


(defn encode [route] (url-encode route `(foo)))
(defn decode [s] (url-decode s `(foo)))

(def route-args2 `(hyperfiddle.blog/post ~#entity["$" [:user/sub "google-oauth2|116635422485042503270"]] #{"events" "news"}))
(def route-args1 `(hyperfiddle.blog/post ~#entity["$" [:user/sub "google-oauth2|116635422485042503270"]]))
(def route-args1-seq `(hyperfiddle.blog/post ~#entity["$" [:user/sub "google-oauth2|116635422485042503270"]]))

(deftest router-basic
  []
  (is (= (encode route-args2)  "/hyperfiddle.blog!post?0=I2VudGl0eVsiJCIgWzp1c2VyL3N1YiAiZ29vZ2xlLW9hdXRoMnwxMTY2MzU0MjI0ODUwNDI1MDMyNzAiXV0,&1=I3sibmV3cyIgImV2ZW50cyJ9"))
  (is (= ((comp decode encode) route-args2) route-args2))
  (is (= ((comp decode encode) route-args1-seq) route-args1-seq))
  #?(:clj (is (not (nil? (java.net.URI. (encode route-args2))))))
  (is (= (encode `(hyperfiddle.blog/post))
         "/hyperfiddle.blog!post"))

  (is (= (encode `(hyperblog/post ~#entity["$" 17592186045826]))
         "/hyperblog!post?0=I2VudGl0eVsiJCIgMTc1OTIxODYwNDU4MjZd"))

  (is (= (decode "/hyperfiddle.blog!post?0=I2VudGl0eVsiJCIgOmh5cGVyZmlkZGxlLmJsb2cvaG9tZXBhZ2Vd")
         `(hyperfiddle.blog/post ~#entity["$" :hyperfiddle.blog/homepage])))

  (is (= "/hyperfiddle.blog!post?0=I2VudGl0eVsiJCIgOmh5cGVyZmlkZGxlLmJsb2cvaG9tZXBhZ2Vd"
         (encode `(hyperfiddle.blog/post ~#entity["$" :hyperfiddle.blog/homepage])))))

(deftest query-params []
  (is (= (decode "/bar?0=InF3ZXJ6eGN2Ig,,") '(bar "qwerzxcv")))
  (is (= (decode "/bar/?0=InF3ZXJ6eGN2Ig,,") '(bar "qwerzxcv")))
  ;
  ; (is (= (decode "/:bar?:hyperfiddle.route!where=W1s_ZSA6Zm9vID9hXV0,&utm=asdfdsaf") {::route/fiddle :bar
  ;                                                                                     ::route/where '[[?e :foo ?a]]
  ;                                                                                     'utm #?(:clj 'j�_vƟ :cljs "jÇ_vÆ")}))
  (is (= (decode "/bar?0=W1s_ZSA6Zm9vID9hXV0,&1=InF3ZXJ6eGN2Ig,,#blah")
         '(bar [[?e :foo ?a]] "qwerzxcv"))))


(deftest router-malformed-1
  [])
  ;#entity["$" [:user/sub "google-oauth2|116635422485042503270"]]
  ;(decode "/") [nil [nil]]
  ;(decode "/garbagasdf..23425649=//e")
  ;(decode "/asdf/asdf/asdf?asdf?asdf?sadf")

