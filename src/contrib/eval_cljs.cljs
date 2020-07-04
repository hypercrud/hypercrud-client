(ns contrib.eval-cljs
  (:require
    [cljs.js :as cljs]
    [cljs.env :as env]
    [shadow.cljs.bootstrap.browser :as boot]
    [cljs.tagged-literals :as tags]
    [hyperfiddle.readers]
    [promesa.core :as p]))


; See https://code.thheller.com/blog/shadow-cljs/2017/10/14/bootstrap-support.html

(defonce compile-state-ref (env/default-compiler-env))

(def booted ((p/promisify boot/init) compile-state-ref {:path "/static/_/boot"})) ; Kickoff self-host asset loading

; These are like clj readers in resources/data-readers.cljc ??
; Should we just slurp those here?
(def clj-readers
  {'entity hypercrud.types.ThinEntity/entity-clj-reader
   'uri contrib.uri/uri-clj-reader
   'long hyperfiddle.readers/long-clj-reader
   'schema hyperfiddle.readers/schema-clj-reader})

(let [eval *eval*
      st (cljs.js/empty-state)]
  (set! *eval*
    (fn [form]
      (binding [cljs.env/*compiler* compile-state-ref
                *ns* (find-ns 'user) ; shadow cljs repl creates 'cljs.user, not available in prod
                cljs.js/*eval-fn* cljs.js/js-eval
                tags/*cljs-data-readers*]
        (assert *ns*)
        (eval form)))))

