(ns contrib.eval-cljs
  (:require
    [cljs.analyzer :as ana]
    [cljs.js :as cljs]
    [cljs.env :as env]
    [shadow.cljs.bootstrap.browser :as boot]
    [cljs.tagged-literals :as tags]
    [hyperfiddle.readers]
    [promesa.core :as p]))


; See https://code.thheller.com/blog/shadow-cljs/2017/10/14/bootstrap-support.html

(defonce compile-state-ref (env/default-compiler-env))

(def booted ((p/promisify boot/init) compile-state-ref {:path "/static/_/boot"}))

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
                *ns* (find-ns 'user #_cljs.analyzer/*cljs-ns*) ; shadow cljs repl creates 'cljs.user, not available in prod
                cljs.js/*eval-fn* cljs.js/js-eval
                tags/*cljs-data-readers*]
        (eval form)))))

(defn eval-expr-str! [code-str]
  ;; (js/console.warn code-str)
  (binding [ana/*cljs-warning-handlers* []
            tags/*cljs-data-readers* (merge tags/*cljs-data-readers*
                                       hyperfiddle.readers/hf-edn-readers
                                       clj-readers)]
    (let [r (atom nil)
          _ (cljs/eval-str compile-state-ref
                           code-str
                           nil
                           {:eval cljs/js-eval
                            :context :expr}
                           (partial reset! r))
          {value :value error :error :as eval-result} @r]
      (if error
        (throw (ex-info "cljs eval failed" {:cljs-input code-str :cljs-result eval-result}))
        value))))
