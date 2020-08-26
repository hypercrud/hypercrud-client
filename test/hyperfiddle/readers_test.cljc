(ns hyperfiddle.readers-test
  #?(:cljs
     (:require-macros [backtick :refer [template]]))
  (:require
    #?(:clj [backtick :refer [template]])
            [clojure.test :refer [deftest is]]
            [contrib.datomic]
            [contrib.eval :as eval :refer [eval-expr-str!]]
            [contrib.reader :as reader :refer [read-edn-string!]]
            #?(:cljs [goog.math])
            [hyperfiddle.readers]
            [hypercrud.transit :as transit]
            [hypercrud.types.ThinEntity :refer [->ThinEntity]]
            [contrib.uri :refer [->URI]])
  (:import #?(:clj java.util.Date)))


(defn test-compile-read [control literal-read]
  (is (= control literal-read)))

(defn test-runtime-read [control strd]
  (is (= control
         (reader/read-string (pr-str control))
         (reader/read-string strd))))

(defn test-edn-read [control strd]
  (is (= control
         (read-edn-string! (pr-str control))
         (read-edn-string! strd))))

(defn test-eval [control strd]
  (is (= control
         (eval/eval-expr-str! (pr-str control))
         (eval/eval-expr-str! strd))))

(defn test-transit [control transit-strd]
  (is (= control
         (transit/decode (transit/encode control))
         (transit/decode transit-strd :type :json-verbose))))

(defn test-all-forms [control literal-read strd transit-strd]
  (test-compile-read control literal-read)
  (test-runtime-read control strd)
  (test-edn-read control strd)
  (test-eval control strd)
  (test-transit control transit-strd))

#?(:clj
   (do
     (deftest entity []
       (test-all-forms (->ThinEntity "foo" "bar")
                       #entity["foo" "bar"]
                       "#entity[\"foo\" \"bar\"]"
                       "{\"~#entity\":[\"foo\",\"bar\"]}"))

     (comment
      (test-edn-read {:form 'foo} (pr-str {:form 'foo}))

      (read-edn-string! "{:form ~(inc 1)}")
      (eval-expr-str! "`{:form ~(inc 1)}")

      (eval-expr-str! "(template {:form ~(inc 1)})")
      (reader/read-string "{:form ~(inc 1)}")
      (eval `(template ~(reader/read-string "{:form ~(inc 1)}")))

      (= {:form 'foo}
         (read-edn-string! "{:form foo}")
         (eval-expr-str! "{:form 'foo}")
         #_(read-edn-string! "{:form 'foo}")
         )
      )

     (deftest uri []
       (test-all-forms (->URI "foo")
                       #uri "foo"
                       "#uri \"foo\""
                       "{\"~#'\":\"~rfoo\"}"))

     (deftest inst []
       (test-all-forms #?(:cljs (js/Date. "2017-12-31") :clj #inst "2017-12-31")
                       #inst "2017-12-31"
                       "#inst \"2017-12-31\""
                       "{\"~#t\":\"2017-12-31\"}"))))

#?(:cljs
   (deftest long|1
     #_(test-all-forms (.fromString goog.math.Long "65332980922449989")
                       #long "65332980922449989"
                       "#long \"65332980922449989\""
                       "[\"~#'\",\"~i65332980922449989\"]")))

#?(:clj
   (deftest schema
     []
     (test-all-forms (contrib.datomic/indexed-schema [{:db/id 10, :db/ident :db/ident, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/one}, :db/unique {:db/ident :db.unique/identity}, :db/doc "Attribute used to uniquely name an entity."}])
                     #schema #:db{:ident #:db{:id 10, :ident :db/ident, :valueType #:db{:ident :db.type/keyword}, :cardinality #:db{:ident :db.cardinality/one}, :unique #:db{:ident :db.unique/identity}, :doc "Attribute used to uniquely name an entity."}}
                     "#schema #:db{:ident #:db{:id 10, :ident :db/ident, :valueType #:db{:ident :db.type/keyword}, :cardinality #:db{:ident :db.cardinality/one}, :unique #:db{:ident :db.unique/identity}, :doc \"Attribute used to uniquely name an entity.\"}}"
                     "[\"~#schema-v\",[[\"^ \",\"~:db/ident\",[\"^ \",\"~:db/id\",10,\"^1\",\"^1\",\"~:db/valueType\",[\"^ \",\"^1\",\"~:db.type/keyword\"],\"~:db/cardinality\",[\"^ \",\"^1\",\"~:db.cardinality/one\"],\"~:db/unique\",[\"^ \",\"^1\",\"~:db.unique/identity\"],\"~:db/doc\",\"Attribute used to uniquely name an entity.\"]]]]"
                     )
     ))
