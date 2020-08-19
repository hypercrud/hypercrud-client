(ns hypercrud.browser.context-test
  (:require
    [cats.core :refer [mlet return extract]]
    [cats.monad.either :as either :refer [left right]]
    [clojure.spec.alpha :as s]
    [contrib.ct :refer [unwrap]]
    [contrib.reactive :as r]
    [clojure.test :refer [deftest is testing]]
    [fixtures.domains]
    [fixtures.tank]
    [hypercrud.browser.context :as context]
    [hyperfiddle.api :as hf]
    [hyperfiddle.data]
    [hyperfiddle.fiddle :as fiddle]
    [hyperfiddle.route :as route]
    [hyperfiddle.runtime]
    [hyperfiddle.state :as state]
    [hyperfiddle.ui.sort :as sort]))


(defn mock-peer [pid schemas]
  ; Stop NPEs in tempid that inspect the stage.
  (let [state-atom (-> {:hyperfiddle.runtime/partitions {pid {:is-branched true
                                                              :schemas schemas}}}
                       state/initialize
                       (r/atom))
        databases (zipmap (keys schemas) (repeatedly (constantly {})))
        domain (reify hf/Domain
                 (databases [domain] databases))]
    (reify
      hf/HF-Runtime
      (domain [rt] domain)
      hf/State
      (state [rt] state-atom))))

(defn mock-fiddle! "This has some subtle differences around tempid handling - this mock
  has no information about tempids at all, so these tests do not exercise tempid paths which
  can subtly influence keyfns possibly other things."
  ([ident] (mock-fiddle! fixtures.tank/schemas fixtures.tank/fiddles ident))
  ([schemas fiddles ident]
   (let [[fiddle result] (-> fiddles ident)
         fiddle (fiddle/apply-defaults fiddle)]
     (-> (context/map->Context
           {:partition-id hf/root-pid
            :hypercrud.browser/route nil
            :runtime (mock-peer hf/root-pid schemas)})
         (context/fiddle+ (r/pure fiddle))
         (either/branch (fn [e] (throw e)) identity)
         (context/result (r/pure result))))))

(deftest primitives
  []
  (testing "row keyfn on relation maps the smart identity"
    (let [ctx (mock-fiddle! :hfnet.tank/index)
          result (context/data ctx)]
      (is (= (context/row-key ctx (first result))
             [[:fiddle/ident :karl.hardenstine/fiddles] 13194139534712 true]))))

  (testing "key-row"
    (let [ctx (mock-fiddle! :dustingetz/gender-shirtsize)
          [row] (context/data ctx)]
      (is (= (context/row-key ctx row)
             [:dustingetz.reg/email "dustin@example.com"]))))

  (testing "from :identity refine to :db/id"
    (is (= (let [ctx (mock-fiddle! :dustingetz/gender-shirtsize)
                 ctx (context/row ctx [:dustingetz.reg/email "dustin@example.com"])
                 ctx (context/attribute ctx :db/id)]
             (context/eav ctx))
           [[:dustingetz.reg/email "dustin@example.com"] :db/id 17592186046196])))
  )

(deftest basics
  (def ctx (mock-fiddle! :dustingetz/gender-shirtsize))

  (testing "row indexing"
    (testing "data sidesteps indexing, userland never sees indexing."
      (is (= (let [ctx (mock-fiddle! :dustingetz/gender-shirtsize)]
               (context/data ctx))
             [{:db/id 17592186046196,
               :dustingetz.reg/email "dustin@example.com",
               :dustingetz.reg/name "Dustin Getz",
               :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
               :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}
              {:db/id 17592186046763,
               :dustingetz.reg/email "bob@example.com",
               :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
               :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}])))

    (testing "FindColl rows are indexed by smart identity (only visible internally)"
      (is (= (let [ctx (mock-fiddle! :dustingetz/gender-shirtsize)]
               @(:hypercrud.browser/result-index ctx))
             {[:dustingetz.reg/email "dustin@example.com"]
              {:db/id 17592186046196,
               :dustingetz.reg/email "dustin@example.com",
               :dustingetz.reg/name "Dustin Getz",
               :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
               :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}},
              [:dustingetz.reg/email "bob@example.com"]
              {:db/id 17592186046763,
               :dustingetz.reg/email "bob@example.com",
               :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
               :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}}))))

  (testing "fiddle level context is set"
    (is (= @(r/fmap :fiddle/ident (:hypercrud.browser/fiddle ctx))
           :dustingetz/gender-shirtsize))
    (is (= @(:hypercrud.browser/eav ctx)
           [nil nil nil]))
    ; result-enclosure, schemas, qfind, link-index, validation

    (is (= @(:hypercrud.browser/result-enclosure ctx)
           [[:dustingetz.reg/email
             :dustingetz.reg/name                           ; name is absent from second row
             #:dustingetz.reg{:gender [:db/ident]}
             #:dustingetz.reg{:shirt-size [:db/ident]}
             :db/id]]))
    )
  )

(def ctx-genders (mock-fiddle! :dustingetz/gender-shirtsize))

(deftest simple

  (testing "refocus to self is noop and doesn't crash"

    (is (= (extract (context/refocus+ ctx-genders :dustingetz/gender-shirtsize))
           ctx-genders)))

  (testing "refocus to dependent didn't crash"

    (is (= @(:hypercrud.browser/eav (extract (context/refocus+ ctx-genders :dustingetz.reg/gender)))
           [nil :dustingetz.reg/gender nil])))


  (testing "simple spreads"
    (is (= (count (for [[_ ctx] (context/spread-result ctx-genders)
                        [_ ctx] (context/spread-rows ctx)
                        [i ctx] (context/spread-elements ctx)
                        [a ctx] (context/spread-attributes ctx)]
                    [i a])
                  )
           (let [[fiddle result] (-> fixtures.tank/fiddles :dustingetz/gender-shirtsize)]
             (* (count result)
                (count (keys (first result))))))))

  (testing "don't need rows to spread attrs"
    (is (= (for [[_ ctx] (context/spread-result ctx-genders)
                 ; skip rows
                 [i ctx] (context/spread-elements ctx)
                 [a ctx] (context/spread-attributes ctx)]
             [a])
           '([:dustingetz.reg/email]
             [:dustingetz.reg/name]
             [:dustingetz.reg/gender]
             [:dustingetz.reg/shirt-size]
             [:db/id]))))

  (testing "infer element when find-element dimension=1"
    (is (= (for [[_ ctx] (context/spread-result ctx-genders)
                 ; skip rows
                 ; skip elements
                 [a ctx] (context/spread-attributes ctx)]
             [a])
           '([:dustingetz.reg/email]
             [:dustingetz.reg/name]
             [:dustingetz.reg/gender]
             [:dustingetz.reg/shirt-size]
             [:db/id]))))

  (testing "depth"
    (is (= (for [[_ ctx] (context/spread-result ctx-genders)
                 [_ ctx] (context/spread-rows ctx)]
             (context/depth ctx))
           '(1 1))))

  (testing "fiddle level a is nil (no longer fiddle-ident)"
    (is (= @(:hypercrud.browser/eav ctx-genders)
           [nil nil nil])))

  (testing "spread-result does not touch eav"
    (is (= [@(:hypercrud.browser/eav ctx-genders)]
           (for [[_ ctx] (context/spread-result ctx-genders)]
             @(:hypercrud.browser/eav ctx))
           '([nil nil nil]))))
  )

(deftest context-a
  (testing "fiddle-level v is the element"
    (is (= (for [[_ ctx] (context/spread-rows ctx-genders)
                 [_ ctx] (context/spread-elements ctx)]
             @(:hypercrud.browser/eav ctx))
           (for [[_ ctx] (context/spread-result ctx-genders)
                 [_ ctx] (context/spread-rows ctx)
                 [_ ctx] (context/spread-elements ctx)]
             @(:hypercrud.browser/eav ctx))
           (for [[_ ctx] (context/spread-result ctx-genders)
                 [_ ctx] (context/spread-rows ctx)
                 [_ ctx] (context/spread-elements ctx)]
             (context/eav ctx))
           '([nil nil [:dustingetz.reg/email "dustin@example.com"]]
             [nil nil [:dustingetz.reg/email "bob@example.com"]]))))

  (testing "spread-row doesnt prefill v, but it can be inferred"
    (is (= #_(for [[_ ctx] (context/spread-rows ctx)]
               @(:hypercrud.browser/eav ctx))               ; element not inferred, why no lookup ref? Because no schema.
          (for [[_ ctx] (context/spread-rows ctx-genders)]
            (context/eav ctx))
          [[nil nil [:dustingetz.reg/email "dustin@example.com"]]
           [nil nil [:dustingetz.reg/email "bob@example.com"]]]))

    (is (= (for [[_ ctx] (context/spread-rows ctx-genders)]
             (context/eav ctx))
           '([nil nil [:dustingetz.reg/email "dustin@example.com"]]
             [nil nil [:dustingetz.reg/email "bob@example.com"]]))))

  (is (= (for [[_ ctx] (context/spread-result ctx-genders)
               [_ ctx] (context/spread-rows ctx)
               [_ ctx] (context/spread-elements ctx)]
           @(:hypercrud.browser/eav ctx))
         '([nil nil [:dustingetz.reg/email "dustin@example.com"]]
           [nil nil [:dustingetz.reg/email "bob@example.com"]]))))

(deftest context-c
  (testing "fiddle/type :pull"
    (is (= (for [ctx [(mock-fiddle! :hyperfiddle/ide)]
                 [_ ctx] (context/spread-rows ctx)
                 [_ ctx] (context/spread-elements ctx)]
             (context/eav ctx))
           ;(for [ctx [(mock-fiddle! :hyperfiddle/ide)]]
           ;  (context/eav ctx))
           [[nil nil [:fiddle/ident :hyperfiddle/ide]]]))   ; no longer infers v without a row

    (is (= (let [ctx (mock-fiddle! :hyperfiddle/ide)
                 ctx (context/attribute ctx :fiddle/renderer)]
             (context/eav ctx))
           [[:fiddle/ident :hyperfiddle/ide] :fiddle/renderer "hyperfiddle.ide.fiddles.fiddle-src/fiddle-src-renderer"]))))

(deftest context-b
  (testing "nested pulls"
    (let [ctx (mock-fiddle! :hyperfiddle/ide)]
      ;(is (= (context/eav ctx) [nil nil [:fiddle/ident :hyperfiddle/ide]])) ; infer scalar
      ;(is (= (context/data ctx) 42))
      (testing "nested ref many"
        (let [ctx (context/attribute ctx :fiddle/links)]
          (is (= (context/eav ctx) [[:fiddle/ident :hyperfiddle/ide] :fiddle/links nil]))
          (is (= (count (context/data ctx)) 5))
          (is (= (first (context/data ctx)) {:db/id 17592186061848, :link/class [:hf/remove], :link/rel :hf/remove, :link/path :fiddle/ident}))
          (let [ctx (context/row ctx 17592186061848)]
            (is (= (context/eav ctx) [[:fiddle/ident :hyperfiddle/ide] :fiddle/links 17592186061848]))
            (is (= (context/data ctx) {:db/id 17592186061848, :link/class [:hf/remove], :link/rel :hf/remove, :link/path :fiddle/ident})))))

      (testing "nested scalar many"
        (let [ctx (context/attribute ctx :hyperfiddle/owners)]
          (is (= (context/eav ctx) [[:fiddle/ident :hyperfiddle/ide] :hyperfiddle/owners nil]))
          (is (= (context/data ctx) [#uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"]))
          (let [ctx (context/row ctx #uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39")]
            (is (= (context/eav ctx)
                   [[:fiddle/ident :hyperfiddle/ide] :hyperfiddle/owners #uuid "acd054a8-4e36-4d6c-a9ec-95bdc47f0d39"])))
          ))))

  (testing "nested pull on :hyperfiddle/ide :fiddle/links"
    (let [ctx (mock-fiddle! :hyperfiddle/ide)]
      ;(is (= (context/eav ctx) [nil nil [:fiddle/ident :hyperfiddle/ide]]))
      (let [ctx (context/attribute ctx :fiddle/links)]
        (is (= (context/eav ctx) [[:fiddle/ident :hyperfiddle/ide] :fiddle/links nil]))
        (is (= (count (context/data ctx)) 5))
        (is (= (first (context/data ctx)) {:db/id 17592186061848, :link/class [:hf/remove], :link/rel :hf/remove, :link/path :fiddle/ident}))
        (let [ctx (context/row ctx 17592186061848)]
          (is (= (context/eav ctx) [[:fiddle/ident :hyperfiddle/ide] :fiddle/links 17592186061848]))
          (is (= (context/data ctx) {:db/id 17592186061848, :link/class [:hf/remove], :link/rel :hf/remove, :link/path :fiddle/ident}))))))

  (testing "a is fiddle-ident if no element set"
    (is (= (for [[_ ctx] (context/spread-result ctx-genders)
                 [_ ctx] (context/spread-rows ctx)]
             ; Element got inferred here
             (context/data ctx))
           [{:db/id 17592186046196,
             :dustingetz.reg/email "dustin@example.com",
             :dustingetz.reg/name "Dustin Getz",
             :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
             :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}
            {:db/id 17592186046763,
             :dustingetz.reg/email "bob@example.com",
             :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
             :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}])))

  (testing "refocusing from a row doesn't lose the row"
    (is (= (for [[_ ctx] (-> (mock-fiddle! :dustingetz/gender-shirtsize)
                             (context/spread-result))
                 [_ ctx] (context/spread-rows ctx)]
             (context/eav (extract (context/refocus+ ctx :dustingetz/gender-shirtsize))))
           [[nil nil [:dustingetz.reg/email "dustin@example.com"]]
            [nil nil [:dustingetz.reg/email "bob@example.com"]]])))

  (testing "refocus"
    (is (= (for [[_ ctx] (context/spread-result ctx-genders)
                 [_ ctx] (context/spread-rows ctx)
                 [_ ctx] (context/spread-elements ctx)]
             ; Focusing the element
             @(:hypercrud.browser/eav ctx #_(extract (context/refocus+ ctx :dustingetz/gender-shirtsize))))
           '([nil nil [:dustingetz.reg/email "dustin@example.com"]]
             [nil nil [:dustingetz.reg/email "bob@example.com"]])))

    (is (= (for [[_ ctx] (context/spread-result ctx-genders)
                 [_ ctx] (context/spread-rows ctx)]
             @(:hypercrud.browser/eav
                (extract (context/refocus+ ctx :dustingetz.reg/gender))))
           '([[:dustingetz.reg/email "dustin@example.com"] :dustingetz.reg/gender :dustingetz.gender/male]
             [[:dustingetz.reg/email "bob@example.com"] :dustingetz.reg/gender :dustingetz.gender/male])))
    )

  )

(deftest context-d
  (testing "focus attribute isolated"
    (is (= (let [ctx (context/row ctx-genders [:dustingetz.reg/email "dustin@example.com"] #_17592186046196)
                 ctx (context/browse-element ctx 0)
                 ctx (context/attribute ctx :dustingetz.reg/gender)]
             (context/data ctx))
           #:db{:ident :dustingetz.gender/male})))

  (testing "FindColl Pull nested"
    (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                 ctx (context/row ctx :dustingetz.shirt-size/womens-medium)]
             (context/eav ctx))
           [nil nil :dustingetz.shirt-size/womens-medium]))

    (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                 ctx (context/row ctx :dustingetz.shirt-size/womens-medium)
                 ctx (context/attribute ctx :dustingetz.reg/gender)]
             (context/eav ctx))
           [:dustingetz.shirt-size/womens-medium :dustingetz.reg/gender :dustingetz.gender/female]))

    (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                 ctx (context/row ctx :dustingetz.shirt-size/womens-medium)
                 ;ctx (context/element ctx 0)
                 ctx (context/attribute ctx :db/ident)]
             (context/eav ctx))
           [:dustingetz.shirt-size/womens-medium :db/ident :dustingetz.shirt-size/womens-medium])))

  #_(testing "refocus from root to gender"
      (is (= (for [[_ ctx] (context/spread-rows ctx)
                   #_#_[_ ctx] (context/spread-elements ctx)]
               #_@(:hypercrud.browser/result-index ctx)
               @(:hypercrud.browser/result ctx)
               #_@(:hypercrud.browser/result-index (extract (context/refocus+ ctx :dustingetz.reg/gender)))
               )
             '({17592186046196 {:db/id 17592186046196,
                                :dustingetz.reg/email "dustin@example.com",
                                :dustingetz.reg/name "Dustin Getz",
                                :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
                                :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}},
                17592186046763 {:db/id 17592186046763,
                                :dustingetz.reg/email "bob@example.com",
                                :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
                                :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}}
               {17592186046196 {:db/id 17592186046196,
                                :dustingetz.reg/email "dustin@example.com",
                                :dustingetz.reg/name "Dustin Getz",
                                :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
                                :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}},
                17592186046763 {:db/id 17592186046763,
                                :dustingetz.reg/email "bob@example.com",
                                :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
                                :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}})))

      (is (= (for [[_ ctx] (context/spread-rows ctx)
                   [_ ctx] (context/spread-elements ctx)]
               @(:hypercrud.browser/eav (extract (context/refocus+ ctx :dustingetz.reg/gender))))
             '([17592186046196 :dustingetz.reg/gender :dustingetz.gender/male]
               [17592186046763 :dustingetz.reg/gender :dustingetz.gender/male])))
      ))

(deftest element-inference
  (testing "element inference"
    (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                 ctx (context/row ctx :dustingetz.shirt-size/womens-medium)
                 ctx (context/attribute ctx :db/ident)]
             (context/data ctx))
           :dustingetz.shirt-size/womens-medium))

    (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                 ctx (context/row ctx :dustingetz.shirt-size/womens-medium)]
             ; infer element
             (context/data ctx))
           {:db/ident :dustingetz.shirt-size/womens-medium, :dustingetz.reg/gender #:db{:ident :dustingetz.gender/female}}))))

(deftest data
  (testing "data"
    (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                 ctx (context/row ctx :dustingetz.shirt-size/womens-medium)
                 ctx (context/attribute ctx :dustingetz.reg/gender)]
             (context/data ctx))
           #:db{:ident :dustingetz.gender/female}))

    (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                 ctx (context/row ctx :dustingetz.shirt-size/womens-medium)
                 ctx (context/attribute ctx :db/ident)]
             (context/data ctx))
           :dustingetz.shirt-size/womens-medium))

    (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)
                 ctx (context/row ctx :dustingetz.shirt-size/womens-medium)]
             (context/data ctx))
           {:db/ident :dustingetz.shirt-size/womens-medium,
            :dustingetz.reg/gender #:db{:ident :dustingetz.gender/female}}))

    (is (= (let [ctx (mock-fiddle! :tutorial.race/shirt-sizes)]
             (context/data ctx))
           [{:db/ident :dustingetz.shirt-size/womens-medium, :dustingetz.reg/gender #:db{:ident :dustingetz.gender/female}}
            {:db/ident :dustingetz.shirt-size/womens-small, :dustingetz.reg/gender #:db{:ident :dustingetz.gender/female}}
            {:db/ident :dustingetz.shirt-size/womens-large, :dustingetz.reg/gender #:db{:ident :dustingetz.gender/female}}]))
    ))

(deftest rows
  (testing "row data by dbid"
    #_(:hypercrud.browser/result-path (context/row ctx 17592186046196))
    #_(:hypercrud.browser/result (context/row ctx 17592186046196))
    ; not sure if element can be inferred in data. Row does not infer.
    (is (= (context/data (-> ctx-genders
                             (context/browse-element 0)
                             (context/row [:dustingetz.reg/email "dustin@example.com"] #_17592186046196)))
           {:db/id 17592186046196,
            :dustingetz.reg/email "dustin@example.com",
            :dustingetz.reg/name "Dustin Getz",
            :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
            :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}))
    )

  (testing "row does not set e"
    (is (= @(:hypercrud.browser/eav (context/row ctx-genders 17592186046196))
           [nil nil nil])))
  (testing "target isolated row"
    (is (= (for [ctx [(context/row ctx-genders [:dustingetz.reg/email "dustin@example.com"] #_17592186046196)]]
             ; infers element
             (context/data ctx))
           (for [ctx [(context/row ctx-genders [:dustingetz.reg/email "dustin@example.com"] #_17592186046196)]
                 [_ ctx] (context/spread-elements ctx)]
             (context/data ctx))
           [{:db/id 17592186046196,
             :dustingetz.reg/email "dustin@example.com",
             :dustingetz.reg/name "Dustin Getz",
             :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
             :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}])))

  (testing "target all rows"
    (is (= (for [[_ ctx] (context/spread-rows ctx-genders)]
             (context/data ctx))
           (for [[_ ctx] (context/spread-rows ctx-genders)
                 [_ ctx] (context/spread-elements ctx)]
             (context/data ctx))
           [{:db/id 17592186046196,
             :dustingetz.reg/email "dustin@example.com",
             :dustingetz.reg/name "Dustin Getz",
             :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
             :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}
            {:db/id 17592186046763,
             :dustingetz.reg/email "bob@example.com",
             :dustingetz.reg/gender #:db{:ident :dustingetz.gender/male},
             :dustingetz.reg/shirt-size #:db{:ident :dustingetz.shirt-size/mens-large}}]))))

(deftest head-sentinel
  (testing "head-sentinel sanity checks, head-sentinel needs to go"
    (let [ctx (mock-fiddle! :seattle/neighborhoods)]
      (is (= (context/eav ctx) [nil nil nil]))
      (is (= (count (context/data ctx)) 7))
      (let [ctx (context/attribute ctx :neighborhood/district)]
        (is (= (context/eav ctx) [nil :neighborhood/district nil])))
      (let [ctx (context/row ctx [:neighborhood/name "Admiral (West Seattle)"] #_17592186045522)
            ctx (context/attribute ctx :neighborhood/district)]
        (is (= (context/eav ctx)
               [[:neighborhood/name "Admiral (West Seattle)"] :neighborhood/district [:district/name "Southwest"]])))
      (testing "if head-sentinel, no v"
        (let [ctx (assoc ctx :hypercrud.browser/head-sentinel true)
              ctx (context/row ctx 17592186045522)
              ctx (context/attribute ctx :neighborhood/district)]
          (is (= (context/eav ctx) [nil :neighborhood/district nil]))))))
  )

(deftest refocus-a
  (testing "refocus to :ref :one from row"
    (is (= (let [ctx (context/row ctx-genders [:dustingetz.reg/email "dustin@example.com"] #_17592186046196)
                 ctx (context/browse-element ctx 0)
                 ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
             (context/data ctx))
           #:db{:ident :dustingetz.gender/male}))

    (is (= (for [[_ ctx] (context/spread-rows ctx-genders)
                 [_ ctx] (context/spread-elements ctx)]
             (let [ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
               (context/data ctx)))
           (for [[_ ctx] (context/spread-rows ctx-genders)]
             ; infer element
             (let [ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
               (context/data ctx)))
           [#:db{:ident :dustingetz.gender/male}
            #:db{:ident :dustingetz.gender/male}]))

    (is (= (let [ctx (context/row ctx-genders [:dustingetz.reg/email "dustin@example.com"] #_17592186046196)
                 ctx (context/browse-element ctx 0)
                 ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
             @(:hypercrud.browser/eav ctx))
           (let [ctx (context/row ctx-genders [:dustingetz.reg/email "dustin@example.com"] #_17592186046196)
                 ; infer element
                 ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
             @(:hypercrud.browser/eav ctx))
           [[:dustingetz.reg/email "dustin@example.com"] :dustingetz.reg/gender :dustingetz.gender/male]))

    (is (= (for [[_ ctx] (context/spread-rows ctx-genders)
                 [_ ctx] (context/spread-elements ctx)]
             (let [ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
               @(:hypercrud.browser/eav ctx)))
           (for [[_ ctx] (context/spread-rows ctx-genders)]
             ; infer elements
             (let [ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
               @(:hypercrud.browser/eav ctx)))
           '([[:dustingetz.reg/email "dustin@example.com"] :dustingetz.reg/gender :dustingetz.gender/male]
             [[:dustingetz.reg/email "bob@example.com"] :dustingetz.reg/gender :dustingetz.gender/male]))))

  (testing "refocus to :ref :many from row")
  (testing "refocus from myself when already here but not at root")

  )

(deftest refocus-b
  (testing "refocus to :ref :one from top"
    (is (= (let [ctx (extract (context/refocus+ ctx-genders :dustingetz.reg/gender))]
             ; Can't do it. Should this throw?
             (context/data ctx))
           nil))

    (is (= (for [[_ ctx] (context/spread-rows ctx-genders)
                 [_ ctx] (context/spread-elements ctx)]
             (let [ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
               (context/data ctx)))
           [#:db{:ident :dustingetz.gender/male}
            #:db{:ident :dustingetz.gender/male}]))

    (is (= (let [ctx (context/row ctx-genders [:dustingetz.reg/email "dustin@example.com"] #_17592186046196)
                 ctx (context/browse-element ctx 0)
                 ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
             @(:hypercrud.browser/eav ctx))
           [[:dustingetz.reg/email "dustin@example.com"] :dustingetz.reg/gender :dustingetz.gender/male]))

    (is (= (for [[_ ctx] (context/spread-rows ctx-genders)
                 [_ ctx] (context/spread-elements ctx)]
             (let [ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
               @(:hypercrud.browser/eav ctx)))
           [[[:dustingetz.reg/email "dustin@example.com"] :dustingetz.reg/gender :dustingetz.gender/male]
            [[:dustingetz.reg/email "bob@example.com"] :dustingetz.reg/gender :dustingetz.gender/male]])))
  )

(deftest refocus-and-attribute-parallels
  (testing "refocus and attribute are nearly the same thing"
    (let [ctx (mock-fiddle! :tutorial.race/submission)]
      (let [ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
        (is (= (context/smart-entity-identifier ctx (context/data ctx))
               :dustingetz.gender/male)))
      (let [ctx (context/attribute ctx :dustingetz.reg/gender)]
        (is (= (context/smart-entity-identifier ctx (context/data ctx))
               :dustingetz.gender/male)))))
  )

(deftest smart-identity
  (testing "smart-entity-identifier db/id"
    (let [ctx (mock-fiddle! :tutorial.race/submission)
          e (context/data ctx)]
      (is (= e
             {:db/id 17592186046196,
              :dustingetz.reg/email "dustin@example.com",
              :dustingetz.reg/name "Dustin Getz",
              :dustingetz.reg/age 102,
              :dustingetz.reg/birthdate #inst "2018-09-13T00:00:00.000-00:00",
              :dustingetz.reg/gender {:db/ident :dustingetz.gender/male},
              :dustingetz.reg/shirt-size {:db/ident :dustingetz.shirt-size/mens-large}}))
      (is (= (context/smart-entity-identifier ctx e)
             17592186046196))))

  (testing "smart-entity-identifier db/ident"
    (let [ctx (mock-fiddle! :tutorial.race/submission)]
      (let [ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
        (is (= (context/smart-entity-identifier ctx (context/data ctx))
               :dustingetz.gender/male)))
      (let [ctx (extract (context/refocus+ ctx :dustingetz.reg/gender))]
        (is (= (context/smart-entity-identifier ctx (context/data ctx))
               :dustingetz.gender/male)))))

  (testing "smart-entity-identifier :db.unique/identity"
    (let [ctx (mock-fiddle! :dustingetz/slack-storm)
          [[e tx]] (context/data ctx)]
      (is (= e
             {:db/id 17592186047000,
              :dustingetz.post/title "is js/console.log syntax future proof?",
              :dustingetz.post/slug :asdf,
              :dustingetz.storm/channel "#clojurescript",
              :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"}))

      ; needs element to know schema, otherwise can't be done
      (is (= (context/smart-entity-identifier (context/browse-element ctx 0) e) [:dustingetz.post/slug :asdf]))
      (is (= (context/smart-entity-identifier (context/browse-element ctx 1) e) 17592186047000)) ; Untangle, this should probably assert
      (is (= (context/smart-entity-identifier ctx e) 17592186047000)) ; Untangle, this should probably assert
      ))
  )

(def ctx-slack (mock-fiddle! :dustingetz/slack-storm))
(def ctx-blog (mock-fiddle! :dustingetz.tutorial/blog))
(def ctx-blog-slug (-> (mock-fiddle! :dustingetz.tutorial/blog)
                       (context/row [[:dustingetz.post/slug :automatic-CRUD-links]])
                       (context/browse-element 0)
                       (context/attribute :dustingetz.post/slug)))

(deftest formulas
  (testing "slack storm identity selection"
    (is (= (for [[_ ctx] (context/spread-rows ctx-slack)
                 [_ ctx] (context/spread-elements ctx)
                 #_#_[_ ctx] (context/spread-attributes ctx)]
             #_(context/data ctx)
             (context/eav ctx)
             )
           [[nil nil [:dustingetz.post/slug :asdf]]
            [nil nil 13194139535895]
            [nil nil 17592186047105]
            [nil nil 13194139536000]])))

  #_(testing "smart identity should prefer identity attrs to db/id"
      (is (= (for [[_ ctx] (context/spread-rows ctx)
                   [_ ctx] (context/spread-elements ctx)
                   #_#_[_ ctx] (context/spread-attributes ctx)]
               (context/eav ctx))
             [[nil :dustingetz/slack-storm 17592186047000]
              [nil :dustingetz/slack-storm 13194139535895]  ; tx
              [nil :dustingetz/slack-storm 17592186047105]
              [nil :dustingetz/slack-storm 13194139536000]  ; tx
              ]))
      )

  #_(testing "FindRel fiddle-level, how to identify each rel?"
      (is (= (for [[_ ctx] (context/spread-rows ctx)
                   [_ ctx] (context/spread-elements ctx)
                   #_#_[_ ctx] (context/spread-attributes ctx)]
               (context/eav ctx))

             ; I think these shouldn't be addressable at all from fiddle-level.
             [[nil :dustingetz/slack-storm [:dustingetz.post/slug :asdf]]
              [nil :dustingetz/slack-storm 13194139535895]  ; (max ?tx)
              [nil :dustingetz/slack-storm 17592186047105]  ; ?e but :identity attr is nil
              [nil :dustingetz/slack-storm 13194139536000]  ; (max ?tx)
              ]))
      )

  (testing "addressing rowtuple by tupled rowkey"
    (is (= (let [ctx (mock-fiddle! :dustingetz/slack-storm)]
             (for [ctx [(context/row ctx [[:dustingetz.post/slug :asdf] 13194139535895])]]
               (context/data ctx)))
           [[{:db/id 17592186047000,
              :dustingetz.post/title "is js/console.log syntax future proof?",
              :dustingetz.post/slug :asdf,
              :dustingetz.storm/channel "#clojurescript",
              :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"}
             13194139535895]])))

  (testing "extract tuples"
    (is (= (let [ctx (mock-fiddle! :dustingetz/slack-storm)]
             (for [[_ ctx] (context/spread-rows ctx)]
               (context/data ctx)))
           [[{:db/id 17592186047000,
              :dustingetz.post/title "is js/console.log syntax future proof?",
              :dustingetz.post/slug :asdf,
              :dustingetz.storm/channel "#clojurescript",
              :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"}
             13194139535895]
            [{:db/id 17592186047105,
              :dustingetz.post/title "large strings and high churn attrs blow out indexes",
              :dustingetz.storm/channel "#datomic",
              :dustingetz.post/published-date #inst"2018-11-21T00:00:00.000-00:00"}
             13194139536000]])))

  (testing "flatten to elements"
    (is (= (let [ctx (mock-fiddle! :dustingetz/slack-storm)]
             (for [[_ ctx] (context/spread-rows ctx)
                   [_ ctx] (context/spread-elements ctx)]
               (context/data ctx)))
           [{:db/id 17592186047000,
             :dustingetz.post/title "is js/console.log syntax future proof?",
             :dustingetz.post/slug :asdf,
             :dustingetz.storm/channel "#clojurescript",
             :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"}
            13194139535895
            {:db/id 17592186047105,
             :dustingetz.post/title "large strings and high churn attrs blow out indexes",
             :dustingetz.storm/channel "#datomic",
             :dustingetz.post/published-date #inst"2018-11-21T00:00:00.000-00:00"}
            13194139536000])))

  (testing "rowkey then all elements"
    (is (= (let [ctx (mock-fiddle! :dustingetz/slack-storm)]
             (for [ctx [(context/row ctx [[:dustingetz.post/slug :asdf] 13194139535895])]
                   [_ ctx] (context/spread-elements ctx)]
               (context/data ctx)))
           [{:db/id 17592186047000,
             :dustingetz.post/title "is js/console.log syntax future proof?",
             :dustingetz.post/slug :asdf,
             :dustingetz.storm/channel "#clojurescript",
             :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"}
            13194139535895])))

  (testing "rowkey then single elements"
    (is (let [ctx (mock-fiddle! :dustingetz/slack-storm)
              ctx (context/row ctx [[:dustingetz.post/slug :asdf] 13194139535895])]
          (is (= (context/eav ctx) [nil nil nil]))          ; could potentially set v to the rowkey here
          (is (= (context/data (context/browse-element ctx 0))
                 {:db/id 17592186047000,
                  :dustingetz.post/title "is js/console.log syntax future proof?",
                  :dustingetz.post/slug :asdf,
                  :dustingetz.storm/channel "#clojurescript",
                  :dustingetz.post/published-date #inst "2018-11-19T00:00:00.000-00:00"}))
          (is (= (context/data (context/browse-element ctx 1)) 13194139535895)))))

  (testing "element level does set value, it is not ambigous due to set semantics in FindRel"
    (is (let [ctx (mock-fiddle! :dustingetz/slack-storm)
              ctx (context/row ctx [[:dustingetz.post/slug :asdf] 13194139535895])]
          (is (= (context/eav ctx) [nil nil nil]))          ; could potentially set v to the rowkey here
          ; Originally thought:
          ; Can't set v to [:dustingetz.post/slug :asdf] because A is ambiguous.
          ; Can't set v to 5895 because A is ambiguous.
          ; NO, wrong.
          ; Arguably this is NOT ambiguous due to set semantics. The same entity can't be pulled twice in a find relation.
          (is (= (context/eav (context/browse-element ctx 0)) [nil nil [:dustingetz.post/slug :asdf]]))
          (is (= (context/eav (context/browse-element ctx 1)) [nil nil 13194139535895])))))

  (testing "key all the way and then attributes"
    (let [ctx (mock-fiddle! :dustingetz/slack-storm)
          ctx (context/row ctx [[:dustingetz.post/slug :asdf] 13194139535895])
          ctx (context/browse-element ctx 0)]
      (is (= (context/eav ctx) [nil nil [:dustingetz.post/slug :asdf]]))
      (is (= (context/data ctx)
             {:db/id 17592186047000,
              :dustingetz.post/title "is js/console.log syntax future proof?",
              :dustingetz.post/slug :asdf,
              :dustingetz.storm/channel "#clojurescript",
              :dustingetz.post/published-date #inst "2018-11-19T00:00:00.000-00:00"}))
      (let [ctx (context/attribute ctx :dustingetz.post/title)]
        (is (= (context/data ctx) "is js/console.log syntax future proof?"))
        (is (= (context/eav ctx) [[:dustingetz.post/slug :asdf] :dustingetz.post/title "is js/console.log syntax future proof?"])))

      (is (= (for [[_ ctx] (context/spread-attributes ctx)]
               (context/eav ctx))
             [[[:dustingetz.post/slug :asdf] :db/id 17592186047000]
              [[:dustingetz.post/slug :asdf] :dustingetz.post/title "is js/console.log syntax future proof?"]
              [[:dustingetz.post/slug :asdf] :dustingetz.post/slug :asdf]
              [[:dustingetz.post/slug :asdf] :dustingetz.storm/channel "#clojurescript"]
              [[:dustingetz.post/slug :asdf] :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"]]))))


  (testing "FindRel tuple at attr level")

  (testing "FindRel attr-level, how to identify each rel?"

    (is (= (let [ctx (mock-fiddle! :dustingetz/slack-storm)]
             (for [#_#_[_ ctx] (context/spread-rows ctx)
                   ctx [(context/row ctx [[:dustingetz.post/slug :asdf] 13194139535895])]
                   [_ ctx] (context/spread-elements ctx)
                   #_#_[_ ctx] (context/spread-attributes ctx)]
               (context/eav ctx)))
           [[nil nil [:dustingetz.post/slug :asdf]]
            [nil nil 13194139535895]]))

    (is (= (let [ctx (mock-fiddle! :dustingetz/slack-storm)]
             (for [ctx [(context/row ctx [[:dustingetz.post/slug :asdf] 13194139535895])]
                   [_ ctx] (context/spread-elements ctx)
                   [_ ctx] (context/spread-attributes ctx)]
               (context/eav ctx)))
           [[[:dustingetz.post/slug :asdf] :db/id 17592186047000]
            [[:dustingetz.post/slug :asdf] :dustingetz.post/title "is js/console.log syntax future proof?"]
            [[:dustingetz.post/slug :asdf] :dustingetz.post/slug :asdf]
            [[:dustingetz.post/slug :asdf] :dustingetz.storm/channel "#clojurescript"]
            [[:dustingetz.post/slug :asdf] :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"]]))

    (is (= (let [ctx (mock-fiddle! :dustingetz/slack-storm)]
             (for [ctx [(context/row ctx [[:dustingetz.post/slug :asdf] 13194139535895])]
                   [_ ctx] (context/spread-elements ctx)
                   [_ ctx] (context/spread-attributes ctx)]
               (context/eav ctx)))
           [[[:dustingetz.post/slug :asdf] :db/id 17592186047000]
            [[:dustingetz.post/slug :asdf] :dustingetz.post/title "is js/console.log syntax future proof?"]
            [[:dustingetz.post/slug :asdf] :dustingetz.post/slug :asdf]
            [[:dustingetz.post/slug :asdf] :dustingetz.storm/channel "#clojurescript"]
            [[:dustingetz.post/slug :asdf] :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"]]))

    (is (= (let [ctx (mock-fiddle! :dustingetz/slack-storm)]
             (for [[_ ctx] (context/spread-rows ctx)
                   [_ ctx] (context/spread-elements ctx)
                   [_ ctx] (context/spread-attributes ctx)]
               (context/eav ctx)))
           [[[:dustingetz.post/slug :asdf] :db/id 17592186047000]
            [[:dustingetz.post/slug :asdf] :dustingetz.post/title "is js/console.log syntax future proof?"]
            [[:dustingetz.post/slug :asdf] :dustingetz.post/slug :asdf]
            [[:dustingetz.post/slug :asdf] :dustingetz.storm/channel "#clojurescript"]
            [[:dustingetz.post/slug :asdf] :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"]
            [17592186047105 :db/id 17592186047105]
            [17592186047105 :dustingetz.post/title "large strings and high churn attrs blow out indexes"]
            [17592186047105 :dustingetz.post/slug nil]
            [17592186047105 :dustingetz.storm/channel "#datomic"]
            [17592186047105 :dustingetz.post/published-date #inst"2018-11-21T00:00:00.000-00:00"]]))
    )

  (testing "link primitives"

    (let [xx (hyperfiddle.data/spread-links-here ctx-blog-slug)]
      (is (= (map first xx)
            ; Hash is not cross platform here. The vals are proven equal below. Problem?
             #?(:cljs [-149644189 -1537857194]
                :clj [925230688 1235745664])))

      (is (= (map (comp deref second) xx)
            ; Test passes both clj and cljs, proving the values are equal on both platforms,
            ; it is the hashing in spread-links-here causing above divergence
            [{:db/id 17592186047370,
              :link/fiddle {:db/id 17592186047371,
                            :fiddle/ident 'dustingetz.tutorial/view-post,
                            :fiddle/type :entity,
                            :fiddle/pull-database "$",
                            :fiddle/pull "[:db/id\n *]",
                            :fiddle/markdown "### dustingetz.tutorial/view-post"}
              :link/path :dustingetz.post/slug,
              :link/rel :hf/self}
             {:db/id 17592186047372,
              :link/class [:hf/new],
              :link/fiddle {:db/id 17592186047373,
                            :fiddle/ident 'dustingetz.tutorial.blog/new-post,
                            :fiddle/type :entity,
                            :fiddle/pull-database "$",
                            :fiddle/pull "[:db/id\n *]",
                            :fiddle/markdown "### dustingetz.tutorial.blog/new-post"}
              :link/path :dustingetz.post/slug,
              :link/rel :hf/new,
              :link/tx-fn :user/new-post}])))

    (is (= (->> (hyperfiddle.data/spread-links-here ctx-blog-slug) (map (comp :link/path deref second)))
           [:dustingetz.post/slug :dustingetz.post/slug]))

    (is (= (->> (hyperfiddle.data/spread-links-here ctx-blog-slug :hf/new) (mapv (comp :link/path deref second)))
           [:dustingetz.post/slug]))

    (is (= (->> (hyperfiddle.data/spread-links-here ctx-blog-slug `dustingetz.tutorial/view-post)
                (mapv (comp :link/path deref second)))
           [:dustingetz.post/slug]))

    (is (= (mlet [r-link (hyperfiddle.data/select-here+ ctx-blog-slug :hf/new1)]
             (return 42))
           (left "no match for class: :hf/new1")))

    (is (= (->> (hyperfiddle.data/select-here+ ctx-blog-slug :hf/new) (unwrap (constantly nil)) deref :link/path)
           :dustingetz.post/slug)))

  )

(def ctx-blog1 (-> (mock-fiddle! :dustingetz.tutorial/blog)
                   (context/row [[:dustingetz.post/slug :automatic-CRUD-links]])
                   (context/browse-element 0)))

(deftest identity-focusing
  (testing "identity focusing"
    (is (= (context/data ctx-blog)
           [[{:db/id 17592186047105,
              :dustingetz.post/published-date #inst"2018-11-21T00:00:00.000-00:00",
              :dustingetz.post/title "large strings and high churn attrs blow out indexes",
              :dustingetz.post/slug :large-strings-and-high-churn-attrs-blow-out-indexes}]
            [{:db/id 17592186047142,
              :dustingetz.post/published-date #inst"2018-11-22T15:57:34.277-00:00",
              :dustingetz.post/title "automatic CRUD links",
              :dustingetz.post/slug :automatic-CRUD-links}]]))
    (is (= (for [[_ ctx] (context/spread-elements ctx-blog)]
             (context/eav ctx))
           [[nil nil nil]]))

    ;(is (= (for [[_ ctx] (context/spread-elements ctx)]
    ;         ; Asking for data without a row and unable to infer row
    ;         (context/data ctx))
    ;       nil))

    (is (= (for [[_ ctx] (context/spread-rows ctx-blog)
                 [_ ctx] (context/spread-elements ctx)]
             (context/data ctx))
           [{:db/id 17592186047105,
             :dustingetz.post/published-date #inst"2018-11-21T00:00:00.000-00:00",
             :dustingetz.post/title "large strings and high churn attrs blow out indexes",
             :dustingetz.post/slug :large-strings-and-high-churn-attrs-blow-out-indexes}
            {:db/id 17592186047142,
             :dustingetz.post/published-date #inst"2018-11-22T15:57:34.277-00:00",
             :dustingetz.post/title "automatic CRUD links",
             :dustingetz.post/slug :automatic-CRUD-links}]))

    (is (= (for [[_ ctx] (context/spread-rows ctx-blog)
                 [_ ctx] (context/spread-elements ctx)]
             (context/eav ctx))
           [[nil nil [:dustingetz.post/slug :large-strings-and-high-churn-attrs-blow-out-indexes]]
            [nil nil [:dustingetz.post/slug :automatic-CRUD-links]]]))

    (is (= (for [[_ ctx] (context/spread-rows ctx-blog)
                 [_ ctx] (context/spread-elements ctx)]
             (context/eav ctx))
           [[nil nil [:dustingetz.post/slug :large-strings-and-high-churn-attrs-blow-out-indexes]]
            [nil nil [:dustingetz.post/slug :automatic-CRUD-links]]]))

    (is (= (context/row-key ctx-blog [{:db/id 17592186047142,
                                       :dustingetz.post/published-date #inst"2018-11-22T15:57:34.277-00:00",
                                       :dustingetz.post/title "automatic CRUD links",
                                       :dustingetz.post/slug :automatic-CRUD-links}])
           ; Wrapper tuple. This kind of makes row-keys confusing for userland to figure out, they have to call
           ; the key-row api.
           [[:dustingetz.post/slug :automatic-CRUD-links]]))
    (is (= (:hypercrud.browser/result-path ctx-blog1) [[[:dustingetz.post/slug :automatic-CRUD-links]] 0]))
    (is (= (keys @(:hypercrud.browser/result-index ctx-blog1))
           [[[:dustingetz.post/slug :large-strings-and-high-churn-attrs-blow-out-indexes]]
            [[:dustingetz.post/slug :automatic-CRUD-links]]]))

    (is (= (context/data ctx-blog1)
           {:db/id 17592186047142,
            :dustingetz.post/published-date #inst"2018-11-22T15:57:34.277-00:00",
            :dustingetz.post/title "automatic CRUD links",
            :dustingetz.post/slug :automatic-CRUD-links}))
    (is (= (context/eav ctx-blog1)
           [nil nil [:dustingetz.post/slug :automatic-CRUD-links]]))

    #_(let [#_#_ctx (context/browse-element ctx 0)]
        (is (= (context/eav ctx) [nil :dustingetz/slack-storm [:dustingetz.post/slug :asdf]]))
        (is (= (context/data ctx)
               {:db/id 17592186047000,
                :dustingetz.post/title "is js/console.log syntax future proof?",
                :dustingetz.post/slug :asdf,
                :dustingetz.storm/channel "#clojurescript",
                :dustingetz.post/published-date #inst "2018-11-19T00:00:00.000-00:00"}))
        (let [ctx (context/attribute ctx :dustingetz.post/title)]
          (is (= (context/data ctx) "is js/console.log syntax future proof?"))
          (is (= (context/eav ctx) [[:dustingetz.post/slug :asdf] :dustingetz.post/title "is js/console.log syntax future proof?"])))

        (is (= (for [[_ ctx] (context/spread-attributes ctx)]
                 (context/eav ctx))
               [[[:dustingetz.post/slug :asdf] :db/id 17592186047000]
                [[:dustingetz.post/slug :asdf] :dustingetz.post/title "is js/console.log syntax future proof?"]
                [[:dustingetz.post/slug :asdf] :dustingetz.post/slug :asdf]
                [[:dustingetz.post/slug :asdf] :dustingetz.storm/channel "#clojurescript"]
                [[:dustingetz.post/slug :asdf] :dustingetz.post/published-date #inst"2018-11-19T00:00:00.000-00:00"]])))

    ))

(def ctx-blog2 (-> (mock-fiddle! :dustingetz.tutorial/blog)
                   (context/row [[:dustingetz.post/slug :automatic-CRUD-links]])
                   (context/browse-element 0)
                   (context/attribute :dustingetz.post/slug)))

(def ctx-seattle1 (-> (mock-fiddle! :dustingetz.seattle/communities)
                      (context/row 17592186045537)
                      (context/attribute :community/neighborhood)
                      (context/attribute :neighborhood/district)
                      (context/attribute :district/region)))

#?(:clj
   (deftest more-link-stuff
     (testing ":identity link refocus v is lookup-ref"
       (def r-link (->> (hyperfiddle.data/select-here+ ctx-blog2 `dustingetz.tutorial/view-post)
                        (unwrap #(throw (ex-info % {})))))
       (is (= (mlet [[ctx ?route] (context/refocus-build-route-and-occlude+ ctx-blog2 r-link)]
                    (context/eav ctx))
              [nil nil [:dustingetz.post/slug :automatic-CRUD-links]]))
       )

     (testing "at fiddle level, link :hf/new eav does not have a parent"
       (let [link1 (extract (hyperfiddle.data/select-here+ ctx-blog2 :hf/new))]
         (is (= (mlet [[ctx r+route] (context/refocus-build-route-and-occlude+ ctx-blog2 link1)]
                      (context/eav ctx))
                ; should it be [nil nil "479925454"] from the txfn perspective?
                [nil nil "hyperfiddle.tempid--853640389"]))
         (is (= (mlet [[ctx +route] (context/refocus-build-route-and-occlude+ ctx-blog2 link1)]
                      (return +route))
                (right `(dustingetz.tutorial.blog/new-post ~#entity["$" "hyperfiddle.tempid--853640389"]))))))

     (testing "iframe at double nested attr"
       (is (= (context/eav ctx-seattle1) [[:district/name "Ballard"] :district/region :region/nw]))
       (is @(->> (hyperfiddle.data/select-here+ ctx-seattle1 :hf/iframe) (unwrap (constantly nil))))
       ; Really we want to assert it was hydrated but no tests for that yet
       )
     ))

#_(testing "refocus link from tupled qfind, identity focused from element ctx"
    (def ctx (-> (mock-fiddle! :dustingetz.tutorial/blog)
                 (context/row [[:dustingetz.post/slug :automatic-CRUD-links]])
                 #_(context/browse-element 0)
                 (context/attribute :dustingetz.post/slug)))
    (def r-link (->> (hyperfiddle.data/select-here+ ctx :hf/new) (unwrap (constantly nil))))
    (testing "at fiddle level, link :hf/new eav does not have a parent"
      (is (= (let [[ctx r+route] (context/refocus-build-route-and-occlude+ ctx r-link)]
               (context/eav ctx))
             ; should it be [nil nil "479925454"] from the txfn perspective?
             [nil :dustingetz.tutorial/blog "479925454"])))
    )

(def ctx-blog3 (-> (mock-fiddle! :dustingetz.tutorial/blog) ; FindRel-1
                   (context/row [[:dustingetz.post/slug :automatic-CRUD-links]])
                   #_(context/browse-element 0)             ; Specifically no element
                   ))

#?(:clj
   (deftest yo
     (testing "refocus link from tupled qfind, identity focused from result ctx"
       ; Select a link from the root context that is reachable but not exactly here.
       ; So select post/slug from a row ctx, as can happen in a view but not an autogrid.
       ; This exercises tag-v-with-color edge cases

       (def link (hyperfiddle.data/select ctx-blog3 :hf/new))
       (is (= (mlet [[ctx _route] (context/refocus-build-route-and-occlude+ ctx-blog3 link)]
                    (context/eav ctx))
              [nil nil "hyperfiddle.tempid--853640389"]))
       (is (= (mlet [[ctx route] (context/refocus-build-route-and-occlude+ ctx-blog3 link)]
                    (return route))
              ; This works because refocus hardcodes element 0, which it turns out is almost always
              ; what the custom renderer wants.
              (right `(dustingetz.tutorial.blog/new-post ~#entity["$" "hyperfiddle.tempid--853640389"])))))))

(def ctx-schema (mock-fiddle! :dustingetz.test/schema-ident-findcoll))

(deftest schema-bug-part-1
  (testing "works"
    (is (= (let [ctx (context/row ctx-schema :db.sys/partiallyIndexed)]
             (context/data ctx))
           #:db{:ident :db.sys/partiallyIndexed}))
    (is (= (let [ctx (context/row ctx-schema :db.sys/partiallyIndexed)]
             (context/eav ctx))
           [nil nil :db.sys/partiallyIndexed]))
    (is (= (let [ctx (context/row ctx-schema :db.sys/partiallyIndexed)]
             (context/row-key ctx (context/data ctx)))
           :db.sys/partiallyIndexed))
    (is (= (let [ctx (context/row ctx-schema :db.sys/partiallyIndexed)]
             (context/row-key-v ctx (context/data ctx)))
           :db.sys/partiallyIndexed))

    (is (= (let [ctx (context/row ctx-schema :db.sys/partiallyIndexed)
                 ctx (context/browse-element ctx 0)]
             (context/eav ctx))
           [nil nil :db.sys/partiallyIndexed]))
    (is (= (let [ctx (context/row ctx-schema :db.sys/partiallyIndexed)
                 ctx (context/browse-element ctx 0)]
             (context/data ctx))
           #:db{:ident :db.sys/partiallyIndexed}))

    (is (= (count (for [[_ ctx] (context/spread-rows ctx-schema)]
                    nil))
           97))))

(deftest findcoll-ident-card-many
  (is (-> (let [ctx (mock-fiddle! :dustingetz.test/findcoll-ident-cardinality-many)]
            (for [[_ ctx] (context/spread-rows ctx)]
              (context/data ctx)))
          count (> 0)))

  )

;(deftest schema-but-part-2
;  (testing "Crashes on the fifth row which is :db.install/valueType"
;    (let [ctx (mock-fiddle! :dustingetz.test/schema-ident-findcoll)]
;      (->> (for [[_ ctx] (drop 1 (context/spread-rows ctx))]
;             (try
;               (context/data ctx)
;               [(:hypercrud.browser/result-path ctx)
;                nil
;                #_@(r/cursor (:hypercrud.browser/result-index ctx) (:hypercrud.browser/result-path ctx))]
;               (catch Exception e
;                 [
;                  (:hypercrud.browser/result-path ctx)
;                  e
;                  #_@(r/cursor (:hypercrud.browser/result-index ctx) (:hypercrud.browser/result-path ctx))
;                  ])))
;           (filter second)
;           doall))
;    )
;
;  (testing "I dont know why this crashes"
;    (def ctx (mock-fiddle! :dustingetz.test/schema-ident-findcoll))
;    (is (= (let [ctx (context/row ctx :db.install/valueType)]
;             (context/data ctx))
;           #:db{:ident :db.sys/partiallyIndexed}))
;    (is (= (let [ctx (context/row ctx :db.install/valueType)]
;             (context/eav ctx))
;           [nil :seattle/neighborhoods :db.sys/partiallyIndexed]))
;    (is (= (let [ctx (context/row ctx :db.install/valueType)]
;             (context/row-key ctx (context/data ctx)))
;           :db.install/valueType))
;    (is (= (let [ctx (context/row ctx :db.install/valueType)]
;             (context/row-key-v ctx (context/data ctx)))
;           :db.install/valueType))
;
;    (for [[_ ctx] (context/spread-rows ctx)]
;      (context/eav ctx))
;
;
;
;    (for [[_ ctx] (context/spread-rows ctx)
;          [_ ctx] (context/spread-elements ctx)]
;      (context/data ctx))
;
;    ))

(def ctx-neighborhoods (mock-fiddle! :seattle/neighborhoods))

(deftest links111
  ;(testing "refocus link from scalar qfind, identity focused from result ctx (infer element)"
  ;  (is true))
  ;(testing "refocus link from scalar qfind, identity focused from element ctx"
  ;  (is true))

  (testing "race shirt-sizes iframe"
    (let [ctx (mock-fiddle! :tutorial.race/submission)]
      (is (-> (hyperfiddle.data/select-many ctx :dustingetz.reg/gender)
              first :link/fiddle :fiddle/ident (= :tutorial.race/shirt-sizes)))
      ;(println @(hyperfiddle.data/select-many ctx :tutorial.race/submission))
      (is (= 2 (count (hyperfiddle.data/select-many ctx :hf/iframe)))))
    )

  (testing "seattle neighborhood districts iframe"

    (is (-> @(hyperfiddle.data/select-many-here ctx-neighborhoods :seattle/districts)
            first :link/fiddle :fiddle/ident (= :seattle/districts)))

    #_(testing "if head-sentinel, no v"
        (let [ctx (assoc ctx :hypercrud.browser/head-sentinel true)
              ctx (context/row ctx 17592186045522)
              ctx (context/attribute ctx :neighborhood/district)]
          (is (= (context/eav ctx) [nil :neighborhood/district nil])))))

  (testing "indexed-links"
    (is @(:hypercrud.browser/link-index ctx-neighborhoods))
    (is (context/links-at @(:hypercrud.browser/link-index ctx-neighborhoods) #{}))
    (is (context/links-in-dimension ctx-neighborhoods #{}))
    )
  )

; broken without schema alias support
#_(deftest schema-aliases
    (let [ctx (mock-fiddle! :cookbook/markdown-table)]
      (is (= (context/data ctx)
             [{:db/id 17592186046744, :task/title "Feed baby", :task/completed true}
              {:db/id 17592186046745, :task/title "Mow the lawn"}
              {:db/id 17592186046746, :task/title "Do the dishes", :task/completed true}])))

    (let [ctx (-> (mock-fiddle! :cookbook/markdown-table)   ; FindColl
                  (context/row 17592186046744)
                  ;(context/element 0)                        ; required for FindRel
                  #_(context/attribute :dustingetz.post/slug))]
      (is (= (context/data ctx)
             {:db/id 17592186046744, :task/title "Feed baby", :task/completed true}))
      (is (= (context/eav ctx)
             [nil :cookbook/markdown-table 17592186046744]))
      (is (= (for [ctx [(context/row ctx 17592186046744)]
                   [_ ctx] (context/spread-elements ctx)
                   [_ ctx] (context/spread-attributes ctx)]
               (context/eav ctx))
             [[nil :db/id nil]
              [nil :cookbook/markdown-table nil]
              [nil :cookbook/markdown-table nil]]))
      ))

(deftest findrel-with-nested-many
  (testing "aaa"
    (let [ctx (mock-fiddle! :dustingetz.seattle/communities2)]
      (for [[_ ctx] (context/spread-rows ctx)
            [_ ctx] (context/spread-elements ctx)]
        (let [ctx (context/attribute ctx :community/type)]
          (for [[_ ctx] (context/spread-rows ctx)]
            (context/row ctx (context/row-key ctx {:db/id 42})))))))
  )

(def ctx-ide-domain (mock-fiddle! fixtures.domains/schemas
                                  fixtures.domains/fiddles
                                  :hyperfiddle.ide/domain))

(deftest ide-domain-databases
  (testing "yaaa"
    ; issue could be isComponent, or multiple maps in the pull
    (let [ctx (extract (context/refocus+ ctx-ide-domain :domain/databases))]
      (= (context/eav ctx)
         [[:domain/ident "hyperfiddle"] :domain/databases nil]))))

(deftest txfn
  (testing "counter button at scalar"
    (let [ctx (mock-fiddle! :dustingetz/counter)
          ctx (extract (context/refocus+ ctx :dustingetz.reg/age))]
      (= (context/eav ctx) [17592186046196 :dustingetz.reg/age 102])))

  ; hf/new at fiddle level
  ; How does it pick the color and set the right eav for [:db/add e a v]
  ; v is formula. What is e?
  ; There isn't, this adds parent-child rel whichj we don't have! Why are we here.

  )

(deftest refocusing-complicated
  (testing "from :many with a row, refocus to self"
    (is (= (-> ctx-ide-domain
               (context/attribute :domain/databases)
               (context/row 17592186046511)
               (context/eav))
           (-> ctx-ide-domain
               (context/attribute :domain/databases)
               (context/row 17592186046511)
               (context/refocus+ :domain/databases)
               extract
               (context/eav))
           [[:domain/ident "hyperfiddle"] :domain/databases 17592186046511]
           )))

  (testing "from :many with a row, refocus to self :db/id, and then back up"
    ; The link is not at :db/id, it is at the equivalent parent ref
    (is (= (-> (mock-fiddle! fixtures.domains/schemas fixtures.domains/fiddles :hyperfiddle.ide/domain)
               (context/attribute :domain/databases)
               (context/row 17592186046511)
               (context/attribute :db/id)
               (context/reachable-pullpaths))
           [[:db/id]
            [:domain/ident]
            [:domain/environment]
            [:domain/disable-javascript]
            [:domain/home-route]
            [:domain/code]
            [:domain/css]
            [:domain/databases]
            [:domain/databases :db/id]
            [:domain/databases :domain.database/name]
            [:domain/databases :domain.database/record]
            [:domain/databases :domain.database/record :db/id]
            [:domain/databases :domain.database/record :database/uri]
            #_[:domain/databases :domain.database/record :database/write-security]
            #_[:domain/databases :domain.database/record :database/write-security :db/id]
            [:domain/databases :domain.database/record :database.custom-security/server]
            [:domain/fiddle-database]
            [:domain/fiddle-database :database/uri]]))

    (is (= (-> ctx-ide-domain
               (context/attribute :domain/databases)
               (context/row 17592186046511)
               (context/eav))
           (-> ctx-ide-domain
               (context/attribute :domain/databases)
               (context/row 17592186046511)
               (context/attribute :db/id)
               (context/refocus+ :domain/databases)         ; refocusing too far up by one (due to db/id i think)
               extract
               (context/eav))
           [[:domain/ident "hyperfiddle"] :domain/databases 17592186046511]))
    ))

(deftest reachable-attrs
  (is (= (-> ctx-ide-domain
             (context/attribute :domain/databases)
             (context/row 17592186046511)
             (context/attribute :db/id)
             (context/reachable-attrs))
         [:db/id
          :domain/ident
          :domain/environment
          :domain/disable-javascript
          :domain/home-route
          :domain/code
          :domain/css
          :domain/databases
          :domain.database/name
          :domain.database/record
          :database/uri
          :database.custom-security/server
          :domain/fiddle-database]))

  )

;(deftest options-findrel-1
;  (is (= (-> (mock-fiddle! fixtures.domains/schemas fixtures.domains/fiddles :hyperfiddle.ide/domain)
;             (context/focus [:domain/fiddle-database])
;             (context/v))
;         [:database/uri #uri"datomic:free://datomic:4334/root"]))
;
;  (def ctx (mock-fiddle! fixtures.domains/schemas fixtures.domains/fiddles :database/options-list))
;  (is (= (context/row-key ctx [{:db/id 17592186046713, :database/uri #uri "datomic:free://datomic:4334/mbrainz"} "mbrainz"])
;         [[:database/uri #uri"datomic:free://datomic:4334/mbrainz"] "mbrainz"]))
;
;  (is (= (-> ctx (context/row [[:database/uri #uri"datomic:free://datomic:4334/mbrainz"] "mbrainz"])
;             (context/data))
;         [{:db/id 17592186046713, :database/uri #uri"datomic:free://datomic:4334/mbrainz"} "mbrainz"]))
;
;  (is (-> (mock-fiddle! fixtures.domains/schemas fixtures.domains/fiddles :hyperfiddle.ide/domain)
;          @(hyperfiddle.data/select-many-here :database/options-list)
;          first :link/fiddle :fiddle/ident (= :database/options-list)))
;
;  )

(deftest fiddle-validation                                  ; todo move to hyperfiddle.ide
  (s/def :hyperfiddle/ide (s/keys))
  (s/def :hyperfiddle.ide/new-fiddle (s/keys :req [:fiddle/ident]))
  (s/def :hyperfiddle.ide/new-link (s/keys :req [:link/path]))

  (testing "new fiddle popover validation"
    (let [ctx-tank1 (mock-fiddle! fixtures.tank/schemas fixtures.tank/fiddles :fixtures.tank/new-link1)
          ctx-tank2 (mock-fiddle! fixtures.tank/schemas fixtures.tank/fiddles :fixtures.tank/new-link2)]
      (is (= (context/tree-invalid? ctx-tank1) true))
      (is (= (context/leaf-invalid? (context/attribute ctx-tank1 :link/path)) true))
      (is (= (context/leaf-invalid? (context/attribute ctx-tank1 :db/id)) false))
      (is (= (context/tree-invalid? ctx-tank2) false))
      (is (= (context/leaf-invalid? (context/attribute ctx-tank2 :link/path)) false))
      (is (= (context/leaf-invalid? (context/attribute ctx-tank2 :db/id)) false))))

  (testing "links table validation"
    (let [table-ctx (mock-fiddle! fixtures.tank/schemas fixtures.tank/fiddles :fixtures.tank/test-links-table-validation)]
      (let [a (-> table-ctx
                  (context/attribute :fiddle/links)
                  (context/row 17592186061849)
                  (context/attribute :link/path))
            b (-> table-ctx
                  (context/attribute :fiddle/links)
                  (context/row 17592186061848)
                  (context/attribute :link/path))]
        (is (= (context/eav a) [17592186061849 :link/path :fiddle/links]))
        (is (= (context/leaf-invalid? a) false))
        (is (= (context/eav b) [17592186061848 :link/path :fiddle/ident]))
        (is (= (context/leaf-invalid? b) false))
        (is (= (context/leaf-invalid? (context/attribute b :db/id)) false))))))

#_(deftest deps-satisfied-1
    []
    (is (= (deps-satisfied? [0 :fiddle/links] [0 :fiddle/links :link/fiddle]) false))
    (is (= (deps-satisfied? [0 :gender] [0 :shirt-size]) true))
    (is (= (deps-satisfied? [0] [0 :shirt-size]) true))
    (is (= (deps-satisfied? [0] [0 :fiddle/links :link/fiddle]) false))
    (is (= (deps-satisfied? [0 :fiddle/links] [0 :fiddle/links :link/fiddle]) false))
    (is (= (deps-satisfied? [] [0]) false))
    (is (= (deps-satisfied? [] [0 :user/user-id]) false))   ; This becomes true if a relation can be implied in scope
    (is (= (deps-satisfied? [] [0 :user/user-id]) true))
    (is (= (deps-satisfied? [0 :user/user-id] []) true))
    )

(deftest link-path-floor-1
  []
  ;(is (= (link-path-floor [0 :user/user-id]) [0]))
  ;(is (= (link-path-floor [0 :reg/gender]) [0]))
  ;(is (= (link-path-floor [0 :fiddle/links :link/fiddle]) [0 :fiddle/links]))
  )

;(deftest deps-over-satisfied-1
;  []
;  (is (= (deps-over-satisfied? [0 :fiddle/links] [0 :fiddle/links :link/fiddle]) true))
;  (is (= (deps-over-satisfied? [0 :gender] [0 :shirt-size]) true))
;  (is (= (deps-over-satisfied? [0] [0 :shirt-size]) true))
;  (is (= (deps-over-satisfied? [0] [0 :fiddle/links :link/fiddle]) true))
;  (is (= (deps-over-satisfied? [0 :fiddle/links] [0 :fiddle/links :link/fiddle]) true))
;  (is (= (deps-over-satisfied? [] [0]) true))
;  (is (= (deps-over-satisfied? [0] []) false))
;  (is (= (deps-over-satisfied? [] [0 :user/user-id]) true))
;  (is (= (deps-over-satisfied? [] [0 :user/user-id]) true))
;  (is (= (deps-over-satisfied? [0 :user/user-id] []) false))
;  )

(deftest newtest-1
  []
  ; !field(0) is oversatisfied for genders
  ;(is (= (deps-satisfied? [0] []) true))               ; body count is >=
  ;(is (= (deps-over-satisfied? [0] []) true))          ; left divergence has

  ; !field(0) is satisfied for shirt-sizes
  ;(is (= (deps-satisfied? [0] [0 :reg/gender]) true))  ; body count is =
  ;(is (= (deps-over-satisfied? [0] [0 :reg/gender]) false))
  ;(is (= (deps-over-satisfied? [0 :reg/gender] [0 :reg/gender]) true))
  ; It would be over-satisfied if there was another
  )


; Clojurians
; Is it sortable at
; aggregate
; column
; nested pull column
(deftest sorting-1
  (testing "from :many with a row, refocus to self"

    (let [ctx (mock-fiddle! fixtures.tank/schemas
                            fixtures.tank/fiddles
                            :foo/clojurians)]

      (is (= (-> ctx
                 (context/browse-element 1)
                 (context/attribute :channel/name)
                 (hyperfiddle.ui.sort/sortable?))
             true))

      (is (= (-> ctx
                 (context/browse-element 1)
                 (context/attribute :channel/creator)       ; ref
                 (hyperfiddle.ui.sort/sortable?))
             false))

      (is (= true (-> ctx
                      (context/browse-element 0)
                      (hyperfiddle.ui.sort/sortable?))))

      #_(is (= false (-> ctx
                         (context/browse-element 1)
                         (hyperfiddle.ui.sort/sortable?))))
      )

    (let [ctx (mock-fiddle! fixtures.tank/schemas
                            fixtures.tank/fiddles
                            :foo.clojurians/channel)]

      (is (= (-> ctx
                 (context/attribute :message/_channel)
                 (hyperfiddle.ui.sort/sortable?))
             false))

      (is (= (-> ctx
                 (context/attribute :message/text)
                 (hyperfiddle.ui.sort/sortable?))
             true))

      (is (= (-> ctx
                 (context/attribute :message/user)          ; ref
                 (hyperfiddle.ui.sort/sortable?))
             false))
      )))

(deftest validation-1
  (is (= (context/validate-result (s/coll-of (s/keys :req [:foo/bar]))
           [{:foo/bar 1 :db/id 123}
            {:foo/baz 42 :db/id 124}]
           contrib.datomic/smart-lookup-ref-no-tempids)
        '([[124 :foo/bar] nil])))
  )

(comment
  (s/def :asdf/qwer (s/coll-of (s/keys :req [:community/name])))
  (describe :asdf/qwer {:reason "Field is required"})
  (s/def :community/name
    (s/and :community/name-not-blank :community/name-no-a))
  (s/def :community/name-no-a #(not (clojure.string/includes? % "a")))
  (s/def :community/name-not-blank #(not (clojure.string/blank? %)))
  (describe :community/name-not-blank {:reason "Name can not be blank"})
  (describe :community/name-no-a {:reason "Comunity name can not contain an a"})
  )

(s/def ::email-malformed #(clojure.string/includes? % "@"))
(s/def ::email-blank (complement clojure.string/blank?))
(s/def ::email (s/and string? ::email-blank ::email-malformed))
(s/def ::gender (s/keys :req [:db/ident]))
(s/def ::person (s/keys :req [::email ::gender]))           ; intermediate named spec vs composite spec
(s/def ::index (s/coll-of (s/keys :req [::person])))
(hf/def-validation-message ::gender "gender")
(hf/def-validation-message ::email "email")
(hf/def-validation-message ::email-blank "email blank")
(hf/def-validation-message ::email-malformed "email malformed")

(def test-validation-result
  [{:db/id 11 ::person {::gender {:db/ident ::male}}}      ; missing email
   {:db/id 12 ::person {::gender {:db/ident ::male} ::email ""}} ; blank email
   {:db/id 13 ::person {::gender {:db/ident ::male} ::email "not-an-email"}} ; malformed email
   {:db/id 14 ::person {::gender {:db/ident ::male} ::email "alice@example.com"}} ; valid
   ])

(deftest validation-2
  (is (= (context/validate-result (s/get-spec ::index)
           test-validation-result
           contrib.datomic/smart-lookup-ref-no-tempids)
        '([[11 :hypercrud.browser.context-test/person :hypercrud.browser.context-test/email] "email"]
          [[12 :hypercrud.browser.context-test/person :hypercrud.browser.context-test/email] "email blank"]
          [[13 :hypercrud.browser.context-test/person :hypercrud.browser.context-test/email] "email malformed"])))

  ;(def problem (-> (s/explain-data ::index test-validation-result) ::s/problems vec #_(get 0)))
  ;(context/form-cell-problem problem)

  ;(def problems (::s/problems (s/explain-data ::index test-validation-result)))
  ;(context/form-validation-hints problems)
  ;(map context/form-cell-problem problems)

  )
