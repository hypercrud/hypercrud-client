(ns hyperfiddle.transaction-test
  (:require
    [contrib.data :as data]
    [contrib.datomic :refer [indexed-schema]]
    [hyperfiddle.transaction :refer [into-tx mappify-add-statements flatten-tx construct remove-tx
                                absorb #?(:clj expand-hf-tx) find-datom identifier->e flatten-ref-stmt
                                deconstruct-ideal flatten-map-stmt absorb-stmt ideal-idx remove-dangling-ids
                                ideals->tx invalid? filter-tx unified-identifier mappify identifier deconstruct stmt->identifier]]
    [clojure.set :as set]
    [clojure.test :refer [deftest is testing]]))

(def schema
  (->> [{:db/ident :foo
         :db/valueType :db.type/string
         :db/cardinality :db.cardinality/one}

        {:db/ident :bar
         :db/valueType :db.type/string
         :db/cardinality :db.cardinality/one}

        {:db/ident :ref
         :db/valueType :db.type/ref
         :db/cardinality :db.cardinality/one}

        {:db/ident :component
         :db/valueType :db.type/ref
         :db/cardinality :db.cardinality/one
         :db/isComponent true}]
       (data/group-by-unique :db/ident)))

(def seattle-schema-tx
  [{:db/ident :community/name, :db/valueType :db.type/string, :db/cardinality :db.cardinality/one, :db/fulltext true, :db/doc "A community's name"}
   {:db/ident :community/url, :db/valueType :db.type/string, :db/cardinality :db.cardinality/one, :db/doc "A community's url"}
   {:db/ident :community/neighborhood, :db/valueType :db.type/ref, :db/cardinality :db.cardinality/one, :db/doc "A community's neighborhood"}
   {:db/ident :community/category, :db/valueType :db.type/string, :db/cardinality :db.cardinality/many, :db/fulltext true, :db/doc "All community categories"}
   {:db/ident :community/orgtype, :db/valueType :db.type/ref, :db/cardinality :db.cardinality/one, :db/doc "A community orgtype enum value"}
   {:db/ident :community/type, :db/valueType :db.type/ref, :db/cardinality :db.cardinality/many, :db/doc "Community type enum values"}
   {:db/ident :community/board :db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/doc "People who sit on the community board"}
   {:db/ident :person/name :db/valueType :db.type/string :db/cardinality :db.cardinality/one :db/doc "A person's full name"}
   {:db/ident :neighborhood/name, :db/valueType :db.type/string, :db/cardinality :db.cardinality/one, :db/unique :db.unique/identity, :db/doc "A unique neighborhood name (upsertable)"}
   {:db/ident :neighborhood/district, :db/valueType :db.type/ref, :db/cardinality :db.cardinality/one, :db/doc "A neighborhood's district"}
   {:db/ident :district/name, :db/valueType :db.type/string, :db/cardinality :db.cardinality/one, :db/unique :db.unique/identity, :db/doc "A unique district name (upsertable)"}
   {:db/ident :district/region, :db/valueType :db.type/ref, :db/cardinality :db.cardinality/one, :db/doc "A district region enum value"}])

(def seattle-schema (indexed-schema seattle-schema-tx))

(deftest no-op
  (= (into-tx seattle-schema [] []) []))

(deftest add-one
  (= (into-tx seattle-schema []
              [[:db/add 1 :district/name "Southwest"]
               [:db/add 1 :district/region 2]])
    [[:db/add 1 :district/name "Southwest"]
     [:db/add 1 :district/region 2]]))


(deftest add-one-override-prior-matching-attr
  (= (into-tx seattle-schema []
              [[:db/add 1 :district/region 2]
               [:db/add 1 :district/name "Southwest"]
               [:db/retract 1 :district/name "Southwest"]
               [:db/add 1 :district/name ""]])
    [[:db/add 1 :district/region 2]
     [:db/add 1 :district/name ""]]))


(deftest retract-one-cancel-matching-add
  (= (into-tx seattle-schema []
              [[:db/add 1 :district/name "Southwest"]
               [:db/add 1 :district/region 2]
               [:db/retract 1 :district/region 2]])
    [[:db/add 1 :district/name "Southwest"]])
  (= (into-tx seattle-schema []
              [[:db/add 1 :district/name "Southwest"]
               [:db/retract 1 :district/name "Southwest"]])
    [])
  (= (into-tx seattle-schema []
              [[:db/retract 1 :district/name "Southwest"]
               [:db/add 1 :district/name "Southwest"]])
    []))

(deftest retract-one-remove-when-not-exists-preserve-retract []
  (= (into-tx seattle-schema []
              [[:db/retract 1 :district/region 2]])
    [[:db/retract 1 :district/region 2]]))

(deftest add-many-add-to-set []
  (= (into-tx seattle-schema []
              [[:db/add 1 :community/type 20]
               [:db/add 1 :community/type 21]])
    [[:db/add 1 :community/type 20]
     [:db/add 1 :community/type 21]]))

(deftest retract-many-cancel-matching-add []
  (= (into-tx seattle-schema []
              [[:db/add 1 :community/type 20]
               [:db/add 1 :community/type 21]
               [:db/retract 1 :community/type 21]])
    [[:db/add 1 :community/type 20]])
  (= (into-tx seattle-schema []
              [[:db/add 1 :community/type 20]
               [:db/retract 1 :community/type 20]])
    [])
  (= (into-tx seattle-schema []
              [[:db/retract 1 :community/type 20]
               [:db/add 1 :community/type 20]])
    []))

(deftest retract-many-empty-entity-preserve-retract) []
  (= (into-tx seattle-schema []
              [[:db/retract 1 :community/type 20]])
    [[:db/retract 1 :community/type 20]])

(deftest add-many-cancel-matching-retract []
  (= (into-tx seattle-schema []
              [[:db/add 1 :community/type 20]
               [:db/retract 1 :community/type 21]
               [:db/add 1 :community/type 21]])
    [[:db/add 1 :community/type 20]]))

(deftest longer-test-one []
  (= (into-tx seattle-schema []
              [[:db/add 1 :district/region 2]
               [:db/add 1 :district/name "Southwest"]
               [:db/add 2 :community/name "Asdf"]
               [:db/add 2 :community/url "asdf.com"]
               [:db/retract 1 :district/name "Southwest"]
               [:db/add 1 :district/name ""]])
    [[:db/add 1 :district/region 2]
     [:db/add 2 :community/name "Asdf"]
     [:db/add 2 :community/url "asdf.com"]
     [:db/add 1 :district/name ""]]))

(deftest longer-test-many []
  (= (into-tx seattle-schema []
              [[:db/add 1 :community/type 2]
               [:db/add 1 :community/type 2]])
    [[:db/add 1 :community/type 2]])
  (= (into-tx seattle-schema []
              [[:db/add 1 :community/type 2]
               [:db/add 1 :community/type 2]
               [:db/retract 1 :community/type 2]])
    [])
  (= (into-tx seattle-schema []
              [[:db/retract 1 :community/type 2]
               [:db/retract 1 :community/type 2]])
    [[:db/retract 1 :community/type 2]])
  (= (into-tx seattle-schema []
              [[:db/retract 1 :community/type 2]
               [:db/retract 1 :community/type 2]
               [:db/add 1 :community/type 2]])
    []))

(deftest retract-entity []
  (testing "all tempids"
    (= (into-tx seattle-schema
                [[:db/add "-1" :foo "asdf"]
                 [:db/add "-1" :foo "bar"]]
                [[:db/retractEntity "-1"]])
      [])

    (testing "remove parent entity"
      (= (into-tx seattle-schema
                  [[:db/add "-1" :foo "asdf"]
                   [:db/add "-1" :ref "-2"]
                   [:db/add "-2" :bar "asdf"]]
                  [[:db/retractEntity "-1"]])
        [[:db/add "-2" :bar "asdf"]]))

    (testing "remove parent component entity"
      (= (into-tx seattle-schema
                  [[:db/add "-1" :foo "asdf"]
                   [:db/add "-1" :component "-2"]
                   [:db/add "-2" :bar "asdf"]]
                  [[:db/retractEntity "-1"]])
        []))

    (testing "remove child entity"
      (= (into-tx seattle-schema
                  [[:db/add "-1" :ref "-2"]
                   [:db/add "-2" :bar "asdf"]]
                  [[:db/retractEntity "-2"]])
        [])

      (= (into-tx seattle-schema
                  [[:db/add "-1" :foo "asdf"]
                   [:db/add "-1" :ref "-2"]
                   [:db/add "-2" :bar "asdf"]]
                  [[:db/retractEntity "-2"]])
        [[:db/add "-1" :foo "asdf"]]))

    (testing "remove child component entity"
      (= (into-tx seattle-schema
                  [[:db/add "-1" :component "-2"]
                   [:db/add "-2" :bar "asdf"]]
                  [[:db/retractEntity "-2"]])
        [])

      (= (into-tx seattle-schema
                  [[:db/add "-1" :foo "asdf"]
                   [:db/add "-1" :component "-2"]
                   [:db/add "-2" :bar "asdf"]]
                  [[:db/retractEntity "-2"]])
        [[:db/add "-1" :foo "asdf"]])))

  (testing "mixed ids"
    (= (into-tx seattle-schema
                [[:db/add 1 :foo "asdf"]
                 [:db/add 1 :foo "bar"]]
                [[:db/retractEntity 1]])
      [[:db/retractEntity 1]])

    (= (into-tx seattle-schema []
                [[:db/add 1 :community/name "asdf"]
                 [:db/add [:community/name "asdf"] :a 1]
                 [:db/retract 1 :b 1]
                 [:db/retract [:community/name "asdf"] :c 1]
                 [:db/cas 1 :x 1 2]
                 [:db/cas [:community/name "asdf"] :y 2 1]])
       [{:db/id 1 :community/name "asdf"}
        [:db/retract 1 :b 1]
        [:db/retract 1 :c 1]
        [:db/cas 1 :x 1 2]
        [:db/cas 1 :y 2 1]])

    (= (into-tx seattle-schema []
                [[:db/add 1 :community/name "asdf"]
                 [:db/add [:community/name "asdf"] :a 1]
                 [:db/retractEntity [:community/name "asdf"]]])
       [[:db/retractEntity 1]])

    (= (into-tx seattle-schema []
                [[:db/add 1 :community/name "asdf"]
                 [:db/add [:community/name "asdf"] :a 1]
                 [:db/retractEntity 1]])
       [[:db/retractEntity 1]])

    (testing "remove parent entity"
      (= (into-tx seattle-schema
                  [[:db/add 1 :foo "asdf"]
                   [:db/add 1 :ref "-2"]
                   [:db/add "-2" :bar "asdf"]]
                  [[:db/retractEntity 1]])
        [[:db/retractEntity 1]
         [:db/add "-2" :bar "asdf"]])

      (= (into-tx seattle-schema
                  [[:db/add "-1" :foo "asdf"]
                   [:db/add "-1" :ref 2]
                   [:db/add 2 :bar "asdf"]]
                  [[:db/retractEntity "-1"]])
        [[:db/add 2 :bar "asdf"]]))

    (testing "remove parent component entity"
      (= (into-tx seattle-schema
                  [[:db/add 1 :foo "asdf"]
                   [:db/add 1 :component "-2"]
                   [:db/add "-2" :bar "asdf"]]
                  [[:db/retractEntity 1]])
        [[:db/retractEntity 1]])

      (= (into-tx seattle-schema
                  [[:db/add "-1" :foo "asdf"]
                   [:db/add "-1" :component 2]
                   [:db/add 2 :bar "asdf"]]
                  [[:db/retractEntity "-1"]])
        ; entity 2 already exists, do not touch it
        [[:db/add 2 :bar "asdf"]]))

    (testing "remove child entity"
      (= (into-tx seattle-schema
                  [[:db/add 1 :ref "-2"]
                   [:db/add "-2" :bar "asdf"]]
                  [[:db/retractEntity "-2"]])
        [])

      (= (into-tx seattle-schema
                  [[:db/add "-1" :ref 2]
                   [:db/add 2 :bar "asdf"]]
                  [[:db/retractEntity 2]])
        [[:db/retractEntity 2]])

      (= (into-tx seattle-schema
                  [[:db/add 1 :foo "asdf"]
                   [:db/add 1 :ref "-2"]
                   [:db/add "-2" :bar "asdf"]]
                  [[:db/retractEntity "-2"]])
        [[:db/add 1 :foo "asdf"]])

      (= (into-tx seattle-schema
                  [[:db/add "-1" :foo "asdf"]
                   [:db/add "-1" :ref 2]
                   [:db/add 2 :bar "asdf"]]
                  [[:db/retractEntity 2]])
        [[:db/add "-1" :foo "asdf"]
         [:db/retractEntity 2]]))

    (testing "remove child component entity"
      (= (into-tx seattle-schema
                  [[:db/add 1 :component "-2"]
                   [:db/add "-2" :bar "asdf"]]
                  [[:db/retractEntity "-2"]])
        [])

      (= (into-tx seattle-schema
                  [[:db/add "-1" :component 2]
                   [:db/add 2 :bar "asdf"]]
                  [[:db/retractEntity 2]])
        ; entity 2 already exists, do not touch it
        [[:db/retractEntity 2]])

      (= (into-tx seattle-schema
                  [[:db/add 1 :foo "asdf"]
                   [:db/add 1 :component "-2"]
                   [:db/add "-2" :bar "asdf"]]
                  [[:db/retractEntity "-2"]])
        [[:db/add 1 :foo "asdf"]])

      (= (into-tx seattle-schema
                  [[:db/add "-1" :foo "asdf"]
                   [:db/add "-1" :component 2]
                   [:db/add 2 :bar "asdf"]]
                  [[:db/retractEntity 2]])
        [[:db/add "-1" :foo "asdf"]
         [:db/retractEntity 2]]))

    (testing "orphaned statements"
      (= (into-tx seattle-schema
                  [[:db/add "-1" :ref "-2"]
                   [:db/add "-2" :component "-3"]
                   [:db/add "-3" :foo "asdf"]]
                  [[:db/retractEntity "-3"]])
        [])

      (= (into-tx seattle-schema
                  [[:db/add "-1" :ref "-2"]
                   [:db/add "-2" :ref "-3"]
                   [:db/add "-3" :ref "-4"]
                   [:db/add "-4" :foo "asdf"]]
                  [[:db/retractEntity "-4"]])
        [])

      (= (into-tx seattle-schema
                  [[:db/add "-1" :ref "-2"]
                   [:db/add "-2" :ref "-3"]
                   [:db/add "-3" :ref 4]
                   [:db/add 4 :foo "asdf"]]
                  [[:db/retractEntity 4]])
        [[:db/retractEntity 4]]))

    (testing "Components"
      (is (= [{:db/id "0" :person/name "Frodo Baggins"}
              {:db/id "1" :person/name "Paragrin Took"}
              {:db/id "2" :person/name "Tom Bombadill"}
              {:db/id 1234 :community/board #{"0" "1" "2"}}]
             (into-tx seattle-schema []
                      [[:db/add "0" :person/name "Frodo Baggins"]
                       [:db/add "1" :person/name "Paragrin Took"]
                       [:db/add "2" :person/name "Tom Bombadill"]
                       [:db/add 1234 :community/board "0"]
                       [:db/add 1234 :community/board "1"]
                       [:db/add 1234 :community/board "2"]]))))

    (is (= [{:db/id "0" :a 1 :b 2}]
           (into-tx seattle-schema []
                              [[:db/add "0" :a 1]
                               [:db/add "0" :b 2]])))

    (is (= []
           (into-tx seattle-schema []
                              [[:db/add "0" :a 1]
                               [:db/retract "0" :a 1]])))

    (is (= [{:db/id 2 :neighborhood/name "qwer" :x 1}]
           (into-tx seattle-schema []
                           [[:db/add [:neighborhood/name "qwer"] :x 1]
                            [:db/add 2 :neighborhood/name "qwer"]])))))


(def datomic-schema
  (indexed-schema
    [{:db/ident :db/ident :db/valueType :db.type/ref :db/cardinality :db.cardinality/one :db/unique :db.unique/identity}
     {:db/ident :db/cardinality :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
     {:db/ident :db/unique :db/valueType :db.type/ref :db/cardinality :db.cardinality/one}
     {:db/ident :db/valueType :db.type/ref :db/cardinality :db/cardinality :db.cardinality/one}
     {:db/ident :db/fulltext :db/valueType :db.type/boolean :db/cardinality :db.cardinality/one}
     {:db/ident :db/doc :db/valueType :db.type/string :db/cardinality :db.cardinality/one}]))

(deftest normalize-tx-1
  (let []
    (is (= (flatten-tx datomic-schema seattle-schema-tx)
           [[:db/add "-700968933" :db/ident :community/name]
            [:db/add "-700968933" :db/valueType :db.type/string]
            [:db/add "-700968933" :db/cardinality :db.cardinality/one]
            [:db/add "-700968933" :db/fulltext true]
            [:db/add "-700968933" :db/doc "A community's name"]
            [:db/add "43126449" :db/ident :community/url]
            [:db/add "43126449" :db/valueType :db.type/string]
            [:db/add "43126449" :db/cardinality :db.cardinality/one]
            [:db/add "43126449" :db/doc "A community's url"]
            [:db/add "-1305932792" :db/ident :community/neighborhood]
            [:db/add "-1305932792" :db/valueType :db.type/ref]
            [:db/add "-1305932792" :db/cardinality :db.cardinality/one]
            [:db/add "-1305932792" :db/doc "A community's neighborhood"]
            [:db/add "1766286392" :db/ident :community/category]
            [:db/add "1766286392" :db/valueType :db.type/string]
            [:db/add "1766286392" :db/cardinality :db.cardinality/many]
            [:db/add "1766286392" :db/fulltext true]
            [:db/add "1766286392" :db/doc "All community categories"]
            [:db/add "1563115599" :db/ident :community/orgtype]
            [:db/add "1563115599" :db/valueType :db.type/ref]
            [:db/add "1563115599" :db/cardinality :db.cardinality/one]
            [:db/add "1563115599" :db/doc "A community orgtype enum value"]
            [:db/add "1352154034" :db/ident :community/type]
            [:db/add "1352154034" :db/valueType :db.type/ref]
            [:db/add "1352154034" :db/cardinality :db.cardinality/many]
            [:db/add "1352154034" :db/doc "Community type enum values"]
            [:db/add "-358122211" :db/ident :community/board]
            [:db/add "-358122211" :db/valueType :db.type/ref]
            [:db/add "-358122211" :db/cardinality :db.cardinality/many]
            [:db/add "-358122211" :db/doc "People who sit on the community board"]
            [:db/add "-1599439536" :db/ident :person/name]
            [:db/add "-1599439536" :db/valueType :db.type/string]
            [:db/add "-1599439536" :db/cardinality :db.cardinality/one]
            [:db/add "-1599439536" :db/doc "A person's full name"]
            [:db/add "1955030477" :db/ident :neighborhood/name]
            [:db/add "1955030477" :db/valueType :db.type/string]
            [:db/add "1955030477" :db/cardinality :db.cardinality/one]
            [:db/add "1955030477" :db/unique :db.unique/identity]
            [:db/add "1955030477" :db/doc "A unique neighborhood name (upsertable)"]
            [:db/add "1168461573" :db/ident :neighborhood/district]
            [:db/add "1168461573" :db/valueType :db.type/ref]
            [:db/add "1168461573" :db/cardinality :db.cardinality/one]
            [:db/add "1168461573" :db/doc "A neighborhood's district"]
            [:db/add "-37942678" :db/ident :district/name]
            [:db/add "-37942678" :db/valueType :db.type/string]
            [:db/add "-37942678" :db/cardinality :db.cardinality/one]
            [:db/add "-37942678" :db/unique :db.unique/identity]
            [:db/add "-37942678" :db/doc "A unique district name (upsertable)"]
            [:db/add "492450710" :db/ident :district/region]
            [:db/add "492450710" :db/valueType :db.type/ref]
            [:db/add "492450710" :db/cardinality :db.cardinality/one]
            [:db/add "492450710" :db/doc "A district region enum value"]]))))


(def simple-schema
  (contrib.datomic/indexed-schema
    [{:db/ident :person/name, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one} :db/unique :db.unique/identity}
     {:db/ident :person/liked-tags, :db/valueType {:db/ident :db.type/keyword}, :db/cardinality {:db/ident :db.cardinality/many}}
     {:db/ident :employee/manager, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
     {:db/ident :person/siblings, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}}
     {:db/ident :person/address, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}, :db/isComponent true}
     {:db/ident :person/summerHomes, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}, :db/isComponent true}
     {:db/ident :address/zip, :db/valueType {:db/ident :db.type/string}, :db/cardinality {:db/ident :db.cardinality/one}}
     {:db/ident :person/age, :db/valueType {:db/ident :db.type/long}, :db/cardinality {:db/ident :db.cardinality/one}}
     {:db/ident :person/bestFriend, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/one}}
     {:db/ident :person/friends, :db/valueType {:db/ident :db.type/ref}, :db/cardinality {:db/ident :db.cardinality/many}}]))

(deftest flatten-tx|1
  (is (= (flatten-tx simple-schema [[:db/add "a" :person/name "Alice"]
                                    {:person/name "Bob"
                                     :person/siblings [{:person/name "Cindy"}
                                                       {:person/name "David"}]}])
         [[:db/add "a" :person/name "Alice"]
          [:db/add "776434203" :person/name "Bob"]
          [:db/add "-279635706" :person/name "Cindy"]
          [:db/add "776434203" :person/siblings "-279635706"]
          [:db/add "278413082" :person/name "David"]
          [:db/add "776434203" :person/siblings "278413082"]])))



(def map-form-stmt
  {:person/name "Bob"                                       ; scalar one
   :person/address {:address/zip "12345"}                   ; ref one component
   :person/summerHomes [{:address/zip "11111"}              ; ref many component
                        {:address/zip "22222"}]
   :person/liked-tags [:movies :ice-cream :clojure]         ; scalar many
   :employee/manager {:person/name "Earnest"}               ; ref one
   :person/siblings [{:person/name "Cindy"}                 ; ref many
                     {:person/name "David"}]
   :person/bestFriend "Benjamin"
   :person/friends ["Harry", "Yennefer"]})

(def diverse-tx
  [map-form-stmt
   {:person/name "Frank"}
   [:db/add "g" :person/name "Geralt"]
   [:db/cas 1 :person/age 41 42]
   [:user.fn/foo 'x 'y 'z 'q 'r]])

(deftest flatten-map-stmt|simple
  (is (= (flatten-map-stmt
           simple-schema
           map-form-stmt)
         [[:db/add "2141158636" :person/name "Bob"]
          [:db/add "-545257583" :address/zip "12345"]
          [:db/add "2141158636" :person/address "-545257583"]
          [:db/add "-39529585" :address/zip "11111"]
          [:db/add "2141158636" :person/summerHomes "-39529585"]
          [:db/add "-1618477697" :address/zip "22222"]
          [:db/add "2141158636" :person/summerHomes "-1618477697"]
          [:db/add "2141158636" :person/liked-tags :movies]
          [:db/add "2141158636" :person/liked-tags :ice-cream]
          [:db/add "2141158636" :person/liked-tags :clojure]
          [:db/add "-2113069627" :person/name "Earnest"]
          [:db/add "2141158636" :employee/manager "-2113069627"]
          [:db/add "-279635706" :person/name "Cindy"]
          [:db/add "2141158636" :person/siblings "-279635706"]
          [:db/add "278413082" :person/name "David"]
          [:db/add "2141158636" :person/siblings "278413082"]
          [:db/add "2141158636" :person/bestFriend "Benjamin"]
          [:db/add "2141158636" :person/friends "Harry"]
          [:db/add "2141158636" :person/friends "Yennefer"]])))


(deftest flatten-map-stmp|invalid-nested-map
  (is (thrown? #?(:clj RuntimeException :cljs js/Error) (flatten-map-stmt simple-schema {:employee/manager {:person/address {:address/zip "1234"}}}))))

(deftest flatten-tx'
  (is (= (flatten-tx
           simple-schema
           diverse-tx)
         [[:db/add "2141158636" :person/name "Bob"]
          [:db/add "-545257583" :address/zip "12345"]
          [:db/add "2141158636" :person/address "-545257583"]
          [:db/add "-39529585" :address/zip "11111"]
          [:db/add "2141158636" :person/summerHomes "-39529585"]
          [:db/add "-1618477697" :address/zip "22222"]
          [:db/add "2141158636" :person/summerHomes "-1618477697"]
          [:db/add "2141158636" :person/liked-tags :movies]
          [:db/add "2141158636" :person/liked-tags :ice-cream]
          [:db/add "2141158636" :person/liked-tags :clojure]
          [:db/add "-2113069627" :person/name "Earnest"]
          [:db/add "2141158636" :employee/manager "-2113069627"]
          [:db/add "-279635706" :person/name "Cindy"]
          [:db/add "2141158636" :person/siblings "-279635706"]
          [:db/add "278413082" :person/name "David"]
          [:db/add "2141158636" :person/siblings "278413082"]
          [:db/add "2141158636" :person/bestFriend "Benjamin"]
          [:db/add "2141158636" :person/friends "Harry"]
          [:db/add "2141158636" :person/friends "Yennefer"]
          [:db/add "974316117" :person/name "Frank"]
          [:db/add "g" :person/name "Geralt"]
          [:db/cas 1 :person/age 41 42]
          [:user.fn/foo 'x 'y 'z 'q 'r]])))



(deftest filter-tx'
  (is (= (filter-tx simple-schema (constantly true) diverse-tx)
        (flatten-tx simple-schema diverse-tx)))
  (is (= (filter-tx simple-schema (constantly false) diverse-tx)
         []))
  (is (= (filter-tx
           simple-schema
           (fn [[o :as stmt]]
             (nil? (#{:db/add} o)))
           diverse-tx)
         [[:db/cas 1 :person/age 41 42]
          [:user.fn/foo 'x 'y 'z 'q 'r]])))


(defn f
  [x y z]
  [[:db/add x y z]])

#?(:clj
   (deftest test|expand-hf-tx
     (is (= [[:db/add 1 2 3]] (expand-hf-tx '[[hyperfiddle.transaction-test/f 1 2 3]])))
     (is (= [[:db/add 1 2 3] [:db/add "asdf" "qwer" :zxcv]]
            (expand-hf-tx '[[hyperfiddle.transaction-test/f 1 2 3] [:db/add "asdf" "qwer" :zxcv]])))))

(deftest test|identifier
  (let [schema seattle-schema]
    (is (= (stmt->identifier schema [:db/add "tempid" :community/name "community"])
           {:tempid "tempid"}))
    (is (= (stmt->identifier schema [:db/add 1234 :community/name "community"])
           {:db/id 1234}))
    (is (= (stmt->identifier schema [:db/add "tempid" :neighborhood/name "name"])
           {:tempid "tempid" :neighborhood/name "name"}))
    (is (= (stmt->identifier schema [:db/add 1234 :neighborhood/name "name"])
           {:db/id 1234 :neighborhood/name "name"}))))

(deftest test|unified-identifier
  (is (= (unified-identifier {:x 1 :y 2 :z 3} {:x 1 :z 3})
         {:x 1 :z 3}))
  (is (= (unified-identifier {:x 1 :y 2} {:z 3 :w 4})
         nil))
  (is (= (unified-identifier {:x 1 :y 2 :z 3} {:x 1 :y 4 :z 5})
         {:x 1})))

(deftest test|absorb
  (let [schema seattle-schema]
    (is (= (absorb schema {:db/id 1} #{} {:neighborhood/name "name" :neighborhood/district [:distrinct/name "somename"]})
           [{:db/id 1 :neighborhood/name "name"} #{[:db/add 1 :neighborhood/name "name"] [:db/add 1 :neighborhood/district [:distrinct/name "somename"]]}]))
    (is (= (absorb schema {:db/id 1} #{[:db/add 1 :community/name "name"]} [:db/add 1 :community/name "qwer"])
           [{:db/id 1} #{[:db/add 1 :community/name "qwer"]}]))
    (is (= (absorb schema {:db/id 1} #{[:db/add 1 :community/name "name"]} [:db/retract 1 :community/name "name"])
           [{:db/id 1} #{}]))
    (is (= (absorb schema {:db/id 1} #{} [:db/cas "e" :community/name "asdf" "qwer"])
           [{:db/id 1 :tempid "e"} #{[:db/cas "e" :community/name "asdf" "qwer"]}]))
    (is (= (absorb schema {:db/id 1} #{} [:db/retractEntity 1])
           [{:db/id 1} #{[:db/retractEntity 1]}]))
    (let [some-fn (fn [] '...)]
      (is (= (absorb schema {:db/id 1} #{} [some-fn "arg0" 'arg1 :arg2])
             [{:db/id 1} #{[some-fn "arg0" 'arg1 :arg2]}])))))

(deftest test|identifier->e
  (let [schema seattle-schema]
    (is (= 1
           (identifier->e schema {:db/id 1 :tempid "asdf" :x/y "z" :neighborhood/name "qwer"})))
    (is (= "asdf"
           (identifier->e schema {:tempid "asdf" :x/y "z" :neighborhood/name "qwer"})))
    (is (= [:neighborhood/name "qwer"]
           (identifier->e schema {:x/y "z" :neighborhood/name "qwer"})))))

(deftest test|invalid?
  (let [schema seattle-schema]
    (is (invalid? schema nil))
    (is (invalid? schema {:singlekey :map}))
    (is (invalid? schema [:some/tx "some-id" :db/id "some-value"]))
    (is (invalid? schema [:some/tx [:a :v] :a :v]))))

(deftest test|remove-dangling-ids
  (let [schema seattle-schema]
    (is (= (remove-dangling-ids schema
                                [[:db/add 1234 :community/neighborhood "dangling id"]
                                 [:db/add 1234 :some/attr :some/value]])
           [[:db/add 1234 :some/attr :some/value]]))
    (is (= (remove-dangling-ids schema
                                [[:db/add 1234 :community/neighborhood "not dangling id"]
                                 [:db/add "not dangling id" :some/attr "some-value"]])
           [[:db/add 1234 :community/neighborhood "not dangling id"]
            [:db/add "not dangling id" :some/attr "some-value"]]))))

(deftest test|deconstruct
  (let [schema seattle-schema]
    (is (= (set (deconstruct schema [[{:db/id 1} #{[:db/add 1 :attr0 1] [:db/retract 1 :attr1 2] [:db/cas 1 :attr2 "v0" "v1"] [:db/some-custom-fn "arg0" 'arg1 "arge2"]}]]))
           #{[:db/add 1 :attr0 1]
             [:db/retract 1 :attr1 2]
             [:db/cas 1 :attr2 "v0" "v1"]
             [:db/some-custom-fn "arg0" 'arg1 "arge2"]}))
    (is (= (set (deconstruct schema [[{:db/id 1} #{[:db/retractEntity 1]}]]))
           #{[:db/retractEntity 1]}))))

(deftest test|mappify
  (let [schema seattle-schema]
    (is (= [{:db/id "0" :a 1 :b 2}] (mappify schema [[:db/add "0" :a 1] [:db/add "0" :b 2]])))
    (is (= [{:db/id 0 :a 1} {:db/id 1 :b 2}] (mappify schema [[:db/add 0 :a 1] [:db/add 1 :b 2]])))))
