(ns hyperfiddle.ide.fiddles.schema-attribute
  (:require
    [clojure.set :as set]
    [contrib.datomic-tx :as tx]
    [contrib.pprint :refer [pprint-datoms-str]]
    [contrib.reactive :as r]
    [contrib.ui :refer [debounced]]
    [hypercrud.browser.context :as context]
    [hyperfiddle.api :as hf]
    [hyperfiddle.runtime :as runtime]
    [hyperfiddle.ui :refer [field markdown]]
    [hyperfiddle.ui.util :refer [entity-change->tx]]))


(def special-attrs #{:db/ident :db/cardinality :db/valueType})

(defn- completed? [entity] (set/subset? special-attrs (set (keys entity))))

(defn- is-ref? [entity] (-> entity :db/valueType :db/ident (= :db.type/ref)))

(defn- merge-in-tx [entity tx ctx]
  ; this fn has bare minimum support for this page e.g. doesnt support card/many or nested modals
  (reduce (fn [entity [op e a v]]
            (case op                                        ; Make it shaped like a pulled-tree
              :db/add (assoc entity a {:db/ident v})        ; these are lookup refs because of the options query
              :db/retract (dissoc entity a)))
          entity
          tx))

(defn valueType-and-cardinality-with-tx! [special-attrs-state {rt :runtime pid :partition-id :as ctx} tx]
  (let [entity @(:hypercrud.browser/result ctx)             ; the whole form, not the data
        dbname (context/dbname ctx)
        schema @(runtime/get-schema+ rt pid dbname)
        new-entity (merge-in-tx entity tx ctx)]
    (case [(completed? entity) (completed? new-entity)]
      [false false]
      (swap! special-attrs-state (partial tx/into-tx schema) tx)

      [false true]
      (do
        (runtime/with-tx rt pid (context/dbname ctx) (tx/into-tx schema @special-attrs-state tx))
        (reset! special-attrs-state nil))

      [true false]
      ; todo this case WILL throw (going from a valid tx to invalid)
      (runtime/with-tx rt pid (context/dbname ctx) tx)

      [true true]
      (runtime/with-tx rt pid (context/dbname ctx) tx))))

(defn ident-with-tx! [special-attrs-state {rt :runtime pid :partition-id :as ctx} tx]
  (let [entity (context/data (context/unwind ctx 1))        ; ctx is focused to :db/ident
        dbname (context/dbname ctx)
        schema @(runtime/get-schema+ rt pid dbname)
        new-entity (merge-in-tx entity tx ctx)]
    (case [(completed? entity) (completed? new-entity)]
      [false false]
      (runtime/with-tx rt pid (context/dbname ctx) tx)

      [false true]
      (do
        (runtime/with-tx rt pid (context/dbname ctx) (tx/into-tx schema @special-attrs-state tx))
        (reset! special-attrs-state nil))

      [true false]
      ; todo this case WILL throw (going from a valid tx to invalid)
      (runtime/with-tx rt pid (context/dbname ctx) tx)

      [true true]
      (runtime/with-tx rt pid (context/dbname ctx) tx))))

(defn renderer [val ctx props]
  (let [special-attrs-state (r/atom nil)
        reactive-merge #(merge-in-tx @% @special-attrs-state ctx)
        ident-f (fn [val ctx props]
                  (let [on-change! (r/comp (r/partial ident-with-tx! special-attrs-state ctx)
                                           (r/partial entity-change->tx ctx))
                        props (assoc props :value (context/data ctx)
                                           :on-change on-change!)]
                    [debounced props contrib.ui/keyword]))
        valueType-and-cardinality-f (fn [val ctx props]
                                      (let [on-change! (r/comp (r/partial valueType-and-cardinality-with-tx! special-attrs-state ctx)
                                                               (r/partial entity-change->tx ctx))]
                                        [hyperfiddle.ui/hyper-control val ctx (assoc props :on-change on-change!)]))]
    (fn [val ctx props]
      ;(println (pr-str (:hypercrud.browser/result ctx)))
      ;(println (pr-str (:hypercrud.browser/result-enclosure ctx)))
      ;(println (pr-str (:hypercrud.browser/schema ctx)))
      ;(println (pr-str (:hypercrud.browser/element ctx)))
      (let [ctx (update ctx :hypercrud.browser/data (partial r/track reactive-merge))
            ctx (-> (update ctx :hypercrud.browser/result (partial r/track reactive-merge))
                    (context/index-result))
            valid-attr? @(r/fmap completed? (:hypercrud.browser/result ctx))
            is-ref? @(r/fmap is-ref? (:hypercrud.browser/result ctx))]
        [:div.container-fluid.-hyperfiddle-ide-schema-editor-attribute props
         [markdown "See [Datomic schema docs](https://docs.datomic.com/on-prem/schema.html)."]
         ; disable the following fields if they ALL have already been filled out
         ; currently, if they are changed after filled out it causes page error
         (field [:db/ident] ctx ident-f {:disabled valid-attr?})
         (field [:db/valueType] ctx valueType-and-cardinality-f {:disabled valid-attr?
                                                                 :options "valueType-options"
                                                                 :option-label (comp name :db/ident)})
         (field [:db/cardinality] ctx valueType-and-cardinality-f {:options "cardinality-options"
                                                                   :option-label (comp name :db/ident)
                                                                   :disabled valid-attr?})

         ; The rule is you can't stage anything until it's a valid Datomic attribute.
         ; So only the special attrs are editable at first.
         ; Once that is completed, the rest are editable.
         [:div {:style {:display (if valid-attr? "block" "none")}}
           (field [:db/doc] ctx nil {:disabled (not valid-attr?)})
           (field [:db/unique] ctx hyperfiddle.ui/hyper-control {:disabled (not valid-attr?)
                                                                 :options "unique-options"})
           [markdown "!block[Careful: below is not validated, don't stage invalid schema]{.alert .alert-warning style=\"margin-bottom: 0\"}"]
           (when-not (or (not valid-attr?) (not is-ref?))
             (field [:db/isComponent] ctx nil))
           (field [:db/fulltext] ctx nil {:disabled (not valid-attr?)})

           [:div.p "Additional attributes"]
           (let [is-whitelist-attr-installed
                 #_(hf/attr ctx :hyperfiddle/whitelist-attribute) ; requires browse-element for ::schema which hasn't happened
                 (context/focus ctx [:hyperfiddle/whitelist-attribute])]
             (if is-whitelist-attr-installed
               (field [:hyperfiddle/whitelist-attribute] ctx nil)
               [:<>
                [:div "missing :hyperfiddle/whitelist-attribute, please transact the following into app db:"]
                [:pre (contrib.pprint/pprint-datoms-str
                        [[:db/add "a" :db/cardinality :db.cardinality/one]
                         [:db/add "a" :db/valueType :db.type/boolean]
                         [:db/add "a" :db/ident :hyperfiddle/whitelist-attribute]
                         [:db/add "a" :hyperfiddle/whitelist-attribute true]])]])) ; this attribute is also whitelisted by default to defeat chicken/egg

           (doall
             (for [[k _] (hf/spread-attributes ctx)
                   :when (not= "db" (namespace k))]
               (field [k] ctx)))]]))))
