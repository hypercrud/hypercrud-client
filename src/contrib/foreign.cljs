(ns contrib.foreign
  "Call foreign JS libraries in a clean, maitainable and safe way"
  (:refer-clojure :exclude [object]))

(def aliases
  "A mapping from a human friendly name to the actual symbol used by the library.
  Optional, just makes maintenance easier."
  {'jquery    '$ ; just an example. You don’t want to track '$
   'fullstory 'FS})

;; -----------------------------------------------------------------------------

(defn- call!* [object method args]
  {:pre [(symbol? object) (symbol? method)]}
  (if-let [obj (aget js/window (name object))]
    (if-let [m (aget obj (name method))]
      (do
        (js/console.info "Foreign call" (seq (into [object method] args)))
        (.apply ^js m obj (apply array args)))
      (throw (ex-info "Method not found on object" {:object object
                                                    :method method})))
    (throw (ex-info "Object not found in js/window scope" {:object object}))))

(defn call! [lib-symbol method-symbol & args]
  (call!* (aliases lib-symbol lib-symbol) method-symbol args))

(defn object [lib-symbol]
  (fn [method-symbol & args]
    (apply call! lib-symbol method-symbol args)))

(defn method [lib-symbol method-symbol]
 (fn [& args]
   (apply call! lib-symbol method-symbol args)))
