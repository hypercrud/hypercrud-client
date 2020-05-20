(ns hyperfiddle.ide.routing
  (:require
    [hypercrud.browser.base :as base]
    [hyperfiddle.route :as route]))


(defn ide-route->preview-route [{[preview-fiddle & preview-datomic-args] ::route/datomic-args :as ide-route}]
  (-> (if (seq preview-datomic-args)
        (assoc ide-route ::route/datomic-args (vec preview-datomic-args))
        (dissoc ide-route ::route/datomic-args))
      (assoc ::route/fiddle (base/legacy-lookup-ref->fiddle-ident preview-fiddle))))

(defn preview-route->ide-route [{:keys [::route/fiddle ::route/datomic-args] :as preview-route}]
  (assoc preview-route
    ::route/fiddle :hyperfiddle.ide/edit ;; TODO: GG: unnest
    ::route/datomic-args (into [(base/legacy-fiddle-ident->lookup-ref fiddle)] datomic-args)))
