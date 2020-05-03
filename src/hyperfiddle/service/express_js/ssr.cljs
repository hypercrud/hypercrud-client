(ns hyperfiddle.service.express-js.ssr
  (:require
    [goog.object :as object]
    [hyperfiddle.service.express-js.middleware :as middleware]
    [hyperfiddle.service.http :refer [handle-route]]
    [hyperfiddle.service.node.ssr :as node-ssr]
    [hyperfiddle.service.ssr :as ssr]
    [promesa.core :as p]
    [reagent.dom.server :as reagent-server]
    [taoensso.timbre :as timbre]))


(defmethod handle-route :ssr [handler config req res]
  (let [domain (object/get req "domain")
        io (node-ssr/->IOImpl domain (middleware/service-uri config req) (object/get req "jwt"))
        route (domain/url-decode domain (.-originalUrl req))
        user-id (object/get req "user-id")]
    (-> (ssr/bootstrap-html-cmp config domain io route user-id)
        (p/then (fn [{:keys [http-status-code component]}]
                  (doto res
                    (.status http-status-code)
                    (.format #js {"text/html" #(.send res (str "<!DOCTYPE html>\n" (reagent-server/render-to-string component)))}))))
        (p/catch (fn [e]
                   (timbre/error e)
                   (doto res
                     (.status (or (:hyperfiddle.io/http-status-code (ex-data e)) 500))
                     (.format #js {"text/html" #(.send res (str "<h2>Fatal error:</h2><h4>" (ex-message e) "</h4>"))})))))))
