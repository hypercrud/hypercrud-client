(ns hyperfiddle.io.client.ws
  (:require [bidi.bidi :as bidi]
            [hypercrud.transit :as hc-t]
            [hyperfiddle.api :as hf]
            [hyperfiddle.io.client.http :as http]
            [hyperfiddle.service.routes :as routes]
            [clojure.core.async :as a :include-macros true]
            [promesa.core :as p]
            [taoensso.timbre :as log]))

(def READY_STATE {0 ::connecting
                  1 ::open
                  2 ::closing
                  3 ::closed})

(defonce ^:private a-socket (atom nil))
(defonce ^:private state (atom {}))
(defonce ^:private connecting (atom nil))

(declare on-message)

(defn- get-token! []
  (http/request! {:method  :get
                  :url     (str (.. js/document -location -origin) (bidi/path-for routes/routes ::hf/ws-exchange-token))
                  :headers {"accept" "text/plain"}}))

(defn- connect! []
  (-> (get-token!)
      (p/then (fn [token]
                (let [prom (p/create (fn [resolve reject]
                                       (try
                                         (let [socket (new js/WebSocket (str "ws://" (.. js/document -location -host) "/ws?token=" token))]
                                           (set! (.-onopen socket) (fn [_event]
                                                                     (reset! connecting false)
                                                                     (resolve socket)))
                                           (set! (.-onmessage socket) on-message)
                                           (set! (.-onerror socket) reject))
                                         (catch js/Error e
                                           (log/error e)
                                           (reject e)))))]
                  (reset! connecting prom)
                  prom)))))

(defn- ready-state [^js socket]
  (get READY_STATE (.-readyState socket)))

(defn- opened? [^js socket]
  (= ::open (ready-state socket)))

(defn- get-client! []
  (p/create (fn [resolve reject]
              (let [connecting @connecting
                    socket     @a-socket
                    next       (fn [socket]
                                 (reset! a-socket socket)
                                 (resolve socket))]
                (cond
                  (and socket (opened? socket)) (resolve socket)
                  connecting                    (p/then connecting next)
                  :else                         (-> (connect!)
                                                    (p/then next)
                                                    (p/catch reject)))))))

(defn- disconnect! []
  (when-let [^js socket @a-socket]
    (when (opened? socket)
      (.close socket))))

(defn- send!* [event model]
  (let [continuation (case model
                       :request-response (p/deferred)
                       :subscription     (a/chan))
        id           (or (:id event) (random-uuid))]
    (-> (get-client!)
        (p/then (fn [^js socket]
                  (swap! state (fn [state]
                                 (if-not (contains? state id)
                                   (assoc state id {:model        model
                                                    :continuation continuation})
                                   state)))
                  (.send socket (hc-t/encode (assoc event :id id)))))
        (p/catch (fn [err]
                   (log/error "Unable to get a socket" err)
                   (case model
                     :request-response (p/resolve! continuation nil)
                     :subscription     (a/close! continuation)))))
    [id continuation]))

(defn send! [event]
  (let [[_ continuation] (send!* event :request-response)]
    continuation))

(defn subscribe! [event]
  (send!* event :subscription))

(defn- on-message [^js message]
  (let [{:keys [id type] :as data} (hc-t/decode (.-data message))]
    (swap! state (fn [state-value]
                   (if-let [continuation (get state-value id)]
                     (let [{:keys [model continuation]} continuation]
                       (case model
                         :request-response (do (case type
                                                 :error (p/reject! continuation data)
                                                 (p/resolve! continuation data))
                                               (dissoc state-value id))
                         :subscription     (do (a/go
                                                 (case type
                                                   :error (do (a/>! continuation data)
                                                              (a/close! continuation)
                                                              (swap! state dissoc id))
                                                   (a/>! continuation data)))
                                               state-value)))
                     (do (log/info "No continuation for this answer" data)
                         state-value))))))

(defn read [s]
  (cljs.reader/read-string s))

(set! (.-edn_read js/window) read)
