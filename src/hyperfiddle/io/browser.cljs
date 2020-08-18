(ns hyperfiddle.io.browser
  (:require
    [goog.object :as object]
    [hypercrud.types.Err :as Err]
    [hyperfiddle.api :as hf]
    [hyperfiddle.io.core :as io]
    [hyperfiddle.io.http-client :as http-client]
    [promesa.core :as p]
    [taoensso.timbre :as timbre]))


(defn handle-401 [domain e]
  (if (= 401 (:status (ex-data e)))
    (do
      (timbre/error e)
      ; just blast the window location to force a refresh
      (object/set js/window "location" (hf/url-encode domain `(hyperfiddle.system/unauthorized))))
    (throw e)))

(deftype IOImpl [domain]
  io/IO
  (global-basis [io]
    (-> (http-client/global-basis! domain nil)
        (p/catch (partial handle-401 domain))))

  (local-basis [io global-basis route]
    (p/resolved (io/local-basis-for io global-basis route)))

  (hydrate-route [io local-basis route pid partitions]
    (-> (http-client/hydrate-route! domain nil local-basis route pid partitions)
        (p/catch (partial handle-401 domain))))

  (hydrate-requests [io local-basis partitions requests]
    (-> (http-client/hydrate-requests! domain nil local-basis partitions requests)
        (p/catch (partial handle-401 domain))))

  (sync [io dbnames]
    (-> (http-client/sync! domain nil dbnames)
        (p/catch (partial handle-401 domain))))

  (transact! [io tx-groups]
    (-> (http-client/transact! domain nil tx-groups)
        (p/catch (fn [e]
                   ; the runtime does not do anything on transact failures yet...
                   (let [message (cond
                                   (string? e) e
                                   (Err/Err? e) (:msg e)
                                   (map? e) (:message e)
                                   :else (ex-message e))]
                     (if (= "Please refresh your browser" message)
                       (js/alert "We deployed some updates and your client is out of date, press OK and we will refresh your page")
                       (js/alert message)))
                   (handle-401 domain e)))))
  ;; This is implemented as a deftype because defrecord brings a lot of extra
  ;; protocols that don't make sense in this case, like assoc.
  IEquiv
  (-equiv [o other]
    (and (instance? IOImpl other) (= (.-domain o) (.-domain other))))

  IHash
  (-hash [this] (hash [domain])))
