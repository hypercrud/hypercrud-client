(ns contrib.printer-worker
  (:require [clojure.pprint]
            [contrib.pprint :as pprint]
            [hyperfiddle.pprint]
            [clojure.string :as str]
            [hypercrud.transit :as transit]))

(defn pprint [o]
  (str/trimr
   (binding [clojure.pprint/*print-right-margin* clojure.pprint/*print-right-margin*]
     (with-out-str (pprint/pprint o)))))

(def encoder (js/TextEncoder.))
(def decoder (js/TextDecoder.))

(defn handle [^js e]
  (let [start (js/self.performance.now)
        in    (->> (.. e -data -buffer)
                   (.decode decoder)
                   (transit/decode))
        ;; TODO this create a new buffer even if we already have one at hand, we
        ;; should reuse allocated memory (reuse buffer?) and just send back a
        ;; pointer. Or we could use shared memory space.
        ;; https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer
        out   (.encode encoder (pprint in))]
    #_(js/console.log #js{:in  in
                          :out (pprint in)})
    (js/postMessage #js{:id     (.. e -data -id)
                        :buffer (.-buffer out)
                        :start  start
                        :end    (js/self.performance.now)}
                    #js[(.-buffer out)])))

(defn init [] (js/self.addEventListener "message" handle))
