(ns contrib.reducers)


(defn combine-reducers [reducer-map]
  (fn [value action & args]
    #_(if action (println "dispatch! " action " " (first args)))
    (reduce (fn [state [k reducer]]
              (update state k #(apply reducer % action args)))
            value
            reducer-map)))

(defn dispatch! [state-atom root-reducer action]
  (let [[action & args] action]
    (if (= :batch action)
      (swap! state-atom (fn [state]
                          ; Batched actions are atomic, the incremental states during computation can violate global invariants
                          #_(println "dispatch! " :batch)
                          (reduce (fn [state [action & args]]
                                    (apply root-reducer state action args))
                                  state
                                  args)))
      (swap! state-atom #(apply root-reducer % action args))))
  nil)
