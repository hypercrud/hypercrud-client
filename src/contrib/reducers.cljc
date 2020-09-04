(ns contrib.reducers)


(defn combine-reducers [reducer-map]
  (fn [value action & args]
    #_(if action (println "dispatch! " action " " (first args)))
    (reduce (fn [state [k reducer]]
              (update state k #(apply reducer % action args)))
            value
            reducer-map)))

(defn dispatch
  [state root-reducer action]
  (let [[action & args] action]
    (if (= :batch action)
      ; Batched actions are atomic, the incremental states during computation can violate global invariants
      (reduce (fn [state [action & args]]
                (apply root-reducer state action args))
              state
              args)
      (apply root-reducer state action args))))

(defn dispatch! [state-atom root-reducer action]
  (swap! state-atom dispatch root-reducer action)
  nil)
