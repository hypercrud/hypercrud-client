(ns contrib.promise
  (:require [promesa.core :as p]))

(defn branch [promise successf errorf]
  (cond-> promise
    successf (p/then successf)
    errorf   (p/catch errorf)))
