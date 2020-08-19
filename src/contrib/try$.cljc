(ns contrib.try$
  #?(:cljs (:require-macros [contrib.try$]))
  (:require
    [cats.monad.either :as either]
    [promesa.core :as p]))

(defn- catches
  "Return all catch forms of a try form, if any"
  [try-body]
  (filter (fn [form]
            (and (seqable? form)
                 (= 'catch (first form))))
          try-body))

(defn default-catch [conts]
  ;; only one default continuation allowed
  (first (filter #(= :default (second %)) conts)))

(defmacro try*
  "Like `clojure.core/try`, but (catch :default err …) works for both clj and
  cljs."
  [& body]
  (let [cljs? (some? (:ns &env))] ;; best way to detect CLJS?
    (cons 'try
          (if cljs?
            body ;; cljs already supports this feature
            (if-let [default (default-catch (catches body))]
              (let [[_catch _default & catch-body] default]
                (replace {default (concat '(catch Exception) catch-body)} body))
              body)))))


#?(:clj
   (defmacro try-catch-non-fatal [& args]
     (let [[try-body [e catch-body]] (split-at (- (count args) 2) args)]
       `(try
          ~@try-body
          ~(if (:ns &env)
             `(catch js/Error ~e ~catch-body)
             `(catch Throwable ~e
                (if (#{VirtualMachineError
                       ThreadDeath
                       InterruptedException
                       LinkageError} (type ~e))
                  (throw ~e)
                  ~catch-body)))))))

#?(:clj
   (defmacro try-either
     "Try to evalute the body and return the result as an either.
     If a non-fatal throwable is thrown return the exception as a left,
     otherwise return the result as a right."
     [& body]
     (let [e (gensym)]
       `(try-catch-non-fatal (either/right (do ~@body)) ~e (either/left ~e)))))

#?(:clj
   (defmacro try-promise
     "Try to evalute the body and return the result as a promise.
     If a non-fatal throwable is thrown return the exception as rejected,
     otherwise return the result as resolved."
     [& body]
     (let [e (gensym)]
       `(try-catch-non-fatal (p/resolved (do ~@body)) ~e (p/rejected ~e)))))

(defn either->promise [mv]
  (either/branch mv p/rejected p/resolved))
