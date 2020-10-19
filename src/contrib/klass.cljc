(ns contrib.klass
  (:refer-clojure :exclude [set?]))

(def get? (comp :get meta))
(def set? (comp :set meta))

(defn- gen-cast [sym]
  (case (:tag (meta sym))
    int      `int
    ints     `ints
    long     `long
    longs    `longs
    float    `float
    floats   `floats
    double   `double
    doubles  `doubles
    short    `short
    shorts   `shorts
    boolean  `boolean
    booleans `booleans
    byte     `byte
    bytes    `bytes
    char     `char
    chars    `chars
    `identity))

(defn- drop-metas [sym] (symbol (name sym)))

(defn- gen-get [prefix sym]
  (let [name (symbol (str prefix "get-" (name sym)))]
    `(~name [~'_] ~(drop-metas sym))))

(defn- gen-set [prefix sym]
  (let [name (symbol (str prefix "set-" (name sym)))]
    `(~name [~'_ v#] (set! ~(drop-metas sym) (~(gen-cast sym) v#)))))

(defn- gen-accessors [prefix fields]
  (reduce (fn [acc field]
            (cond-> acc
              (get? field) (conj (gen-get prefix field))
              (set? field) (conj (gen-set prefix field))))
          []
          fields))

(defmacro deftype+
  "Like `deftype`, but generates getters and setters methods for fields annotated
  with `^:get` or `^:set`. Useful to expose mutable fields which defaults to
  private, like (^:volatile-mutable or ^:unsynchronized-mutable). If this
  doesn’t sound like a terrible idea to you, don’t use this."
  [type-name fields & body]
  (let [accessors      (gen-accessors (:accessors-prefix (meta type-name)) fields)
        accessors-name (symbol (str (name type-name) "Accessors"))]
    (if (seq accessors)
      `(do
         (defprotocol ~accessors-name ~@(map butlast accessors))
         (deftype ~type-name ~fields ~@body ~accessors-name ~@accessors))
      `(deftype ~type-name ~fields ~@body))))
