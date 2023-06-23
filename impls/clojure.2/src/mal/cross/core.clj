(defrecord Function [macro? params body context make-locals])
(defrecord Namespace [name bindings])
(defrecord EvalContext [ns-registry current-ns])

(declare list?)
(declare map?)
(declare apply)

(defn nil? [x]
  (identical? x nil))

(defn true? [x]
  (identical? x true))

(defn false? [x]
  (identical? x false))

(defn not [x]
  (if x false true))

(defn some? [x]
  (not (nil? x)))

(defn fn? [x]
  (or (and (instance? Function x)
           (not (:macro? x)))
      (native-fn? x)))

(defn macro? [x]
  (and (instance? Function x)
       (:macro? x)))

(defn sequential? [x]
  (or (list? x)
      (vector? x)))

(defn seqable? [x]
  (or (nil? x)
      (list? x)
      (vector? x)
      (map? x)
      (set? x)
      (seq? x)))

(defn mapcat [f & colls]
  (apply concat (apply map f colls)))
