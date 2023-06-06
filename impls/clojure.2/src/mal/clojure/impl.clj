(in-ns 'mal.clojure.impl)

(def ^:macro defrecord #'clojure.core/defrecord)
(def ^:macro defn #'clojure.core/defn)

(def identical? clojure.core/identical?)
(def instance? clojure.core/instance?)
(def native-fn? clojure.core/fn?)
(def symbol clojure.core/symbol)
(def symbol? clojure.core/symbol?)
(def simple-symbol? clojure.core/simple-symbol?)
(def keyword clojure.core/keyword)
(def keyword? clojure.core/keyword?)
(def name clojure.core/name)
(def namespace clojure.core/namespace)
(def str clojure.core/str)
(def string? clojure.core/string?)
(def join clojure.string/join)
(def index-of clojure.string/index-of)
(def subs clojure.core/subs)
(def inc clojure.core/inc)
(def atom clojure.core/atom)
(def deref clojure.core/deref)
(def reset! clojure.core/reset!)
(def swap! clojure.core/swap!)
(def boolean? clojure.core/boolean?)
(def number? clojure.core/number?)
(def list clojure.core/list)
(def vec clojure.core/vec)
(def vector clojure.core/vector)
(def vector? clojure.core/vector?)
(def hash-map clojure.core/hash-map)
(def hash-set clojure.core/hash-set)
(def set? clojure.core/set?)
(def seq? clojure.core/seq?)
(def apply clojure.core/apply)
(def seq clojure.core/seq)
(def type clojure.core/type)
(def meta clojure.core/meta)
(def swap! clojure.core/swap!)
(def assoc clojure.core/assoc)
(def dissoc clojure.core/dissoc)
(def contains? clojure.core/contains?)
(def get clojure.core/get)
(def empty? clojure.core/empty?)
(def equal? clojure.core/=)
(def greater? clojure.core/>)
(def less? clojure.core/<)
(def greater-equal? clojure.core/>=)
(def less-equal? clojure.core/<=)
(def plus clojure.core/+)
(def minus clojure.core/-)
(def multiply clojure.core/*)
(def divide clojure.core//)
(def count clojure.core/count)
(def reverse clojure.core/reverse)
(def first clojure.core/first)
(def second clojure.core/second)
(def rest clojure.core/rest)
(def cons clojure.core/cons)
(def drop clojure.core/drop)
(def nth clojure.core/nth)
(def map clojure.core/map)
(def reduce clojure.core/reduce)
(def reduce-kv clojure.core/reduce-kv)
(def into clojure.core/into)
(def even? clojure.core/even?)
(def print clojure.core/print)
(def slurp clojure.core/slurp)

(def OBJECT-EXCEPTION (new Object))

(defn object-exception [obj]
  (clojure.core/ex-info "object exception"
    {:object obj :tag OBJECT-EXCEPTION}))

(defn object-exception? [ex]
  (clojure.core/and
    (instance? clojure.lang.ExceptionInfo ex)
    (clojure.core/=
      OBJECT-EXCEPTION
      (clojure.core/-> ex clojure.core/ex-data :tag))))

(defn object-exception-unwrap [ex]
  (clojure.core/-> ex clojure.core/ex-data :object))

(defn throw [obj]
  (if (instance? Throwable obj)
    (throw obj)
    (throw (object-exception obj))))

(defn atom? [x]
  (instance? clojure.lang.Atom x))

(defn list? [x]
  (clojure.core/seq? x))

(defn map? [x]
  (clojure.core/or
    (instance? clojure.lang.PersistentArrayMap x)
    (instance? clojure.lang.PersistentHashMap x)))

(defn keys [m]
  (if (empty? m)
    (list)
    (clojure.core/keys m)))

(defn vals [m]
  (if (empty? m)
    (list)
    (clojure.core/vals m)))

(defn debug-macro [x]
  (clojure.core/let [m (meta x)
        tag (:tag m)
        prefix (if (clojure.core/some? tag)
                 (str tag)
                 nil)]
    `(clojure.core/let [x# ~x
           prefix# ~prefix]
       (if (clojure.core/some? prefix#)
         (clojure.core/println prefix# x#)
         (clojure.core/println x#))
       x#)))
