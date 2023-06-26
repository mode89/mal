(in-ns 'mal.core)

(def ^:macro defrecord #'clojure.core/defrecord)
(def ^:macro defn #'clojure.core/defn)
(def ^:macro defmacro #'clojure.core/defmacro)
(def ^:macro fn #'clojure.core/fn)
(def ^:macro let #'clojure.core/let)
(def ^:macro declare #'clojure.core/declare)
(def ^:macro or #'clojure.core/or)
(def ^:macro and #'clojure.core/and)
(def ^:macro assert #'clojure.core/assert)
(def ^:macro when #'clojure.core/when)
(def ^:macro when-some #'clojure.core/when-some)
(def ^:macro if-some #'clojure.core/if-some)
(def ^:macro case #'clojure.core/case)
(def ^:macro cond #'clojure.core/cond)
(def ^:macro condp #'clojure.core/condp)
(def ^:macro loop #'clojure.core/loop)
(def ^:macro doseq #'clojure.core/doseq)
(def ^:macro -> #'clojure.core/->)

(def identical? clojure.core/identical?)
(def instance? clojure.core/instance?)
(def symbol clojure.core/symbol)
(def symbol? clojure.core/symbol?)
(def simple-symbol? clojure.core/simple-symbol?)
(def keyword clojure.core/keyword)
(def keyword? clojure.core/keyword?)
(def name clojure.core/name)
(def namespace clojure.core/namespace)
(def char? clojure.core/char?)
(def string? clojure.core/string?)
(def join clojure.string/join)
(def index-of clojure.string/index-of)
(def subs clojure.core/subs)
(def inc clojure.core/inc)
(def atom clojure.core/atom)
(def deref clojure.core/deref)
(def reset! clojure.core/reset!)
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
(def seq clojure.core/seq)
(def type clojure.core/type)
(def meta clojure.core/meta)
(def swap! clojure.core/swap!)
(def assoc clojure.core/assoc)
(def dissoc clojure.core/dissoc)
(def contains? clojure.core/contains?)
(def get clojure.core/get)
(def empty? clojure.core/empty?)
(def = clojure.core/=)
(def > clojure.core/>)
(def < clojure.core/<)
(def >= clojure.core/>=)
(def <= clojure.core/<=)
(def + clojure.core/+)
(def - clojure.core/-)
(def * clojure.core/*)
(def / clojure.core//)
(def count clojure.core/count)
(def reverse clojure.core/reverse)
(def first clojure.core/first)
(def second clojure.core/second)
(def rest clojure.core/rest)
(def cons clojure.core/cons)
(def drop clojure.core/drop)
(def nth clojure.core/nth)
(def reduce clojure.core/reduce)
(def reduce-kv clojure.core/reduce-kv)
(def into clojure.core/into)
(def concat clojure.core/concat)
(def partition clojure.core/partition)
(def partition-by clojure.core/partition-by)
(def butlast clojure.core/butlast)
(def last clojure.core/last)
(def even? clojure.core/even?)
(def print clojure.core/print)
(def slurp clojure.core/slurp)

(def native-fn? clojure.core/fn?)
(def native-to-string clojure.core/str)

(clojure.core/load "cross/core")
(clojure.core/load "cross/eval")
(clojure.core/load "cross/print")

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

(defn apply [f & args]
  (assert (> (count args) 0) "apply expects at least 2 arguments")
  (let [args* (let [rev-args (reverse args)
                    last-arg (first rev-args)]
                (assert (seqable? last-arg)
                  "last argument to apply must be a sequence")
                (reduce
                  (fn [acc x]
                    (cons x acc))
                  last-arg
                  (rest rev-args)))]
    (cond
      (instance? Function f)
        (let [make-locals (:make-locals f)]
          (eval (atom (:context f)) (make-locals args*) (:body f)))
      (native-fn? f)
        (clojure.core/apply f args*)
      :else
        (mal.core/throw (str "Can't call this: " (pr-str f))))))

(defn native-eval-try-catch [ctx locals try-expr catch-form]
  (try
    (eval ctx locals try-expr)
    (catch Throwable ex0
      (let [ex-binding (second catch-form)
            _ (assert (symbol? ex-binding)
                "exception object must be a symbol")
            catch-body (nth catch-form 2)
            ex (if (object-exception? ex0)
                 (object-exception-unwrap ex0)
                 ex0)]
        (eval ctx
              (cons {ex-binding ex} locals)
              catch-body)))))

(defn swap! [a f & args]
  (assert (atom? a))
  (clojure.core/swap! a
    (fn [x]
      (apply f x args))))

(defn map [f coll]
  (clojure.core/map
    (if (native-fn? f)
      f
      (fn [x]
        (apply f (list x))))
    coll))

(clojure.core/load "cross/core_ns")

(defn debug-macro [x]
  (clojure.core/let [m (meta x)
        tag (:tag m)
        prefix (if (clojure.core/some? tag)
                 (clojure.core/str tag)
                 nil)]
    `(clojure.core/let [x# ~x
           prefix# ~prefix]
       (if (clojure.core/some? prefix#)
         (clojure.core/println prefix# x#)
         (clojure.core/println x#))
       x#)))
