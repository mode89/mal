(ns mal.core
  (:refer-clojure :exclude [apply atom concat cons deref eval first fn? keys
                            keyword keyword? list? macroexpand map map? nth
                            pr-str prn println read-string reset! rest
                            sequential?  slurp str swap! symbol symbol? vals
                            vec vector?])
  (:require [clojure.core :as clj]
            [clojure.string :refer [join]]
            [mal.reader :as reader]
            [mal.types])
  (:import [mal.types Atom Function Keyword Symbol]))

(declare apply)
(declare cons)
(declare eval)
(declare first)
(declare map)
(declare nth)
(declare rest)
(declare str)
(declare throw)

(defn keyword [name]
  (if (instance? Keyword name)
    name
    (new Keyword name)))

(defn keyword? [x]
  (instance? Keyword x))

(defn symbol [name]
  (Symbol. name))

(defn symbol? [x]
  (instance? Symbol x))

(defn map? [x]
  (or (instance? clojure.lang.PersistentArrayMap x)
      (instance? clojure.lang.PersistentHashMap x)))

(defn fn? [x]
  (or (and (instance? Function x)
           (not (:macro? x)))
      (clj/fn? x)))

(defn macro? [x]
  (and (instance? Function x)
       (:macro? x)))

(defn atom [x]
  (new Atom (clj/atom x)))

(defn atom? [x]
  (instance? Atom x))

(defn list? [x]
  (or (clj/list? x)
      (clj/seq? x)))

(defn vector? [x]
  (instance? clojure.lang.PersistentVector x))

(defn sequential? [x]
  (or (list? x)
      (vector? x)))

(def -OBJECT-EXCEPTION (new Object))

(defn object-exception [obj]
  (ex-info "object exception" {:object obj :tag -OBJECT-EXCEPTION}))

(defn object-exception? [ex]
  (and (instance? clojure.lang.ExceptionInfo ex)
       (= -OBJECT-EXCEPTION (-> ex ex-data :tag))))

(defn object-exception-unwrap [ex]
  (-> ex ex-data :object))

(defn- -pr-char-readable [ch]
  (case ch
    \"       "\\\""
    \newline "\\n"
    \tab     "\\t"
    \\       "\\\\"
    ch))

(defn pr-object [object print-readably]
  (cond
    (nil? object)
      "nil"
    (boolean? object)
      (clj/str object)
    (number? object)
      (clj/str object)
    (string? object)
      (if print-readably
        (apply clj/str
          (clj/concat [\"] (map -pr-char-readable object) [\"]))
        object)
    (symbol? object)
      (clj/str (:name object))
    (keyword? object)
      (clj/str \: (:name object))
    (list? object)
      (clj/str \(
               (join " "
                 (map (fn [x]
                        (pr-object x print-readably))
                      object))
               \) )
    (vector? object)
      (clj/str \[
               (join " "
                 (map (fn [x]
                        (pr-object x print-readably))
                      object))
               \] )
    (map? object)
      (clj/str \{
               (join " "
                 (map (fn [x]
                        (pr-object x print-readably))
                      (flatten (into [] object))))
               \} )
    (fn? object)
      (clj/str "#<function " (clj/str object) ">")
    (macro? object)
      (clj/str "#<macro " (clj/str object) ">")
    (atom? object)
      (clj/str "(atom " (-> object :value clj/deref) ")")
    :else
      (clj/str "#<" (clj/pr-str (type object)) " " (clj/pr-str object) ">")))

(defn debug-macro [x]
  (let [m (meta x)
        tag (:tag m)
        prefix (if (some? tag)
                 (clj/str tag)
                 nil)]
    `(let [x# ~x
           prefix# ~prefix]
       (if (some? prefix#)
         (clj/println prefix# x#)
         (clj/println x#))
       x#)))

(defn env-make [outer bindings]
  (clj/atom {:outer outer
             :table bindings}))

(defn env-set! [env key value]
  (clj/swap! env assoc-in [:table key] value))

(defn env-get [env key]
  (let [e (clj/deref env)
        table (:table e)
        outer (:outer e)]
    (if (contains? table key)
      (clojure.core/get table key)
      (if (some? outer)
        (env-get outer key)
        (mal.core/throw (str "'" (:name key) "' not found"))))))

(defn eval-form [ast env]
  (cond
    (symbol? ast)
      (env-get env ast)
    (list? ast)
      (map
        (fn [x]
          (eval x env))
        ast)
    (vector? ast)
      (clj/vec (map (fn [x] (eval x env)) ast))
    (map? ast)
      (into {}
        (map
          (fn [[k v]]
            [(eval k env) (eval v env)])
          ast))
    :else
      ast))

(defn- -fn-env-template [parameters]
  (loop [params parameters
         param-index 0
         template {}]
    (if (empty? params)
      template
      (let [param (first params)]
        (assert (symbol? param))
        (if (= (:name param) "&")
          (let [rest-arg (second params)]
            (assert (symbol? rest-arg))
            (assert (= (count params) 2))
            (assoc template rest-arg
                   (fn [args]
                     (drop param-index args))))
          (recur (rest params)
                 (inc param-index)
                 (assoc template param
                        (fn [args]
                          (nth args param-index)))))))))

(defn- -fn-env-bindings [env-template args]
  (reduce-kv
    (fn [env param arg-extractor]
      (assoc env param (arg-extractor args)))
    {}
    env-template))

(defn make-fn* [macro? env params body]
  (let [env-template (-fn-env-template params)]
    (new Function
      macro?
      params
      body
      (fn [args]
        (env-make env
          (-fn-env-bindings env-template args))))))

(defn quasiquote [ast]
  (if (list? ast)
    (cond
      (empty? ast)
        ast
      (and (= (first ast) (symbol "unquote"))
           (= (count ast) 2))
        (second ast)
      :else
        (let [element (first ast)]
          (if (and (list? element)
                   (= (first element) (symbol "splice-unquote")))
            (list (symbol "concat")
                  (second element)
                  (quasiquote (rest ast)))
            (list (symbol "cons")
                  (quasiquote element)
                  (quasiquote (rest ast))))))
    (cond
      (or (symbol? ast) (map? ast))
        (list (symbol "quote") ast)
      (vector? ast)
        (list (symbol "vec")
          (quasiquote (seq ast)))
      :else
        ast)))

(defn macroexpand [form env]
  (if (list? form)
    (let [head (first form)]
      (if (symbol? head)
        (if-some [macro (try (env-get env head)
                             (catch Exception _ nil))]
          (if (macro? macro)
            (let [args (rest form)
                  body (:body macro)
                  make-env (:make-env macro)]
              (recur (eval body (make-env args)) env))
            form)
          form)
        form))
    form))

(defn eval [form0 env]
  (if (list? form0)
    (if (empty? form0)
      form0
      (let [form (macroexpand form0 env)]
        (if (not (list? form))
          (eval-form form env)
          (let [head (first form)
                args (rest form)]
            (condp = head
              (symbol "def!")
                (let [name (first args)
                      value-ast (second args)]
                  (assert (= (count args) 2))
                  (assert (symbol? name))
                  (let [value (eval value-ast env)]
                    (env-set! env name value)
                    value))
              (symbol "defmacro!")
                (let [name (first args)
                      f (eval (second args) env)]
                  (assert (= (count args) 2))
                  (assert (symbol? name))
                  (assert (fn? f))
                  (let [macro (assoc f :macro? true)]
                    (env-set! env name macro)
                    macro))
              (symbol "let*")
                (let [let-env (env-make env {})
                      bindings (first args)
                      body (second args)]
                  (assert (even? (count bindings)))
                  (loop [bs bindings]
                    (when (>= (count bs) 2)
                      (let [bname (first bs)
                            bvalue (second bs)]
                        (assert (symbol? bname))
                        (env-set! let-env bname (eval bvalue let-env))
                        (recur (drop 2 bs)))))
                  (recur body let-env))
              (symbol "do")
                (let [butlast-forms (butlast args)
                      last-form (last args)]
                  (loop [forms butlast-forms]
                    (when-some [form (first forms)]
                      (eval form env)
                      (recur (rest forms))))
                  (eval last-form env))
              (symbol "if")
                (let [nargs (count args)]
                  (assert (>= nargs 2))
                  (assert (<= nargs 3))
                  (if (eval (first args) env)
                    (recur (second args) env)
                    (if (= nargs 3)
                      (recur (nth args 2) env)
                      nil)))
              (symbol "fn*")
                (make-fn* false env (first args) (second args))
              (symbol "quote")
                (do (assert (= (count args) 1))
                    (first args))
              (symbol "quasiquote")
                (do (assert (= (count args) 1))
                    (recur (quasiquote (first args)) env))
              (symbol "quasiquoteexpand")
                (do (assert (= (count args) 1))
                    (quasiquote (first args)))
              (symbol "macroexpand")
                (do (assert (= (count args) 1))
                    (macroexpand (first args) env))
              (symbol "try*")
                (let [try-expr (first args)]
                  (if-some [catch-form (second args)]
                    (do (assert (= (count args) 2))
                        (assert (= (count catch-form) 3))
                        (assert (= (first catch-form) (symbol "catch*")))
                        (try
                          (eval try-expr env)
                          (catch Throwable ex0
                            (let [ex-binding (second catch-form)
                                  _ (assert (symbol? ex-binding))
                                  catch-body (nth catch-form 2)
                                  ex (if (object-exception? ex0)
                                       (object-exception-unwrap ex0)
                                       ex0)
                                  catch-env (env-make env {ex-binding ex})]
                              (eval catch-body catch-env)))))
                    (do (assert (= (count args) 1))
                        (recur try-expr env))))
              (let [f (eval head env)
                    args (map (fn [x] (eval x env)) args)]
                (cond
                  (instance? Function f)
                    (let [body (:body f)
                          make-env (:make-env f)]
                      (recur body (make-env args)))
                  (clj/fn? f)
                    (clj/apply f args)
                  :else
                    (throw (ex-info "Can't call this" {:object f})))))))))
    (eval-form form0 env)))

(defn pr-str [& args]
  (join " "
    (map (fn [x]
           (pr-object x true))
         args)))

(defn str [& args]
  (join ""
    (map (fn [x]
           (pr-object x false))
         args)))

(defn prn [& args]
  (print (apply pr-str args))
  (print \newline))

(defn println [& args]
  (print
    (join " "
      (map (fn [x]
             (pr-object x false))
           args)))
  (print \newline))

(def read-string reader/read-string)

(defn slurp [filename]
  (clj/slurp filename))

(defn deref [a]
  (assert (atom? a))
  (clj/deref (:value a)))

(defn reset! [a v]
  (assert (atom? a))
  (clj/reset! (:value a) v))

(defn swap! [a f & args]
  (assert (atom? a))
  (clj/swap! (:value a)
    (fn [x]
      (apply f x args))))

(defn cons [x xs]
  (clj/cons x xs))

(defn concat [& args]
  (clj/apply clj/concat args))

(defn vec [l]
  (cond
    (list? l) (clj/vec l)
    (vector? l) l
    (nil? l) []
    :else (throw (ex-info "Can't convert to vector" {:object l}))))

(defn nth [coll i]
  (cond
    (list? coll) (clj/nth coll i)
    (vector? coll) (clj/nth coll i)
    (nil? coll) nil
    (seq? coll) (clj/nth coll i)
    :else (throw (ex-info "`nth` not supported on this type"
                          {:object coll :type (type coll)}))))

(defn first [coll]
  (clj/first coll))

(defn rest [coll]
  (clj/rest coll))

(defn apply [f & args]
  (assert (> (count args) 0))
  (cond
    (instance? Function f)
      (let [body (:body f)
            make-env (:make-env f)]
        (eval body
          (make-env (let [rev-args (reverse args)
                          last-arg (first rev-args)]
                      (assert (seqable? last-arg))
                      (reduce
                        (fn [acc x]
                          (cons x acc))
                        last-arg
                        (rest rev-args))))))
    (clj/fn? f)
      (clj/apply clj/apply f args)
    :else
      (throw (ex-info "Can't call this" {:object f}))))

(defn map [f coll]
  (clj/map
    (if (clj/fn? f)
      f
      (fn [x]
        (apply f (list x))))
    coll))

(defn keys [m]
  (if (empty? m)
    (list)
    (clj/keys m)))

(defn vals [m]
  (if (empty? m)
    (list)
    (clj/vals m)))

(defn throw [obj]
  (if (instance? Throwable obj)
    (throw obj)
    (throw (object-exception obj))))

(def core-ns
  {(symbol "list") clj/list
   (symbol "list?") list?
   (symbol "empty?") clj/empty?
   (symbol "count") clj/count
   (symbol "=") =
   (symbol "<") <
   (symbol "<=") <=
   (symbol ">") >
   (symbol ">=") >=
   (symbol "+") +
   (symbol "-") -
   (symbol "*") *
   (symbol "/") /
   (symbol "pr-str") pr-str
   (symbol "prn") prn
   (symbol "str") str
   (symbol "println") println
   (symbol "read-string") read-string
   (symbol "slurp") slurp
   (symbol "atom") atom
   (symbol "atom?") atom?
   (symbol "deref") deref
   (symbol "reset!") reset!
   (symbol "swap!") swap!
   (symbol "cons") cons
   (symbol "concat") concat
   (symbol "vec") vec
   (symbol "fn?") fn?
   (symbol "macro?") macro?
   (symbol "nth") nth
   (symbol "first") first
   (symbol "rest") rest
   (symbol "throw") mal.core/throw
   (symbol "apply") apply
   (symbol "map") map
   (symbol "nil?") clj/nil?
   (symbol "true?") clj/true?
   (symbol "false?") clj/false?
   (symbol "symbol") symbol
   (symbol "symbol?") symbol?
   (symbol "keyword") keyword
   (symbol "keyword?") keyword?
   (symbol "vector") clj/vector
   (symbol "vector?") vector?
   (symbol "sequential?") sequential?
   (symbol "hash-map") clj/hash-map
   (symbol "map?") map?
   (symbol "assoc") clj/assoc
   (symbol "dissoc") clj/dissoc
   (symbol "get") clj/get
   (symbol "contains?") clj/contains?
   (symbol "keys") keys
   (symbol "vals") vals})
