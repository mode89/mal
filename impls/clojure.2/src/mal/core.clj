(ns mal.core
  (:refer-clojure :exclude [apply atom concat cons deref eval first fn?
                            keys keyword keyword? list? macroexpand
                            map map? nth pr-str prn println read-string
                            reset! rest sequential? slurp str swap! symbol
                            symbol? vals vec vector?])
  (:require [clojure.core :as clj]
            [clojure.string :refer [join]]
            [mal.reader :as reader]
            [mal.types :as types])
  (:import [mal.types Atom Function Keyword Namespace EvalContext]))

(declare apply)
(declare atom)
(declare cons)
(declare deref)
(declare eval)
(declare first)
(declare map)
(declare nth)
(declare reset!)
(declare rest)
(declare str)
(declare swap!)
(declare throw)
(declare vec)

(defn keyword [name]
  (if (instance? Keyword name)
    name
    (new Keyword name)))

(defn keyword? [x]
  (instance? Keyword x))

(def symbol types/symbol)

(def symbol? types/symbol?)

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
    (instance? Namespace object)
      (clj/str "#<namespace " (-> object :name :name) ">")
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

(defn- ns-make [name]
  (assert (symbol? name) "namespace name must be a symbol")
  (new Namespace name (atom {})))

(defn ns-bind [ns name value]
  (assert (symbol? name) "namespace binding name must be a symbol")
  (swap! (:bindings ns) assoc name value))

(defn- ns-find-or-create [ctx name]
  (let [registry-atom (-> ctx deref :ns-registry)
        registry (deref registry-atom)]
    (if (contains? registry name)
      (get registry name)
      (let [new-ns (ns-make name)]
        (swap! registry-atom assoc name new-ns)
        new-ns))))

(defn- throw-not-found [sym]
  (mal.core/throw (str "'" (:name sym) "' not found")))

(defn resolve-symbol [ctx locals sym]
  (assert (symbol? sym))
  (assert (sequential? locals))
  (if-some [sym-ns-name (:namespace sym)]
    (if-some [sym-ns (-> ctx deref
                         :ns-registry deref
                         (get (symbol sym-ns-name)))]
      (let [bindings (-> sym-ns :bindings deref)
            simp-sym (symbol (:name sym))]
        (if (contains? bindings simp-sym)
          (get bindings simp-sym)
          (throw-not-found sym)))
      (mal.core/throw (str "namespace '" sym-ns-name "' not found")))
    (loop [locals* locals]
      (if (empty? locals*)
        (if-some [current-ns (-> ctx deref :current-ns)]
          (let [bindings (-> current-ns :bindings deref)]
            (if (contains? bindings sym)
              (get bindings sym)
              (throw-not-found sym)))
          (throw-not-found sym))
        (let [bindings (first locals*)]
          (if (contains? bindings sym)
            (get bindings sym)
            (recur (rest locals*))))))))

(defn eval-value [ctx locals ast]
  (cond
    (symbol? ast)
      (if (= (symbol "*ns*") ast)
        (-> ctx deref :current-ns)
        (resolve-symbol ctx locals ast))
    (list? ast)
      (map
        (fn [x]
          (eval ctx locals x))
        ast)
    (vector? ast)
      (clj/vec (map (fn [x]
                      (eval ctx locals x))
                    ast))
    (map? ast)
      (into {}
        (map
          (fn [[k v]]
            [(eval ctx locals k)
             (eval ctx locals v)])
          ast))
    :else
      ast))

(defn- fn-env-template
  "Returns a map of parameter names to functions that extract the
  corresponding argument from the argument list."
  [parameters]
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

(defn- fn-env-bindings
  "Returns a map of parameter names to the corresponding argument values."
  [env-template args]
  (reduce-kv
    (fn [env param arg-extractor]
      (assoc env param (arg-extractor args)))
    {}
    env-template))

(defn make-fn* [ctx locals macro? params body]
  (assert (instance? EvalContext ctx))
  (assert (sequential? locals))
  (let [env-template (fn-env-template params)]
    (new Function
      macro?
      params
      body
      ctx
      (fn [args]
        (cons (fn-env-bindings env-template args)
              locals)))))

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

(defn macroexpand [ctx locals form]
  (if (list? form)
    (let [head (first form)]
      (if (symbol? head)
        (if-some [macro (try (resolve-symbol ctx locals head)
                             (catch Exception _ nil))]
          (if (macro? macro)
            (let [mctx (:context macro)]
              (assert (identical? (:ns-registry mctx)
                                  (:ns-registry (deref ctx))))
              (apply macro (rest form)))
            form)
          form)
        form))
    form))

(defn eval [ctx locals form0]
  (if (list? form0)
    (if (empty? form0)
      form0
      (let [form (macroexpand ctx locals form0)]
        (if (not (list? form))
          (eval-value ctx locals form)
          (let [head (first form)
                args (rest form)]
            (condp = head
              (symbol "def!")
                (let [name (first args)
                      value-ast (second args)]
                  (assert (= (count args) 2))
                  (assert (symbol? name))
                  (let [value (eval ctx locals value-ast)
                        current-ns (-> ctx deref :current-ns)]
                    (assert (some? current-ns))
                    (ns-bind current-ns name value)
                    value))
              (symbol "defmacro!")
                (let [name (first args)
                      f (eval ctx locals (second args))]
                  (assert (= (count args) 2))
                  (assert (symbol? name))
                  (assert (fn? f))
                  (let [macro (assoc f :macro? true)
                        current-ns (-> ctx deref :current-ns)]
                    (assert (some? current-ns))
                    (swap! (:bindings current-ns) assoc name macro)
                    macro))
              (symbol "let*")
                (let [bindings (first args)
                      body (second args)]
                  (assert (even? (count bindings)))
                  (recur
                    ctx
                    (reduce
                      (fn [locals' [name value]]
                        (assert (symbol? name))
                        (cons {name (eval ctx locals' value)} locals'))
                      locals
                      (partition 2 bindings))
                    body))
              (symbol "do")
                (let [butlast-forms (butlast args)
                      last-form (last args)]
                  (loop [forms butlast-forms]
                    (when-some [form (first forms)]
                      (eval ctx locals form)
                      (recur (rest forms))))
                  (recur ctx locals last-form))
              (symbol "if")
                (let [nargs (count args)]
                  (assert (>= nargs 2))
                  (assert (<= nargs 3))
                  (if (eval ctx locals (first args))
                    (recur ctx locals (second args))
                    (if (= nargs 3)
                      (recur ctx locals (nth args 2))
                      nil)))
              (symbol "fn*")
                (make-fn*
                  (deref ctx)
                  locals
                  false
                  (first args)
                  (second args))
              (symbol "quote")
                (do (assert (= (count args) 1))
                    (first args))
              (symbol "quasiquote")
                (do (assert (= (count args) 1))
                    (recur ctx locals (quasiquote (first args))))
              (symbol "quasiquoteexpand")
                (do (assert (= (count args) 1))
                    (quasiquote (first args)))
              (symbol "macroexpand")
                (do (assert (= (count args) 1))
                    (macroexpand ctx locals (first args)))
              (symbol "try*")
                (let [try-expr (first args)]
                  (if-some [catch-form (second args)]
                    (do (assert (= (count args) 2))
                        (assert (= (count catch-form) 3))
                        (assert (= (first catch-form) (symbol "catch*")))
                        (try
                          (eval ctx locals try-expr)
                          (catch Throwable ex0
                            (let [ex-binding (second catch-form)
                                  _ (assert (symbol? ex-binding))
                                  catch-body (nth catch-form 2)
                                  ex (if (object-exception? ex0)
                                       (object-exception-unwrap ex0)
                                       ex0)]
                              (eval ctx
                                    (cons {ex-binding ex} locals)
                                    catch-body)))))
                    (do (assert (= (count args) 1))
                        (recur ctx locals try-expr))))
              (symbol "in-ns")
                (let [ns-name (first args)]
                  (assert (= (count args) 1))
                  (let [ns (ns-find-or-create ctx ns-name)]
                    (swap! ctx assoc :current-ns ns)
                    ns))
              (let [f (eval ctx locals head)
                    args (map (fn [x] (eval ctx locals x)) args)]
                (cond
                  (instance? Function f)
                    (let [fctx (:context f)
                          make-locals (:make-locals f)]
                      (assert (identical?
                                (:ns-registry fctx)
                                (:ns-registry (deref ctx))))
                      (recur (atom fctx)
                             (make-locals args)
                             (:body f)))
                  (clj/fn? f)
                    (clj/apply f args)
                  :else
                    (throw (ex-info "Can't call this" {:object f})))))))))
    (eval-value ctx locals form0)))

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

(defn deref [x]
  (cond
    (atom? x) (clj/deref (:value x))
    :else (throw (ex-info "Can't deref this" {:object x}))))

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
  (let [args* (let [rev-args (reverse args)
                    last-arg (first rev-args)]
                (assert (seqable? last-arg))
                (reduce
                  (fn [acc x]
                    (cons x acc))
                  last-arg
                  (rest rev-args)))]
    (cond
      (instance? Function f)
        (let [make-locals (:make-locals f)]
          (eval (atom (:context f)) (make-locals args*) (:body f)))
      (clj/fn? f)
        (clj/apply f args*)
      :else
        (throw (ex-info "Can't call this" {:object f})))))

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
  (new Namespace
    (symbol "mal.core")
    (atom
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
       (symbol "vals") vals})))
