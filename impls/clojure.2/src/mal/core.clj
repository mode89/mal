(ns mal.core
  (:refer-clojure :exclude [atom concat cons deref eval fn? keyword keyword?
                            list?  pr-str prn println read-string reset!
                            slurp str swap!  symbol symbol?])
  (:require [clojure.core :as clj]
            [clojure.string :refer [join]]
            [mal.environ :as environ]
            [mal.reader :as reader]
            [mal.types])
  (:import [mal.types Atom Function Keyword Symbol]))

(declare eval)

(defn keyword [name]
  (Keyword. name))

(defn keyword? [x]
  (instance? Keyword x))

(defn symbol [name]
  (Symbol. name))

(defn symbol? [x]
  (instance? Symbol x))

(defn hash-map? [x]
  (instance? clojure.lang.PersistentArrayMap x))

(defn fn? [x]
  (or (instance? Function x)
      (clj/fn? x)))

(defn atom [x]
  (new Atom (clj/atom x)))

(defn atom? [x]
  (instance? Atom x))

(defn list? [x]
  (clj/list? x))

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
    (hash-map? object)
      (clj/str \{
               (join " "
                 (map (fn [x]
                        (pr-object x print-readably))
                      (flatten (into [] object))))
               \} )
    (fn? object)
      (clj/str "#<function " (clj/str object) ">")
    (atom? object)
      (clj/str "(atom " (-> object :value clj/deref) ")")
    :else
      (throw (ex-info "Don't know how to print this" {:object object}))))

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

(defn eval-form [ast env]
  (cond
    (symbol? ast)
      (environ/get env ast)
    (list? ast)
      (apply list
        (map
          (fn [x]
            (eval x env))
          ast))
    (vector? ast)
      (vec (map (fn [x] (eval x env)) ast))
    (hash-map? ast)
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
                     (apply list (drop param-index args)))))
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

(defn quasiquote [ast]
  (if (list? ast)
    (cond
      (empty? ast)
        ast
      (= (first ast) (symbol "unquote"))
        (second ast)
      :else
        (let [element (first ast)]
          (if (and (list? element)
                   (= (first element) (symbol "splice-unquote")))
            (if (> (count ast) 1)
              (list (symbol "concat")
                    (second element)
                    (quasiquote (rest ast)))
              (second element))
            (if (> (count ast) 1)
              (list (symbol "cons")
                    (quasiquote element)
                    (quasiquote (rest ast)))
              (list (symbol "list")
                    (quasiquote element))))))
    (if (or (symbol? ast)
            (hash-map? ast))
      (list (symbol "quote") ast)
      ast)))

(defn eval [form env]
  (if (list? form)
    (if (empty? form)
      form
      (let [head (first form)
            args (rest form)]
        (condp = head
          (symbol "def!")
            (let [name (first args)
                  value-ast (second args)]
              (assert (= (count args) 2))
              (assert (symbol? name))
              (let [value (eval value-ast env)]
                (environ/set! env name value)
                value))
          (symbol "let*")
            (let [let-env (environ/make env {})
                  bindings (first args)
                  body (second args)]
              (assert (even? (count bindings)))
              (loop [bs bindings]
                (when (>= (count bs) 2)
                  (let [bname (first bs)
                        bvalue (second bs)]
                    (assert (symbol? bname))
                    (environ/set! let-env
                                  bname
                                  (eval bvalue let-env))
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
            (let [params (first args)
                  body (second args)
                  env-template (-fn-env-template params)]
              (new Function
                params
                body
                (fn [args]
                  (environ/make env
                    (-fn-env-bindings env-template args)))))
          (symbol "quote")
            (do (assert (= (count args) 1))
                (first args))
          (symbol "quasiquote")
            (do (assert (= (count args) 1))
                (recur (quasiquote (first args)) env))
          (let [f (eval head env)
                args (map (fn [x] (eval x env)) args)]
            (cond
              (instance? Function f)
                (let [body (:body f)
                      make-env (:make-env f)]
                  (recur body (make-env args)))
              (clj/fn? f)
                (apply f args)
              :else
                (throw (ex-info "Can't call this" {:object f})))))))
    (eval-form form env)))

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
      (eval (clj/apply list f x args)
            (environ/make nil {})))))

(defn cons [x xs]
  (cond
    (list? xs)
      (clj/conj xs x)
    (vector? xs)
      (apply list (clj/cons x xs))))

(defn concat [& args]
  (if (empty? args)
    (list)
    (let [lists (reverse args)]
      (reduce
        (fn [acc l]
          (loop [l (reverse l)
                 result acc]
            (if (empty? l)
              result
              (recur (rest l) (conj result (first l))))))
        (first lists)
        (rest lists)))))

(def core-ns
  {(symbol "list") list
   (symbol "list?") list?
   (symbol "empty?") empty?
   (symbol "count") count
   (symbol "=") =
   (symbol "<") <
   (symbol "<=") <=
   (symbol ">") >
   (symbol ">=") >=
   (symbol "+") +
   (symbol "-") -
   (symbol "*") *
   (symbol "/") /
   (symbol "not") not
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
   (symbol "concat") concat})
