(ns mal.core
  (:refer-clojure :exclude [eval keyword keyword? pr-str symbol symbol?])
  (:require [clojure.string :refer [join]]
            [mal.environ :as environ]
            [mal.types])
  (:import [mal.types Keyword Symbol]))

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

(defn- -pr-char-readable [ch]
  (case ch
    \"       "\\\""
    \newline "\\n"
    \tab     "\\t"
    \\       "\\\\"
    ch))

(defn pr-str [object print-readably]
  (cond
    (nil? object)
      "nil"
    (boolean? object)
      (str object)
    (number? object)
      (str object)
    (string? object)
      (if print-readably
        (apply str (concat [\"] (map -pr-char-readable object) [\"]))
        object)
    (symbol? object)
      (str (:name object))
    (keyword? object)
      (str \: (:name object))
    (list? object)
      (str \( (join " " (map (fn [x]
                               (pr-str x print-readably))
                             object)) \) )
    (vector? object)
      (str \[ (join " " (map (fn [x]
                               (pr-str x print-readably))
                             object)) \] )
    (hash-map? object)
      (str \{ (join " " (map (fn [x]
                               (pr-str x print-readably))
                             (flatten (into [] object)))) \} )
    (fn? object)
      (str "#<function " (str object) ">")
    :else
      (throw (ex-info "Don't know how to print this" {:object object}))))

(defn debug-macro [x]
  (let [m (meta x)
        tag (:tag m)
        prefix (if (some? tag)
                 (str tag)
                 nil)]
    `(let [x# ~x
           prefix# ~prefix]
       (if (some? prefix#)
         (println prefix# x#)
         (println x#))
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

(defn call-form [f-ast args-ast env]
  (let [f (eval f-ast env)
        args (map (fn [x] (eval x env)) args-ast)]
    (apply f args)))

(defn eval [form env]
  (if (list? form)
    (if (empty? form)
      form
      (let [head (first form)
            args (rest form)]
        (if (symbol? head)
          (case (:name head)
            "def!" (let [name (first args)
                         value-ast (second args)]
                     (assert (= (count args) 2))
                     (assert (symbol? name))
                     (let [value (eval value-ast env)]
                       (environ/set! env name (eval value env))
                       value))
            "let*" (let [let-env (environ/make env {})
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
                     (eval body let-env))
            "do" (loop [forms args
                        result nil]
                   (if-some [form (first forms)]
                     (recur (rest forms) (eval form env))
                     result))
            "if" (let [nargs (count args)]
                   (assert (>= nargs 2))
                   (assert (<= nargs 3))
                   (if (eval (first args) env)
                     (eval (second args) env)
                     (if (= nargs 3)
                       (eval (nth args 2) env)
                       nil)))
            "fn*" (let [arg-defs (first args)
                        body (second args)]
                    (doseq [adef arg-defs]
                      (assert (symbol? adef)))
                    (fn [& arg-values]
                      (assert (= (count arg-defs) (count arg-values)))
                      (eval body
                        (environ/make env
                          (zipmap arg-defs arg-values)))))
            (call-form head args env))
          (call-form head args env))))
    (eval-form form env)))
