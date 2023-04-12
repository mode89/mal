(ns mal.core
  (:refer-clojure
    :exclude [eval keyword keyword? pr-str prn println symbol symbol?]
    :rename {str clj-str
             println clj-println})
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

(defn pr-object [object print-readably]
  (cond
    (nil? object)
      "nil"
    (boolean? object)
      (clj-str object)
    (number? object)
      (clj-str object)
    (string? object)
      (if print-readably
        (apply clj-str (concat [\"] (map -pr-char-readable object) [\"]))
        object)
    (symbol? object)
      (clj-str (:name object))
    (keyword? object)
      (clj-str \: (:name object))
    (list? object)
      (clj-str \(
               (join " "
                 (map (fn [x]
                        (pr-object x print-readably))
                      object))
               \) )
    (vector? object)
      (clj-str \[
               (join " "
                 (map (fn [x]
                        (pr-object x print-readably))
                      object))
               \] )
    (hash-map? object)
      (clj-str \{
               (join " "
                 (map (fn [x]
                        (pr-object x print-readably))
                      (flatten (into [] object))))
               \} )
    (fn? object)
      (clj-str "#<function " (clj-str object) ">")
    :else
      (throw (ex-info "Don't know how to print this" {:object object}))))

(defn debug-macro [x]
  (let [m (meta x)
        tag (:tag m)
        prefix (if (some? tag)
                 (clj-str tag)
                 nil)]
    `(let [x# ~x
           prefix# ~prefix]
       (if (some? prefix#)
         (clj-println prefix# x#)
         (clj-println x#))
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

(defn- -fn-env-template [argument-definitions]
  (loop [arg-defs argument-definitions
         arg-index 0
         template {}]
    (if (empty? arg-defs)
      template
      (let [arg-def (first arg-defs)]
        (assert (symbol? arg-def))
        (if (= (:name arg-def) "&")
          (let [rest-arg (second arg-defs)]
            (assert (symbol? rest-arg))
            (assert (= (count arg-defs) 2))
            (assoc template rest-arg
                   (fn [arg-values]
                     (apply list (drop arg-index arg-values)))))
          (recur (rest arg-defs)
                 (inc arg-index)
                 (assoc template arg-def
                        (fn [arg-values]
                          (nth arg-values arg-index)))))))))

(defn- -fn-env [env-template argument-values]
  (reduce-kv
    (fn [env arg-name arg-value-extractor]
      (assoc env arg-name (arg-value-extractor argument-values)))
    {}
    env-template))

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
                        body (second args)
                        env-template (-fn-env-template arg-defs)]
                    (fn [& arg-values]
                      (eval body
                        (environ/make env
                          (-fn-env env-template arg-values)))))
            (call-form head args env))
          (call-form head args env))))
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
