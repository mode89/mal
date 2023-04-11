(ns mal.core
  (:refer-clojure :exclude [eval keyword keyword? symbol symbol?])
  (:require [mal.environ :as environ]
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
            (call-form head args env))
          (call-form head args env))))
    (eval-form form env)))
