(ns mal.s2
  (:require [mal.core :as core]
            [mal.printer :as printer]
            [mal.reader :as reader]))

(declare EVAL)

(def repl-env
  {(core/symbol "+") (fn [a b] (+ a b))
   (core/symbol "-") (fn [a b] (- a b))
   (core/symbol "*") (fn [a b] (* a b))
   (core/symbol "/") (fn [a b] (/ a b))})

(defn eval-ast [ast env]
  (cond
    (core/symbol? ast)
      (if (contains? env ast)
        (get env ast)
        (throw (ex-info "Symbol not found" {:symbol ast})))
    (list? ast)
      (apply list
        (map
          (fn [x]
            (EVAL x env))
          ast))
    (vector? ast)
      (vec (map (fn [x] (EVAL x env)) ast))
    (core/hash-map? ast)
      (into {}
        (map
          (fn [[k v]]
            [(EVAL k env) (EVAL v env)])
          ast))
    :else
      ast))

(defn READ [input]
  (reader/read-string input))

(defn EVAL [ast env]
  (if (list? ast)
    (if (empty? ast)
      ast
      (let [evaluated-ast (eval-ast ast env)
            function (first evaluated-ast)
            args (rest evaluated-ast)]
        (apply function args)))
    (eval-ast ast env)))

(defn PRINT [input]
  (printer/pr-str input))

(defn rep [input]
  (-> input
      READ
      (EVAL repl-env)
      PRINT))

(defn main [& _]
  (loop []
    (print "user> ")
    (flush)
    (when-let [input (read-line)]
      (try
        (-> input rep println)
        (catch Exception e
          (.printStackTrace e)))
      (recur))))
