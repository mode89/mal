(ns mal.s3
  (:require [mal.core :as core]
            [mal.reader :as reader]))

(def repl-env
  (core/env-make
    nil
    {(core/symbol "+") (fn [a b] (+ a b))
     (core/symbol "-") (fn [a b] (- a b))
     (core/symbol "*") (fn [a b] (* a b))
     (core/symbol "/") (fn [a b] (/ a b))}))

(defn READ [input]
  (reader/read-string input))

(defn EVAL [form env]
  (core/eval form env))

(defn PRINT [input]
  (core/pr-object input true))

(defn rep [input]
  (-> input
      READ
      (EVAL repl-env)
      PRINT))

(defn -main [& _]
  (loop []
    (print "user> ")
    (flush)
    (when-let [input (read-line)]
      (try
        (-> input rep println)
        (catch Exception e
          (.printStackTrace e)))
      (recur))))
