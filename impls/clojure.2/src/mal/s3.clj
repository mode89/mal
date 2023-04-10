(ns mal.s3
  (:require [mal.core :as core]
            [mal.environ :as environ]
            [mal.printer :as printer]
            [mal.reader :as reader]))

(def repl-env
  (environ/make
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
