(ns mal.s5
  (:require [mal.core :as core]
            [mal.reader :as reader]
            [mal.types :as types]))

(def CONTEXT
  (core/atom
    (types/->EvalContext
      (core/atom {(:name core/core-ns) core/core-ns})
      core/core-ns)))

(defn READ [input]
  (reader/read-string input))

(defn EVAL [form]
  (core/eval CONTEXT [] form))

(defn PRINT [input]
  (core/pr-str input))

(defn rep [input]
  (-> input
      READ
      EVAL
      PRINT))

(defn -main [& _]
  (loop []
    (print "user> ")
    (flush)
    (when-let [input (read-line)]
      (try
        (-> input rep println)
        (catch Throwable e
          (.printStackTrace e)))
      (recur))))
