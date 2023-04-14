(ns mal.s4
  (:require [mal.core :as core]
            [mal.environ :as environ]
            [mal.reader :as reader]))

(def repl-env
  (environ/make
    nil
    core/core-ns))

(defn READ [input]
  (reader/read-string input))

(defn EVAL [form env]
  (core/eval form env))

(defn PRINT [input]
  (core/pr-str input))

(defn rep [input]
  (-> input
      READ
      (EVAL repl-env)
      PRINT))

(rep "(def! not
        (fn* (a)
          (if a false true)))")

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
