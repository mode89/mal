(ns mal.s3
  (:require [mal.core :as core]
            [mal.reader :as reader]
            [mal.repl-namespace :refer [repl-namespace]]))

(def CONTEXT
  (core/atom
    (core/->EvalContext
      (core/atom {(:name repl-namespace) repl-namespace})
      repl-namespace)))

(def repl-env
  {(core/symbol "+") (fn [a b] (+ a b))
   (core/symbol "-") (fn [a b] (- a b))
   (core/symbol "*") (fn [a b] (* a b))
   (core/symbol "/") (fn [a b] (/ a b))})

(defn READ [input]
  (reader/read-string input))

(defn EVAL [form]
  (core/eval CONTEXT [repl-env] form))

(defn PRINT [input]
  (core/pr-str* input true))

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
        (catch Exception e
          (.printStackTrace e)))
      (recur))))
