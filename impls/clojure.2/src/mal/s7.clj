(ns mal.s7
  (:require [mal.core :as core]
            [mal.reader :as reader]
            [mal.repl-namespace :refer [repl-namespace]]))

(def CONTEXT
  (core/atom
    (core/->EvalContext
      (core/atom {(:name repl-namespace) repl-namespace})
      repl-namespace)))

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

(core/ns-bind repl-namespace (core/symbol "eval") EVAL)

(core/ns-bind repl-namespace (core/symbol "load-file")
  (fn [filename]
    (core/eval CONTEXT []
      (reader/read-string
        (core/str "(do " (core/slurp filename) "\n" "nil" ")")))))

(defn -main [& args]
  (core/ns-bind repl-namespace (core/symbol "*ARGV*") (apply list (rest args)))
  (when-some [filename (first args)]
    (rep (core/str "(load-file \"" filename "\")")))
  (loop []
    (print "user> ")
    (flush)
    (when-let [input (read-line)]
      (try
        (-> input rep println)
        (catch Throwable e
          (.printStackTrace e)))
      (recur))))
