(ns mal.s7
  (:require [mal.core :as core]
            [mal.environ :as environ]
            [mal.reader :as reader]))

(def repl-env
  (environ/make
    nil
    core/core-ns))
(environ/set! repl-env (core/symbol "eval")
  (fn [form]
    (core/eval form repl-env)))
(environ/set! repl-env (core/symbol "load-file")
  (fn [filename]
    (core/eval
      (core/read-string
        (core/str "(do " (core/slurp filename) "\n" "nil" ")"))
      repl-env)))

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

(defn -main [& args]
  (environ/set! repl-env (core/symbol "*ARGV*") (apply list (rest args)))
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
