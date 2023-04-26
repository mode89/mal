(ns mal.s6
  (:require [mal.core :as core]
            [mal.reader :as reader]
            [mal.types :as types]
            ))

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

(core/ns-bind core/core-ns (core/symbol "eval") EVAL)

(core/ns-bind core/core-ns (core/symbol "load-file")
  (fn [filename]
    (core/eval CONTEXT []
      (core/read-string
        (core/str "(do " (core/slurp filename) "\n" "nil" ")")))))

(defn -main [& args]
  (core/ns-bind core/core-ns (core/symbol "*ARGV*") (apply list (rest args)))
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
