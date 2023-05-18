(ns mal.s8
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

(rep "(def! not
        (fn* (a)
          (if a false true)))")
(rep "(def! load-file
        (fn* (f)
          (eval (read-string (str \"(do \"
                                  (slurp f)
                                  \"\nnil)\")))))")
(rep "(defmacro! cond
        (fn* (& xs)
          (if (> (count xs) 0)
            (list 'if (first xs)
              (if (> (count xs) 1)
                (nth xs 1)
                (throw \"odd number of forms to cond\"))
              (cons 'cond (rest (rest xs)))))))")

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
