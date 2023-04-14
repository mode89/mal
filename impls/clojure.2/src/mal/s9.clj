(ns mal.s9
  (:require [mal.core :as core]))

(def repl-env
  (core/env-make
    nil
    core/core-ns))
(core/env-set! repl-env (core/symbol "eval")
  (fn [form]
    (core/eval form repl-env)))

(defn READ [input]
  (core/read-string input))

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
  (core/env-set! repl-env (core/symbol "*ARGV*") (apply list (rest args)))
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
