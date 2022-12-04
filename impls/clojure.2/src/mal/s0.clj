(ns mal.s0)

(defn READ [input]
  input)

(defn EVAL [input]
  input)

(defn PRINT [input]
  input)

(defn rep [input]
  (-> input
      READ
      EVAL
      PRINT))

(defn main [& _]
  (loop []
    (print "user> ")
    (flush)
    (when-let [input (read-line)]
      (-> input rep println)
      (recur))))
