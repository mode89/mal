#!/usr/bin/env clj

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

(defn main []
  (loop []
    (print "user> ")
    (flush)
    (when-let [input (read-line)]
      (-> input rep println)
      (recur))))

(main)
