(ns mal.s1
  (:require [mal.printer :as printer]
            [mal.reader :as reader]))

(defn READ [input]
  (reader/read-string input))

(defn EVAL [input]
  input)

(defn PRINT [input]
  (printer/pr-str input true))

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
      (try
        (-> input rep println)
        (catch Exception e
          (.printStackTrace e)))
      (recur))))
