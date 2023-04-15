(ns mal.python.compiler
  (:require [clojure.string :refer [join split-lines]]))

(defn indent [s prefix]
  (assert (string? s))
  (assert (string? prefix))
  (join "\n"
    (map (fn [line]
           (str prefix line))
         (split-lines s))))

(defn indent1 [s]
  (indent s "  "))

(defn emit-assign [name value]
  (assert (string? name))
  (assert (string? value))
  (str name " = " value))

(defn emit-call [name args kwargs]
  (assert (string? name))
  (assert (seqable? args))
  (assert (map? kwargs))
  (let [kwargs* (sort (map (fn [[k v]]
                             (assert (string? k))
                             (assert (string? v))
                             (str k "=" v))
                           kwargs))]
    (str name "(" (join ", " (concat args kwargs*)) ")")))

(defn emit-return [value]
  (assert (string? value))
  (str "return " value))

(defn emit-if [condition then elifs else]
  (assert (string? condition))
  (assert (string? then))
  (str "if " condition ":\n"
       (indent1 then)
       (when (some? elifs)
         (assert (seqable? elifs))
         (str "\n" (join "\n"
                     (map (fn [[condition body]]
                            (assert (string? condition))
                            (assert (string? body))
                            (str "elif " condition ":\n"
                                 (indent1 body)))
                          elifs))))
       (when (some? else)
         (assert (string? else))
         (str "\nelse:\n"
              (indent1 else)))))

(defn emit-while [condition body]
  (assert (string? condition))
  (assert (string? body))
  (str "while " condition ":\n"
       (indent1 body)))

(defn emit-break []
  "break")

(defn emit-continue []
  "continue")

(defn emit-def [name params body]
  (assert (string? name))
  (assert (seqable? params))
  (assert (string? body))
  (str "def " name "(" (join ", "
                          (map (fn [param]
                                 (assert (string? param))
                                 param)
                               params)) "):\n"
       (indent1 body)))

(defn emit-try [body excepts finally]
  (assert (string? body))
  (assert (seqable? excepts))
  (str "try:\n"
       (indent1 body)
       (when (some? excepts)
         (assert (seqable? excepts))
         (str "\n" (join "\n"
                     (map (fn [[exception binding body]]
                            (assert (string? exception))
                            (assert (string? body))
                            (str "except " exception
                                 (when (some? binding)
                                   (assert (string? binding))
                                   (str " as " binding))
                                 ":\n"
                                 (indent1 body)))
                          excepts))))
       (when (some? finally)
         (assert (string? finally))
         (str "\nfinally:\n"
              (indent1 finally)))))
