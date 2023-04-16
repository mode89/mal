(ns mal.python.compiler
  (:require [clojure.string :refer [join split-lines triml]]))

(defn indent [s prefix]
  (assert (string? s))
  (assert (string? prefix))
  (join "\n"
    (map (fn [line]
           (str prefix line))
         (split-lines s))))

(defn indent1 [s]
  (indent s "  "))

(defmacro assert-symbol [s]
  `(let [s# ~s]
     (assert (string? s#))
     (assert (not-any? (fn [c#] (= c# \newline)) s#))
     (assert (-> s# triml seq))))

(defmacro assert-line [s]
  `(let [s# ~s]
     (assert (string? s#))
     (assert (not-any? (fn [c#] (= c# \newline)) s#))
     (assert (-> s# triml seq))))

(defn indent-lines [lines prefix]
  (map (fn [line]
         (assert-line line)
         (str prefix line))
       lines))

(defn indent-lines1 [lines]
  (indent-lines lines "  "))

(defn emit-assign [name value]
  (assert-symbol name)
  (assert-line value)
  [(str name " = " value)])

(defn emit-call [name args kwargs]
  (assert-symbol name)
  (assert (map? kwargs))
  (let [kwargs* (sort (map (fn [[k v]]
                             (assert-symbol k)
                             (assert-line v)
                             (str k "=" v))
                           kwargs))]
    [(str name "(" (join ", " (concat args kwargs*)) ")")]))

(defn emit-if [condition then elifs else]
  (assert-line condition)
  (assert (seq then))
  (concat
    [(str "if " condition ":")]
    (indent-lines1 then)
    (when (some? elifs)
      (mapcat
        (fn [[condition body]]
          (assert-line condition)
          (assert (seq body))
          (cons (str "elif " condition ":")
                (indent-lines1 body)))
        elifs))
    (when (some? else)
      (assert (seq else))
      (cons "else:" (indent-lines1 else)))))

(defn emit-while [condition body]
  (assert-line condition)
  (assert (seq body))
  (cons (str "while " condition ":")
        (indent-lines1 body)))

(defn emit-break []
  ["break"])

(defn emit-continue []
  ["continue"])

(defn emit-def [name params body]
  (assert-symbol name)
  (assert (seq body))
  (let [params* (map (fn [param]
                       (assert-symbol param)
                       param)
                     params)]
    (cons
      (str "def " name "(" (join ", " params*) "):")
      (indent-lines1 body))))

(defn emit-return [value]
  (assert-line value)
  [(str "return " value)])

(defn emit-try [body excepts finally]
  (assert (seq body))
  (concat
    ["try:"]
    (indent-lines1 body)
    (when (some? excepts)
      (mapcat
        (fn [[exception binding body]]
          (assert-symbol exception)
          (assert (seq body))
          (cons
            (str "except " exception
                 (when (some? binding)
                   (assert-symbol binding)
                   (str " as " binding))
                 ":")
            (indent-lines1 body)))
           excepts))
    (when (some? finally)
      (assert (seq finally))
      (cons "finally:" (indent-lines1 finally)))))
