(ns mal.printer
  (:refer-clojure :exclude [pr-str])
  (:require [clojure.string :refer [join]]
            [mal.lexer])
  (:import [mal.lexer Symbol]))

(defn pr-char [ch]
  (case ch
    \"       "\\\""
    \newline "\\n"
    \tab     "\\t"
    \\       "\\\\"
    ch))

(defn pr-str [object]
  (cond
    (number? object)
      (str object)
    (string? object)
      (apply str (concat [\"] (map pr-char object) [\"]))
    (instance? Symbol object)
      (str (:name object))
    (list? object)
      (str \( (join " " (map pr-str object)) \) )
    (vector? object)
      (str \[ (join " " (map pr-str object)) \] )
    (map? object)
      (str \{ (join " " (map pr-str (flatten (into [] object)))) \} )
    :else
      (throw (ex-info "Don't know how to print this" {:object object}))))
