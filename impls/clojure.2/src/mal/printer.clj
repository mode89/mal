(ns mal.printer
  (:refer-clojure :exclude [pr-str])
  (:require [clojure.string :refer [join]]
            [mal.core :as core]))

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
    (core/symbol? object)
      (str (:name object))
    (core/keyword? object)
      (str \: (:name object))
    (list? object)
      (str \( (join " " (map pr-str object)) \) )
    (vector? object)
      (str \[ (join " " (map pr-str object)) \] )
    (core/hash-map? object)
      (str \{ (join " " (map pr-str (flatten (into [] object)))) \} )
    (nil? object)
      "nil"
    :else
      (throw (ex-info "Don't know how to print this" {:object object}))))
