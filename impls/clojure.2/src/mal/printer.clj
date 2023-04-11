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

(defn pr-str [object print-readably]
  (cond
    (nil? object)
      "nil"
    (boolean? object)
      (str object)
    (number? object)
      (str object)
    (string? object)
      (if print-readably
        (apply str (concat [\"] (map pr-char object) [\"]))
        object)
    (core/symbol? object)
      (str (:name object))
    (core/keyword? object)
      (str \: (:name object))
    (list? object)
      (str \( (join " " (map (fn [x]
                               (pr-str x print-readably))
                             object)) \) )
    (vector? object)
      (str \[ (join " " (map (fn [x]
                               (pr-str x print-readably))
                             object)) \] )
    (core/hash-map? object)
      (str \{ (join " " (map (fn [x]
                               (pr-str x print-readably))
                             (flatten (into [] object)))) \} )
    (fn? object)
      (str "#<function " (str object) ">")
    :else
      (throw (ex-info "Don't know how to print this" {:object object}))))
