(ns mal.reader
  (:refer-clojure :exclude [atom read-string])
  (:require [mal.lexer :as l]
            [mal.parsing :as pa])
  (:import [mal.lexer Symbol]
           [mal.parsing ParseError Value]))

(def any-token
  (pa/make-parser [tokens]
    (if (empty? tokens)
      (pa/->ParseError "token" tokens)
      (pa/->Value (first tokens) (rest tokens)))))

(defn token [value]
  (pa/label (str "token '" value "'")
    (pa/try
      (pa/satisfy
        (fn [t] (= (:value t) value))
        any-token))))

(def atom
  (pa/label "atom"
    (pa/try
      (pa/satisfy
        (fn [v]
          (or (string? v)
              (number? v)
              (instance? Symbol v)))
        (pa/map
          (fn [t] (:value t))
          any-token)))))

(declare list-form)
(declare vector-form)

(defn form []
  (pa/choice
    (list-form)
    (vector-form)
    atom))

(defn list-form []
  (pa/let-bind [_ (token \( )
                forms (pa/many (form) :till (token \) ))]
    (pa/return (into '() (reverse forms)))))

(defn vector-form []
  (pa/let-bind [_ (token \[ )
                forms (pa/many (form) :till (token \] ))]
    (pa/return forms)))

(defn read-string [string]
  (let [tokens (l/tokenize string)
        result (pa/run (form) tokens)]
    (condp instance? result
      Value (:value result)
      ParseError (throw
                   (ex-info "Failed to parse"
                     {:expected (:message result)
                      :next-token (first (:state result))})))))
