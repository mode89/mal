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

(def end-of-stream
  (pa/make-parser [tokens]
    (if (empty? tokens)
      (pa/->Value :end-of-stream nil)
      (pa/->ParseError "expected end of stream" tokens))))

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
  (let [unbalanced-list (pa/let-bind [_ end-of-stream]
                          (pa/fail "unbalanced list"))
        left-paren (token \( )
        right-paren (token \) )
        element (pa/let-bind [_ (pa/maybe unbalanced-list)]
                  (form))]
    (pa/let-bind [_ left-paren
                  forms (pa/many element :till right-paren)]
      (pa/return (into '() (reverse forms))))))

(defn vector-form []
  (let [unbalanced-vector (pa/let-bind [_ end-of-stream]
                             (pa/fail "unbalanced vector"))
        left-bracket (token \[)
        right-bracket (token \])
        element (pa/let-bind [_ (pa/maybe unbalanced-vector)]
                  (form))]
    (pa/let-bind [_ left-bracket
                  forms (pa/many element :till right-bracket)]
      (pa/return forms))))

(defn read-string [string]
  (let [tokens (l/tokenize string)
        result (pa/run (form) tokens)]
    (condp instance? result
      Value (:value result)
      ParseError (throw
                   (ex-info "Failed to parse"
                     {:message (:message result)
                      :next-token (->> result :state first (into {}))})))))
