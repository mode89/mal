(ns mal.reader
  (:refer-clojure :exclude [atom read-string])
  (:require [mal.core :as core]
            [mal.lexer :as l]
            [mal.parsing :as pa])
  (:import [mal.parsing ParseError Value]))

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
          (or (nil? v)
              (boolean? v)
              (string? v)
              (number? v)
              (core/symbol? v)
              (core/keyword? v)))
        (pa/map
          (fn [t] (:value t))
          any-token)))))

(declare form)

(def list-form
  (let [unbalanced-list (pa/let-bind [_ end-of-stream]
                          (pa/fail "unbalanced list"))
        left-paren (token \( )
        right-paren (token \) )
        element (pa/let-bind [_ (pa/maybe unbalanced-list)]
                  (form))
        elements (pa/many element :till right-paren)
        reverse-elements (pa/map
                           (fn [fs]
                             (into '() (reverse fs)))
                           elements)]
    (pa/let-bind [_ left-paren]
      reverse-elements)))

(def vector-form
  (let [unbalanced-vector (pa/let-bind [_ end-of-stream]
                             (pa/fail "unbalanced vector"))
        left-bracket (token \[)
        right-bracket (token \])
        element (pa/let-bind [_ (pa/maybe unbalanced-vector)]
                  (form))
        elements (pa/many element :till right-bracket)]
    (pa/let-bind [_ left-bracket]
      elements)))

(def hash-map-form
  (let [unbalanced-map (pa/let-bind [_ end-of-stream]
                         (pa/fail "unbalanced map"))
        left-brace (token \{)
        right-brace (token \})
        element (pa/let-bind [_ (pa/maybe unbalanced-map)]
                  (form))
        elements (pa/many element :till right-brace)]
    (pa/let-bind [_ left-brace
                  elems elements]
      (if (even? (count elems))
        (pa/return (into {} (map vec (partition 2 elems))))
        (pa/fail "map literal must contain an even number of forms")))))

(def quote-form
  (pa/let-bind [_ (token \')]
    (pa/map
      (fn [f]
        (list (core/symbol "quote") f))
      (form))))

(def quasiquote-form
  (pa/let-bind [_ (token \`)]
    (pa/map
      (fn [f]
        (list (core/symbol "quasiquote") f))
      (form))))

(def unquote-form
  (pa/let-bind [_ (token \~)]
    (pa/map
      (fn [f]
        (list (core/symbol "unquote") f))
      (form))))

(def splice-unquote-form
  (pa/let-bind [_ (token "~@")]
    (pa/map
      (fn [f]
        (list (core/symbol "splice-unquote") f))
      (form))))

(def deref-form
  (pa/let-bind [_ (token \@)]
    (pa/map
      (fn [f]
        (list (core/symbol "deref") f))
      (form))))

(def with-meta-form
  (pa/let-bind [_ (token \^)
                m (form)
                f (form)]
    (pa/return (list (core/symbol "with-meta") f m))))

(defn form []
  (pa/label "expected a valid form"
    (pa/choice
      list-form
      vector-form
      hash-map-form
      quote-form
      quasiquote-form
      unquote-form
      splice-unquote-form
      deref-form
      with-meta-form
      atom)))

(defn read-string [string]
  (let [tokens (l/tokenize string)
        result (pa/run (form) tokens)]
    (condp instance? result
      Value (:value result)
      ParseError (throw
                   (ex-info "Failed to parse"
                     {:message (:message result)
                      :next-token (->> result :state first (into {}))})))))
