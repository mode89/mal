(ns mal.lexer
  (:refer-clojure :exclude [atom comment keyword newline symbol])
  (:require [mal.parsing :as pa])
  (:import [mal.parsing Value]))

(alias 'core 'mal.core)

(defrecord Token [value line column])
(defrecord Comment [text])

(defn letter? [ch]
  (if (some? ch)
    (java.lang.Character/isLetter ch)
    false))

(defn digit? [ch]
  (if (some? ch)
    (java.lang.Character/isDigit ch)
    false))

(defn run [parser char-stream]
  (let [result (pa/run parser [char-stream 1 1])
        [stream line column] (:state result)]
    (merge
      (if (instance? Value result)
        {:value (:value result)}
        {:error (:message result)})
      {:remainder stream
       :line line
       :column column})))

(def any-char
  (pa/make-parser [[stream line column :as state]]
    (if (empty? stream)
      (pa/->ParseError "any character" state)
      (let [ch (first stream)]
        (pa/->Value ch [(rest stream)
                        (if (= ch \newline) (inc line) line)
                        (if (= ch \newline) 1 (inc column))])))))

(def end-of-stream
  (pa/make-parser [[stream line column :as state]]
    (if (empty? stream)
      (pa/->Value :end-of-stream [nil line column])
      (pa/->ParseError "end of stream" state))))

(defn character [ch]
  (pa/label (str "'" ch "'")
    (pa/try
      (pa/satisfy
        (fn [x] (= x ch))
        any-char))))

(defn one-of [chs]
  (let [chars-set (set (seq chs))]
    (pa/label (str "one of '" chs "' characters")
      (pa/try
        (pa/satisfy
          (fn [ch] (contains? chars-set ch))
          any-char)))))

(def space (character \space))
(def newline (character \newline))
(def tab (character \tab))
(def comma (character \,))
(def double-quote (character \"))
(def backslash (character \\ ))
(def semicolon (character \;))
(def colon (character \:))
(def tilde (character \~))
(def at (character \@))

(def comment
  (pa/let-bind
    [_ semicolon
     text (pa/many any-char :till (pa/choice newline end-of-stream))]
    (pa/return (->Comment (apply str text)))))

(def whitespace
  (pa/label "whitespace"
    (one-of " \n\t,")))

(def whitespaces
  (pa/skip-many
    (pa/choice whitespace comment)))

(def escape-sequence
  (pa/let-bind [_ backslash
                c (pa/label "escape character"
                    (pa/choice
                      (pa/map (character \n) :to \newline)
                      (pa/map (character \t) :to \tab)
                      (character \\ )
                      (character \" )))]
    (pa/return c)))

(def string-literal
  (let [unbalanced-string (pa/let-bind [_ end-of-stream]
                            (pa/fail "unbalanced string"))
        element (pa/let-bind [_ (pa/maybe unbalanced-string)]
                  (pa/choice escape-sequence any-char))
        characters (pa/many element :till double-quote)]
    (pa/let-bind [_ double-quote
                  cs characters]
      (pa/return (apply str cs)))))

(def special-character
  (pa/label "special character"
    (pa/choice
      (pa/try
        (pa/let-bind [_ tilde
                      _ at]
          (pa/return "~@")))
      (one-of "[]{}()'`~^@"))))

(def letter
  (pa/label "letter"
    (pa/try (pa/satisfy letter? any-char))))

(def digit
  (pa/label "digit"
    (pa/try (pa/satisfy digit? any-char))))

(def atom-char
  (pa/choice
    letter
    digit
    (one-of ":*+!-_'?<>=/.&")))

(defn number-from-string [s]
  (try
    (Integer/parseInt s)
    (catch Exception _
      nil)))

(def atom
  (pa/let-bind [id-vector (pa/many atom-char)]
    (let [id (apply str id-vector)
          id0 (first id)]
      (cond
        (= "nil" id)
          (pa/return nil)
        (= "true" id)
          (pa/return true)
        (= "false" id)
          (pa/return false)
        (or (digit? id0)
            (and (or (= \+ id0) (= \- id0))
                 (digit? (second id))))
          (if-some [value (number-from-string id)]
            (pa/return value)
            (pa/fail (str "invalid number: " id)))
        (= \: id0)
          (pa/return (core/keyword (apply str (rest id))))
        :else
          (pa/return (core/symbol id))))))

(def token-types
  [special-character
   string-literal
   atom])

(def token
  (let [value (apply pa/choice token-types)
        parser (pa/let-bind [v value
                             _ whitespaces]
                 (pa/return v))]
    (pa/make-parser [[_ line column :as state]]
        (pa/run
          (pa/map
            (fn [v]
              (->Token v line column))
            parser)
          state))))

(defn tokenize [stream]
  (let [parser (pa/let-bind [_ whitespaces
                             parsed-tokens (pa/many token
                                             :till end-of-stream)]
                 (pa/return parsed-tokens))
        result (run parser stream)]
    (if (contains? result :error)
      (throw (ex-info "Failed to tokenize"
               (select-keys result [:error :line :column])))
      (:value result))))
