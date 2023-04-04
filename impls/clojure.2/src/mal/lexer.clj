(ns mal.lexer
  (:refer-clojure :exclude [atom comment newline symbol])
  (:require [mal.parsing :as pa])
  (:import [mal.parsing Value]))

(defrecord Token [value line column])
(defrecord Comment [text])
(defrecord Symbol [name])

(defn letter? [ch]
  (java.lang.Character/isLetter ch))

(defn digit? [ch]
  (java.lang.Character/isDigit ch))

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
  (pa/label "escape sequence"
    (pa/let-bind [_ backslash
                  c (pa/choice
                      (pa/map (character \n) :to \newline)
                      (pa/map (character \t) :to \tab)
                      (character \\ )
                      (character \" ))]
      (pa/return c))))

(def string-literal
  (pa/label "string literal"
    (pa/let-bind
      [_ double-quote
       cs (pa/many (pa/choice escape-sequence any-char)
            :till double-quote)]
      (pa/return (apply str cs)))))

(def special-character
  (pa/label "special character"
    (pa/choice
      (pa/map (pa/sequence [tilde at]) :to "~@")
      (one-of "[]{}()'`~^@"))))

(def letter
  (pa/label "letter"
    (pa/try (pa/satisfy letter? any-char))))

(def digit
  (pa/label "digit"
    (pa/try (pa/satisfy digit? any-char))))

(def symbol-init-char
  (pa/choice
    letter
    (one-of "*+!-_'?<>=/.")))

(def symbol-rest-char
  (pa/choice
    symbol-init-char
    digit))

(def symbol
  (pa/label "symbol"
    (pa/let-bind [init-char symbol-init-char
                  rest-chars (pa/many symbol-rest-char)]
      (pa/return (->Symbol (apply str (cons init-char rest-chars)))))))

(defn integer-from-string [digits]
  (try
    (Integer/parseInt digits)
    (catch Exception _
      nil)))

(def integer
  (pa/let-bind [digits (pa/many digit)
                _ (pa/not-followed-by symbol-rest-char)]
    (let [number (integer-from-string (apply str digits))]
      (if (some? number)
        (pa/return number)
        (pa/fail "integer")))))

(def number
  (pa/label "number"
    (pa/choice integer)))

(def token-types
  [special-character
   symbol
   string-literal
   number])

(def token
  (pa/make-parser [state]
    (let [parser (pa/let-bind [value (apply pa/choice token-types)
                               _ whitespaces]
                   (pa/return
                     (let [[_ line column] state]
                       (->Token value line column))))]
      (pa/run parser state))))

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
