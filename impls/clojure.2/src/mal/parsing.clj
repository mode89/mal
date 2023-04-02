(ns mal.parsing
  (:refer-clojure :exclude [map sequence try]))

(defrecord Value
  [value ;; The value returned by the parser.
   state ;; The remaining state to be parsed.
   ])

(defrecord ParseError
  [message ;; The error message.
   state ;; The remaining state to be parsed.
   ])

(defn parse
  "Parses the given string using the given parser and returns the result of
  the parsing. A parser is a function that takes a state and returns
  a Result record."
  [parser state]
  (parser state))

(defn return
  "Returns a parser that always succeeds with the given value."
  [x]
  (fn [state]
    (->Value x state)))

(defn fail
  "Returns a parser that always fails with the given message."
  [message]
  (fn [state]
    (->ParseError message state)))

(defn label
  "Returns a parser that applies the given parser and then checks if the
  parse was successful. If the parse was successful, the returned parser
  succeeds. Otherwise, the returned parser fails with the given message."
  [message parser]
  (fn [state]
    (let [result (parser state)]
      (if (instance? Value result)
        result
        (->ParseError message (:state result))))))

(defn bind
  "Returns a parser that applies the given function to the value returned
  by the given parser. The function must return a parser. If the given
  parser fails, the returned parser fails."
  [parser f]
  (fn [state]
    (let [result (parser state)]
      (if (instance? Value result)
        (let [parser2 (f (:value result))]
          (parser2 (:state result)))
        result))))

(defn map
  "When two arguments are given, returns a parser that applies the given
  function to the value returned by the given parser.

  When keyword :to is given as the second argument, returns a parser that
  always returns the given value.
  "
  ([f parser]
    (fn [state]
      (let [result (parser state)]
        (if (instance? Value result)
          (->Value (f (:value result)) (:state result))
          result))))
  ([parser _to value]
    (assert (= _to :to) "The second argument must be :to")
    (mal.parsing/map (constantly value) parser)))

(defn satisfy
  "Returns a parser that applies the given parser and then checks if the
  value returned by the parser satisfies the given predicate. If the value
  satisfies the predicate, the returned parser succeeds. Otherwise, the
  returned parser fails."
  [predicate parser]
  (bind parser
    (fn [x]
      (if (predicate x)
        (return x)
        (fail "Does not satisfy predicate")))))

(defn try
  "Returns a parser that behaves like the given parser, except that it
  doesn't consume any input if the given parser fails."
  [parser]
  (fn [state]
    (let [result (parser state)]
      (if (instance? Value result)
        result
        (->ParseError (:message result) state)))))

(defn alt
  "This combinator implements choice. It applies the given parsers in order.
  If the first parser succeeds then the result of the first parser is
  returned. If the first parser fails without consuming any input,
  then the second parser is tried."
  [parser1 parser2]
  (fn [state]
    (let [result1 (parser1 state)]
      (if (instance? Value result1)
        result1
        (if (not= state (:state result1))
          ; The first parser consumed input, so we can't try the second
          result1
          ; The first parser didn't consume input, so we can try the second
          (let [result2 (parser2 state)]
            (if (instance? Value result2)
              ; The second parser succeeded
              result2
              (let [message (if (= state (:state result2))
                              ; The second parser failed without consuming
                              ; any input
                              (str (:message result1) " or "
                                   (:message result2))
                              ; The second parser consumed some input
                              (:message result2))]
                (->ParseError message (:state result2))))))))))

(defn choice
  "Returns a parser that tries each parser in the given sequence. If all
  parsers fail, the returned parser fails."
  [& parsers]
  (reduce alt parsers))

(defn many
  "Returns a parser that applies the given parser zero or more times.
  Returns a vector of the values returned by the given parser.
  If keyword argument :till is given, the parser will stop when
  the given end-parser succeeds."
  ([parser]
    (fn [state0]
      (loop [values []
             state state0]
        (let [result (parser state)]
          (if (instance? Value result)
            (recur (conj values (:value result)) (:state result))
            (if (= state (:state result))
              (->Value values state)
              result))))))
  ([parser _till end-parser]
    (assert (= _till :till) "The second argument must be :till")
    (let [end-marker (Object.)
          end-or-value (alt (map end-parser :to end-marker) parser)]
      (fn [state0]
        (loop [values []
               state state0]
          (let [result (end-or-value state)]
            (if (instance? Value result)
              (if (= (:value result) end-marker)
                (->Value values (:state result))
                (recur (conj values (:value result)) (:state result)))
              result)))))))

(defn skip-many
  "Returns a parser that applies the given parser zero or more times,
  skipping the values returned by the parser."
  [parser]
  (fn [state0]
    (loop [state state0]
      (let [result (parser state)]
        (if (instance? Value result)
          (recur (:state result))
          (->Value nil state))))))

(defn sequence
  "Returns a parser that applies each parser in the given sequence. Returns
  a vector of the values returned by the parsers."
  [parsers]
  (fn [state0]
    (loop [remaining-parsers parsers
           values []
           state state0]
      (if (empty? remaining-parsers)
        (->Value values state)
        (let [parser (first remaining-parsers)
              result (parser state)]
          (if (instance? Value result)
            (recur (rest remaining-parsers)
                   (conj values (:value result))
                   (:state result))
            result))))))

(defmacro let-bind
  "Binds the given bindings and then evaluates the given body. The bindings
  are evaluated in the order they are given. The body is evaluated in the
  order the bindings are given. Can be used to create a parser that binds
  multiple values in sequence."
  [bindings & body]
  (letfn [(recurse [bs]
            (if (seq bs)
              (let [[bname bvalue & rest-bs] bs
                    inner (recurse rest-bs)]
                `(bind ~bvalue (fn [~bname] ~inner)))
              `(do ~@body)))]
    (recurse bindings)))
