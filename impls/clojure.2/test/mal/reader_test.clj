(ns mal.reader-test
  (:require [clojure.string :refer [join]]
            [clojure.test :refer [deftest is]]
            [mal.lexer :as l]
            [mal.parsing :as pa]
            [mal.reader :as r]
            [mal.test.utils :refer [catch-ex-info]])
  (:import [mal.parsing ParseError]))

(deftest any-token
  (let [p (partial pa/run r/any-token)]
    (is (= (p []) (pa/->ParseError "token" [])))
    (is (= (p [(l/->Token 1 1 1)])
           (pa/->Value (l/->Token 1 1 1) [])))))

(deftest token
  (let [p (partial pa/run (r/token 42))]
    (is (= (p [(l/->Token 42 1 1)]) (pa/->Value (l/->Token 42 1 1) [])))
    (is (= (p [(l/->Token 43 1 1)])
           (pa/->ParseError "token '42'" [(l/->Token 43 1 1)])))))

(deftest atoms
  (let [p (partial pa/run r/atom)]
    (is (= (p [(l/->Token 42 1 1)]) (pa/->Value 42 [])))
    (is (= (p [(l/->Token \[ 1 1)])
           (pa/->ParseError "atom" [(l/->Token \[ 1 1)])))))

(deftest list-form
  (let [p (partial pa/run (r/list-form))]
    (is (= (p [(l/->Token \( 1 1)
               (l/->Token 1 1 2)
               (l/->Token "2" 1 4)
               (l/->Token \) 1 7)])
           (pa/->Value (list 1 "2") [])))
    (is (= (p [(l/->Token \( 1 1)
               (l/->Token 1 1 2)
               (l/->Token \( 1 4)
               (l/->Token (l/->Symbol "a") 1 5)
               (l/->Token "2" 1 7)
               (l/->Token \) 1 10)
               (l/->Token 3 1 12)
               (l/->Token \) 1 13)])
           (pa/->Value (list 1 (list (l/->Symbol "a") "2") 3) [])))
    (is (instance? ParseError
          (p [(l/->Token \( 1 1)
              (l/->Token 1 1 2)])))))

(deftest vector-form
  (let [p (partial pa/run (r/vector-form))]
    (is (= (p [(l/->Token \[ 1 1)
               (l/->Token 1 1 2)
               (l/->Token "2" 1 4)
               (l/->Token (l/->Symbol "x") 1 8)
               (l/->Token \] 1 9)])
           (pa/->Value [1 "2" (l/->Symbol "x")] [])))))

(deftest read-strings
  (is (= (r/read-string "42") 42))
  (is (= (catch-ex-info (r/read-string "("))
         ["Failed to parse" {:message "unbalanced list" :next-token {}}]))
  (is (= (catch-ex-info (r/read-string "42x"))
         ["Failed to tokenize"
          {:error "invalid number" :line 1 :column 3}]))
  (is (= (r/read-string
           (join \newline
             [" ; some function "
              "(defn foo"
              "  \"A docstring\""
              "  [a b c]"
              "  ; get some of numbers"
              "  (+ a, b, c))"]))
         (list (l/->Symbol "defn")
               (l/->Symbol "foo")
               "A docstring"
               [(l/->Symbol "a") (l/->Symbol "b") (l/->Symbol "c")]
               (list (l/->Symbol "+")
                     (l/->Symbol "a")
                     (l/->Symbol "b")
                     (l/->Symbol "c")))))
  (is (= (catch-ex-info (r/read-string "(1 2"))
         ["Failed to parse" {:message "unbalanced list" :next-token {}}]))
  (is (= (catch-ex-info (r/read-string "[1 2"))
         ["Failed to parse" {:message "unbalanced vector" :next-token {}}])))
