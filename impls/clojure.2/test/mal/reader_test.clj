(ns mal.reader-test
  (:require [clojure.string :refer [join]]
            [clojure.test :refer [deftest is]]
            [mal.core :as core]
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
  (let [p (partial pa/run r/list-form)]
    (is (= (p [(l/->Token \( 1 1)
               (l/->Token 1 1 2)
               (l/->Token "2" 1 4)
               (l/->Token \) 1 7)])
           (pa/->Value (list 1 "2") [])))
    (is (= (p [(l/->Token \( 1 1)
               (l/->Token 1 1 2)
               (l/->Token \( 1 4)
               (l/->Token (core/symbol "a") 1 5)
               (l/->Token "2" 1 7)
               (l/->Token \) 1 10)
               (l/->Token 3 1 12)
               (l/->Token \) 1 13)])
           (pa/->Value (list 1 (list (core/symbol "a") "2") 3) [])))
    (is (instance? ParseError
          (p [(l/->Token \( 1 1)
              (l/->Token 1 1 2)])))))

(deftest vector-form
  (let [p (partial pa/run r/vector-form)]
    (is (= (p [(l/->Token \[ 1 1)
               (l/->Token 1 1 2)
               (l/->Token "2" 1 4)
               (l/->Token (core/symbol "x") 1 8)
               (l/->Token \] 1 9)])
           (pa/->Value [1 "2" (core/symbol "x")] [])))))

(deftest read-strings
  (is (= (r/read-string "42") 42))
  (is (= (catch-ex-info (r/read-string "("))
         ["Failed to parse" {:message "unbalanced list" :next-token {}}]))
  (is (= (catch-ex-info (r/read-string "123+"))
         ["Failed to tokenize"
          {:error "invalid number: 123+" :line 1 :column 5}]))
  (is (= (r/read-string
           (join \newline
             [" ; some function "
              "(defn foo"
              "  \"A docstring\""
              "  [a b c]"
              "  ; get some of numbers"
              "  (+ a, b, c))"]))
         (list (core/symbol "defn")
               (core/symbol "foo")
               "A docstring"
               [(core/symbol "a") (core/symbol "b") (core/symbol "c")]
               (list (core/symbol "+")
                     (core/symbol "a")
                     (core/symbol "b")
                     (core/symbol "c")))))
  (is (= (r/read-string "{}") {}))
  (is (= (r/read-string "{1 2}") {1 2}))
  (is (= (catch-ex-info (r/read-string "{1 2"))
         ["Failed to parse" {:message "unbalanced map" :next-token {}}]))
  (is (= (catch-ex-info (r/read-string "{1 2 3}"))
         ["Failed to parse"
          {:message "map literal must contain an even number of forms"
           :next-token {}}]))
  (is (= (catch-ex-info (r/read-string "(1 2"))
         ["Failed to parse" {:message "unbalanced list" :next-token {}}]))
  (is (= (catch-ex-info (r/read-string "[1 2"))
         ["Failed to parse" {:message "unbalanced vector" :next-token {}}]))
  (is (= (r/read-string "'1") (list (core/symbol "quote") 1)))
  (is (= (r/read-string "'(1 \"2\" a)")
         (list (core/symbol "quote") (list 1 "2" (core/symbol "a")))))
  (is (= (r/read-string "`1") (list (core/symbol "quasiquote") 1)))
  (is (= (r/read-string "`(1 \"2\" a)")
         (list (core/symbol "quasiquote") (list 1 "2" (core/symbol "a")))))
  (is (= (r/read-string "~1") (list (core/symbol "unquote") 1)))
  (is (= (r/read-string "~(1 \"2\" a)")
         (list (core/symbol "unquote") (list 1 "2" (core/symbol "a")))))
  (is (= (r/read-string "~@(1 \"2\" a)")
         (list (core/symbol "splice-unquote")
               (list 1 "2" (core/symbol "a")))))
  (is (= (r/read-string ",\n:some.namespace/some-keyword,; comment\n")
         (core/keyword "some.namespace/some-keyword")))
  (is (= (r/read-string "@a")
         (list (core/symbol "deref") (core/symbol "a"))))
  (is (= (r/read-string "^{\"a\" 1} [1 2 3]")
         (list (core/symbol "with-meta") [1 2 3] {"a" 1})))
  (is (= (r/read-string "[1 nil {:a true :b [false]}]")
         [1 nil {(core/keyword "a") true (core/keyword "b") [false]}])))

(deftest read-string*
  (is (= [(list (core/symbol "a") (core/keyword "d") 3)
          [4 (core/symbol "b") (core/keyword "e")]
          {7 (core/keyword "f") (core/symbol "c") 10}]
         (r/read-string* "(a :d 3)\n[4 b :e]\n{7 :f c 10}"))))
