(ns mal.lexer-test
  (:refer-clojure :exclude [comment])
  (:require [clojure.string :refer [join]]
            [clojure.test :refer [deftest is]]
            [mal.core :as core]
            [mal.lexer :as l]
            [mal.test.utils :refer [catch-ex-info]]))

(deftest any-char
  (let [p (partial l/run l/any-char)]
    (is (= (p "") {:error "any character"
                   :remainder "" :line 1 :column 1}))
    (is (= (p "1") {:value \1 :remainder '() :line 1 :column 2}))
    (is (= (p "\n1") {:value \newline :remainder '(\1) :line 2 :column 1}))))

(deftest end-of-stream
  (let [p (partial l/run l/end-of-stream)]
    (is (= (p "") {:value :end-of-stream :remainder nil :line 1 :column 1}))
    (is (= (p "1") {:error "end of stream"
                    :remainder "1" :line 1 :column 1}))))

(deftest character
  (let [p (partial l/run (l/character \1))]
    (is (= (p "1") {:value \1 :remainder '() :line 1 :column 2}))
    (is (= (p "2") {:error "'1'" :remainder "2" :line 1 :column 1}))))

(deftest one-of
  (let [p (partial l/run (l/one-of "123"))]
    (is (= (p "1") {:value \1 :remainder '() :line 1 :column 2}))
    (is (= (p "2") {:value \2 :remainder '() :line 1 :column 2}))
    (is (= (p "3") {:value \3 :remainder '() :line 1 :column 2}))
    (is (= (p "4") {:error "one of '123' characters"
                    :remainder "4" :line 1 :column 1}))))

(deftest whitespace
  (let [p (partial l/run l/whitespace)]
    (is (= (p " ") {:value \space :remainder '() :line 1 :column 2}))
    (is (= (p ",") {:value \, :remainder '() :line 1 :column 2}))
    (is (= (p "\t") {:value \tab :remainder '() :line 1 :column 2}))
    (is (= (p "\n") {:value \newline :remainder '() :line 2 :column 1}))
    (is (= (p "1") {:error "whitespace"
                    :remainder "1" :line 1 :column 1}))))

(deftest whitespaces
  (let [p (partial l/run l/whitespaces)]
    (is (= (p " ") {:value nil :remainder '() :line 1 :column 2}))
    (is (= (p "  ") {:value nil :remainder '() :line 1 :column 3}))
    (is (= (p "  1") {:value nil :remainder '(\1) :line 1 :column 3}))
    (is (= (p " \n") {:value nil :remainder '() :line 2 :column 1}))
    (is (= (p " \n\t") {:value nil :remainder '() :line 2 :column 2}))
    (is (= (p "\t\n \t,\t ,") {:value nil :remainder '() :line 2 :column 7}))
    (is (= (p ", \n ,\t \n,\t")
           {:value nil :remainder '() :line 3 :column 3}))))

(deftest comment
  (let [p (partial l/run l/comment)]
    (is (= (p ";") {:value (l/->Comment "")
                    :remainder nil :line 1 :column 2}))
    (is (= (p ";\n") {:value (l/->Comment "")
                      :remainder '() :line 2 :column 1}))
    (is (= (p ";1") {:value (l/->Comment "1")
                     :remainder nil :line 1 :column 3}))
    (is (= (p ";12") {:value (l/->Comment "12")
                      :remainder nil :line 1 :column 4}))
    (is (= (p ";123\n") {:value (l/->Comment "123")
                         :remainder '() :line 2 :column 1}))))

(deftest escape-sequence
  (let [p (partial l/run l/escape-sequence)]
    (is (= (p [\\ \n]) {:value \newline :remainder '() :line 1 :column 3}))
    (is (= (p [\\ \t]) {:value \tab :remainder '() :line 1 :column 3}))
    (is (= (p [\\ \"]) {:value \" :remainder '() :line 1 :column 3}))
    (is (= (p "\\\\") {:value \\ :remainder '() :line 1 :column 3}))
    (is (= (p [\\ \1]) {:error "escape character"
                        :remainder '(\1) :line 1 :column 2}))))

(deftest string-literal
  (let [p (partial l/run l/string-literal)]
    (is (= (p [\" \"]) {:value "" :remainder '() :line 1 :column 3}))
    (is (= (p [\"])
           {:error "unbalanced string" :remainder nil :line 1 :column 2}))
    (is (= (p [\" \1 \"]) {:value "1" :remainder '() :line 1 :column 4}))
    (is (= (p [\" \1 \newline \2 \"])
           {:value "1\n2" :remainder '() :line 2 :column 3}))
    (is (= (p [\" \\ \" \"]) {:value "\"" :remainder '() :line 1 :column 5}))
    (is (= (p [\" \\ \n \"]) {:value "\n" :remainder '() :line 1 :column 5}))
    (is (= (p [\" \1 \\ \" \2 \\ \\ \\ \n \3 \\ \\ \\ \" \4 \"])
           {:value (str \1 \" \2 \\ \newline \3 \\ \" \4)
            :remainder '() :line 1 :column 17}))))

(deftest special-character
  (let [p (partial l/run l/special-character)]
    (is (= (p [\~ \@]) {:value "~@" :remainder '() :line 1 :column 3}))
    (is (= (p [\~ \1]) {:value \~ :remainder '(\1) :line 1 :column 2}))))

(deftest letter
  (let [p (partial l/run l/letter)]
    (is (= (p "a") {:value \a :remainder '() :line 1 :column 2}))
    (is (= (p "1") {:error "letter" :remainder "1" :line 1 :column 1}))))

(deftest digit
  (let [p (partial l/run l/digit)]
    (is (= (p "1") {:value \1 :remainder '() :line 1 :column 2}))
    (is (= (p "a") {:error "digit" :remainder "a" :line 1 :column 1}))))

(deftest integer
  (let [p (partial l/run l/atom)]
    (is (= (p "1") {:value 1 :remainder '() :line 1 :column 2}))
    (is (= (p "12345") {:value 12345 :remainder '() :line 1 :column 6}))
    (is (= (p "12345a") {:error "invalid number: 12345a"
                         :remainder '() :line 1 :column 7}))
    (is (= (p "12345)") {:value 12345
                         :remainder '( \) ) :line 1 :column 6}))
    (is (= (p "2147483647") {:value 2147483647
                             :remainder '() :line 1 :column 11}))
    (is (= (p "2147483648") {:error "invalid number: 2147483648"
                             :remainder '() :line 1 :column 11}))
    (is (= (p "-123") {:value -123 :remainder '() :line 1 :column 5}))
    (is (= (p "+456") {:value 456 :remainder '() :line 1 :column 5}))))

(deftest symbols
  (let [p (partial l/run l/atom)]
    (is (= (p "a") {:value (core/symbol "a")
                    :remainder '() :line 1 :column 2}))
    (is (= (p "a1") {:value (core/symbol "a1")
                     :remainder '() :line 1 :column 3}))
    (is (= (p "*s+o!m-e_s'y?m<b>o=l/n.a1:m2e")
           {:value (core/symbol "*s+o!m-e_s'y?m<b>o=l/n.a1:m2e")
            :remainder '() :line 1 :column 30}))))

(deftest keywords
  (let [p (partial l/run l/atom)]
    (is (= (p ":a") {:value (core/keyword "a")
                     :remainder '() :line 1 :column 3}))
    (is (= (p ":some.namespace/keyword-name")
           {:value (core/keyword "some.namespace/keyword-name")
            :remainder '() :line 1 :column 29}))))

(deftest token
  (let [p (partial l/run l/token)]
    (is (= (p "~@ ,\n") {:value (l/->Token "~@" 1 1)
                         :remainder '() :line 2 :column 1}))
    (is (= (p "some-symbol\n\n\t")
           {:value (l/->Token (core/symbol "some-symbol") 1 1)
            :remainder '() :line 3 :column 2}))
    (is (= (p "\"some\\\"st\nring\"\t, \n, ")
           {:value (l/->Token "some\"st\nring" 1 1)
            :remainder '() :line 3 :column 3}))
    (is (= (p "1234, ; number\n ,\t") {:value (l/->Token 1234 1 1)
                                       :remainder '() :line 2 :column 4}))
    (is (= (p "42x ") {:error "invalid number: 42x",
                       :remainder '(\space) :line 1 :column 4}))
    (is (= (p ":another.name.space/some.kw,\n\t")
           {:value (l/->Token (core/keyword
                                "another.name.space/some.kw")
                              1 1)
            :remainder '() :line 2 :column 2}))
    (is (= (p "nil\n") {:value (l/->Token nil 1 1)
                        :remainder '() :line 2 :column 1}))
    (is (= (p "nilA ") {:value (l/->Token (core/symbol "nilA") 1 1)
                       :remainder '() :line 1 :column 6}))
    (is (= (p "true,") {:value (l/->Token true 1 1)
                        :remainder '() :line 1 :column 6}))
    (is (= (p "true?\n") {:value (l/->Token (core/symbol "true?") 1 1)
                         :remainder '() :line 2 :column 1}))
    (is (= (p "false\t") {:value (l/->Token false 1 1)
                          :remainder '() :line 1 :column 7}))
    (is (= (p "false+") {:value (l/->Token (core/symbol "false+") 1 1)
                         :remainder '() :line 1 :column 7}))))

(deftest tokenize
  (is (= (l/tokenize "") []))
  (is (= (l/tokenize " \n\t ") []))
  (is (= (l/tokenize " ,; some comments \n\t,") []))
  (is (= (l/tokenize
           (join \newline
             ["  ; some tokens"
              "(a 'b {c d,"
              "       e [f g]"
              "       \"h\" 42})"]))
         [(l/->Token \( 2 1)
          (l/->Token (core/symbol "a") 2 2)
          (l/->Token \' 2 4)
          (l/->Token (core/symbol "b") 2 5)
          (l/->Token \{ 2 7)
          (l/->Token (core/symbol "c") 2 8)
          (l/->Token (core/symbol "d") 2 10)
          (l/->Token (core/symbol "e") 3 8)
          (l/->Token \[ 3 10)
          (l/->Token (core/symbol "f") 3 11)
          (l/->Token (core/symbol "g") 3 13)
          (l/->Token \] 3 14)
          (l/->Token "h" 4 8)
          (l/->Token 42 4 12)
          (l/->Token \} 4 14)
          (l/->Token \) 4 15)]))
  (is (= (catch-ex-info (l/tokenize " 9001- "))
         ["Failed to tokenize"
          {:error "invalid number: 9001-" :line 1 :column 7}]))
  (is (= (catch-ex-info (l/tokenize "\"abc"))
         ["Failed to tokenize"
          {:error "unbalanced string" :line 1 :column 5}]))
  (is (= (l/tokenize ", -42\n") [(l/->Token -42 1 3)]))
  (is (= (l/tokenize "\n+42,\t") [(l/->Token 42 2 1)]))
  (is (= (catch-ex-info (l/tokenize " -1a "))
         ["Failed to tokenize"
          {:error "invalid number: -1a" :line 1 :column 5}]))
  (is (= (l/tokenize "-a") [(l/->Token (core/symbol "-a") 1 1)]))
  (is (= (l/tokenize "+x") [(l/->Token (core/symbol "+x") 1 1)])))
