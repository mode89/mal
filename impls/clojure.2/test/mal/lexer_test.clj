(ns mal.lexer-test
  (:refer-clojure :exclude [comment])
  (:require [clojure.string :refer [join]]
            [clojure.test :refer [deftest is]]
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
    (is (= (p [\\ \1]) {:error "escape sequence"
                        :remainder '(\1) :line 1 :column 2}))))

(deftest string-literal
  (let [p (partial l/run l/string-literal)]
    (is (= (p [\" \"]) {:value "" :remainder '() :line 1 :column 3}))
    (is (= (p [\"])
           {:error "string literal" :remainder '() :line 1 :column 2}))
    (is (= (p [\" \1 \"]) {:value "1" :remainder '() :line 1 :column 4}))
    (is (= (p [\" \1 \newline \2 \"])
           {:value "1\n2" :remainder '() :line 2 :column 3}))
    (is (= (p [\" \\ \" \"]) {:value "\"" :remainder '() :line 1 :column 5}))
    (is (= (p [\" \\ \n \"]) {:value "\n" :remainder '() :line 1 :column 5}))
    (is (= (p [\" \1 \\ \" \2 \\ \\ \\ \n \3 \\ \\ \\ \" \4 \"])
           {:value (str \1 \" \2 \\ \newline \3 \\ \" \4)
            :remainder '() :line 1 :column 17}))))

(deftest letter
  (let [p (partial l/run l/letter)]
    (is (= (p "a") {:value \a :remainder '() :line 1 :column 2}))
    (is (= (p "1") {:error "letter" :remainder "1" :line 1 :column 1}))))

(deftest digit
  (let [p (partial l/run l/digit)]
    (is (= (p "1") {:value \1 :remainder '() :line 1 :column 2}))
    (is (= (p "a") {:error "digit" :remainder "a" :line 1 :column 1}))))

(deftest integer
  (let [p (partial l/run l/integer)]
    (is (= (p "1") {:value 1 :remainder '() :line 1 :column 2}))
    (is (= (p "12345") {:value 12345 :remainder '() :line 1 :column 6}))
    (is (= (p "12345a") {:error "invalid number"
                         :remainder '(\a) :line 1 :column 6}))
    (is (= (p "12345)") {:value 12345
                         :remainder '( \) ) :line 1 :column 6}))
    (is (= (p "2147483647") {:value 2147483647
                             :remainder '() :line 1 :column 11}))
    (is (= (p "2147483648") {:error "invalid number"
                             :remainder '() :line 1 :column 11}))))

(deftest symbols
  (let [p (partial l/run l/symbol)]
    (is (= (p "a") {:value (l/->Symbol "a")
                    :remainder '() :line 1 :column 2}))
    (is (= (p "a1") {:value (l/->Symbol "a1")
                     :remainder '() :line 1 :column 3}))
    (is (= (p "*s+o!m-e_s'y?m<b>o=l/n.a1m2e")
           {:value (l/->Symbol "*s+o!m-e_s'y?m<b>o=l/n.a1m2e")
            :remainder '() :line 1 :column 29}))))

(deftest token
  (let [p (partial l/run l/token)]
    (is (= (p "~@ ,\n") {:value (l/->Token "~@" 1 1)
                         :remainder '() :line 2 :column 1}))
    (is (= (p "some-symbol\n\n\t")
           {:value (l/->Token (l/->Symbol "some-symbol") 1 1)
            :remainder '() :line 3 :column 2}))
    (is (= (p "\"some\\\"st\nring\"\t, \n, ")
           {:value (l/->Token "some\"st\nring" 1 1)
            :remainder '() :line 3 :column 3}))
    (is (= (p "1234, ; number\n ,\t") {:value (l/->Token 1234 1 1)
                                       :remainder '() :line 2 :column 4}))
    (is (= (p "42x ") {:error "invalid number",
                       :remainder '(\x \space) :line 1 :column 3}))))

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
          (l/->Token (l/->Symbol "a") 2 2)
          (l/->Token \' 2 4)
          (l/->Token (l/->Symbol "b") 2 5)
          (l/->Token \{ 2 7)
          (l/->Token (l/->Symbol "c") 2 8)
          (l/->Token (l/->Symbol "d") 2 10)
          (l/->Token (l/->Symbol "e") 3 8)
          (l/->Token \[ 3 10)
          (l/->Token (l/->Symbol "f") 3 11)
          (l/->Token (l/->Symbol "g") 3 13)
          (l/->Token \] 3 14)
          (l/->Token "h" 4 8)
          (l/->Token 42 4 12)
          (l/->Token \} 4 14)
          (l/->Token \) 4 15)]))
  (is (= (catch-ex-info (l/tokenize " 42x "))
         ["Failed to tokenize" {:error "invalid number" :line 1 :column 4}])))
