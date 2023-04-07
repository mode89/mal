(ns mal.printer-test
  (:require [clojure.test :refer [deftest is]]
            [mal.lexer :as l]
            [mal.printer :as p]))

(deftest mal-pr-str
  (is (= (p/pr-str 1) "1"))
  (is (= (p/pr-str "1") "\"1\""))
  (is (= (p/pr-str (l/->Symbol "a")) "a"))
  (is (= (p/pr-str [1 "2" (l/->Symbol "x")]) "[1 \"2\" x]"))
  (is (= (p/pr-str (list 1 ["2" (l/->Symbol "some-symbol")]))
         "(1 [\"2\" some-symbol])"))
  (is (= (p/pr-str "abc\"def") "\"abc\\\"def\""))
  (is (= (p/pr-str "a\nb") "\"a\\nb\""))
  (is (= (p/pr-str "a\tb") "\"a\\tb\""))
  (is (= (p/pr-str "a\\b") "\"a\\\\b\""))
  (is (= (p/pr-str {}) "{}"))
  (is (= (p/pr-str {1 2 3 4}) "{1 2 3 4}"))
  (is (= (p/pr-str (l/->Keyword "some.namespace/some-keyword"))
         ":some.namespace/some-keyword"))
  (is (= (p/pr-str {(l/->Keyword "a") 1 (l/->Keyword "b") 2})
         "{:a 1 :b 2}")))
