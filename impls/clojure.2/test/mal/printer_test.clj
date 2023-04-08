(ns mal.printer-test
  (:require [clojure.test :refer [deftest is]]
            [mal.core :as core]
            [mal.printer :as p]))

(deftest mal-pr-str
  (is (= (p/pr-str 1) "1"))
  (is (= (p/pr-str "1") "\"1\""))
  (is (= (p/pr-str (core/symbol "a")) "a"))
  (is (= (p/pr-str [1 "2" (core/symbol "x")]) "[1 \"2\" x]"))
  (is (= (p/pr-str (list 1 ["2" (core/symbol "some-symbol")]))
         "(1 [\"2\" some-symbol])"))
  (is (= (p/pr-str "abc\"def") "\"abc\\\"def\""))
  (is (= (p/pr-str "a\nb") "\"a\\nb\""))
  (is (= (p/pr-str "a\tb") "\"a\\tb\""))
  (is (= (p/pr-str "a\\b") "\"a\\\\b\""))
  (is (= (p/pr-str {}) "{}"))
  (is (= (p/pr-str {1 2 3 4}) "{1 2 3 4}"))
  (is (= (p/pr-str (core/keyword "some.namespace/some-keyword"))
         ":some.namespace/some-keyword"))
  (is (= (p/pr-str {(core/keyword "a") 1 (core/keyword "b") 2})
         "{:a 1 :b 2}")))
