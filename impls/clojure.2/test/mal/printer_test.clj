(ns mal.printer-test
  (:require [clojure.test :refer [deftest is]]
            [mal.core :as core]
            [mal.test.utils :refer [mock-eval-context mock-ns]]))

(deftest core-pr-object
  (is (= (core/pr-object 1 true) "1"))
  (is (= (core/pr-object "1" true) "\"1\""))
  (is (= "a" (core/pr-object (core/symbol "a") false)))
  (is (= "a" (core/pr-object (core/symbol "a") true)))
  (is (= "[1 \"2\" x]" (core/pr-object [1 "2" (core/symbol "x")] true)))
  (is (= "(1 [\"2\" some-symbol])"
         (core/pr-object (list 1 ["2" (core/symbol "some-symbol")]) true)))
  (is (= "foo/bar" (core/pr-object (core/symbol "foo/bar") true)))
  (is (= (core/pr-object "abc\"def" true) "\"abc\\\"def\""))
  (is (= (core/pr-object "a\nb" true) "\"a\\nb\""))
  (is (= (core/pr-object "a\tb" true) "\"a\\tb\""))
  (is (= (core/pr-object "a\\b" true) "\"a\\\\b\""))
  (is (= (core/pr-object {} true) "{}"))
  (is (= (core/pr-object {1 2 3 4} true) "{1 2 3 4}"))
  (is (= (core/pr-object (core/keyword "some.namespace/some-keyword") true)
         ":some.namespace/some-keyword"))
  (is (= (core/pr-object {(core/keyword "a") 1 (core/keyword "b") 2} true)
         "{:a 1 :b 2}"))
  (is (= (core/pr-object nil true) "nil"))
  (is (= (core/pr-object true true) "true"))
  (is (= (core/pr-object false true) "false"))
  (is (= (core/pr-object "abc\"def" false) "abc\"def"))
  (is (= (core/pr-object "a\nb" false) "a\nb"))
  (is (= (core/pr-object "a\tb" false) "a\tb"))
  (is (= (core/pr-object "a\\b" false) "a\\b"))
  (is (= (core/pr-object (list "a\"b" "c\nd" "e\\f" "g\th") false)
         "(a\"b c\nd e\\f g\th)"))
  (is (= (core/pr-object ["a\"b" "c\nd" "e\\f" "g\th"] false)
         "[a\"b c\nd e\\f g\th]"))
  (is (= (core/pr-object {"a\"b" "c\nd" "e\\f" "g\th"} false)
         "{a\"b c\nd e\\f g\th}"))
  (is (re-matches #"#function\[.*\]" (core/pr-object + false)))
  (is (re-matches #"#macro\[.*\]"
        (core/pr-object
          (core/eval
            (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")
            []
            (core/read-string "(defmacro! foo (fn* [x] x))"))
          false)))
  (is (= "(atom 42)" (core/pr-object (core/atom 42) false)))
  (is (= "#namespace[some.random.namespace.name]"
        (core/pr-object (mock-ns "some.random.namespace.name" {}) false)))
  (is (re-matches #"#object\[.*\]" (core/pr-object (Object.) false))))
