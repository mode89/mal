(ns mal.python.compiler-test
  (:require [clojure.string :refer [join]]
            [clojure.test :refer [deftest is]]
            [mal.python.compiler :as c]))

(deftest indent
  (is (= (c/indent "a" "b") "ba"))
  (is (= (c/indent "a\nb" "c") "ca\ncb"))
  (is (= (c/indent "a\nb\n" "c") "ca\ncb"))
  (is (= (c/indent "hello\nworld" "  ") "  hello\n  world")))

(deftest emit-assign
  (is (= (c/emit-assign "a" "b") "a = b")))

(deftest emit-call
  (is (= (c/emit-call "foo" ["a" "b"] {}) "foo(a, b)"))
  (is (= (c/emit-call "foo" (list "a" "b") {}) "foo(a, b)"))
  (is (= (c/emit-call "foo" ["a" "b"] {"c" "d" "e" "f"})
         "foo(a, b, c=d, e=f)"))
  (is (= (c/emit-call "foo" ["a" "b"] {"e" "f" "c" "d"})
         "foo(a, b, c=d, e=f)")))

(deftest emit-return
  (is (= (c/emit-return "a") "return a")))

(deftest emit-if
  (is (= (c/emit-if "a" "b" nil nil) (join "\n" ["if a:" "  b"])))
  (is (= (c/emit-if "a" "b" nil "c")
         (join "\n" ["if a:"
                     "  b"
                     "else:"
                     "  c"])))
  (is (= (c/emit-if "a" "b" [["c" "d"]] nil)
         (join "\n" ["if a:"
                     "  b"
                     "elif c:"
                     "  d"])))
  (is (= (c/emit-if "a" "b" [["c" "d"]] "e")
         (join "\n" ["if a:"
                     "  b"
                     "elif c:"
                     "  d"
                     "else:"
                     "  e"])))
  (is (= (c/emit-if "a" "b" [["c" "d"] ["e" "f"]] nil)
         (join "\n" ["if a:"
                     "  b"
                     "elif c:"
                     "  d"
                     "elif e:"
                     "  f"])))
  (is (= (c/emit-if "a" "b" [["c" "d"] ["e" "f"]] "g")
         (join "\n" ["if a:"
                     "  b"
                     "elif c:"
                     "  d"
                     "elif e:"
                     "  f"
                     "else:"
                     "  g"])))
  (is (= (let [then (c/emit-if "a" "b" nil "c")
               else (c/emit-if "d" "e" nil "f")]
           (c/emit-if "g" then nil else))
         (join "\n" ["if g:"
                     "  if a:"
                     "    b"
                     "  else:"
                     "    c"
                     "else:"
                     "  if d:"
                     "    e"
                     "  else:"
                     "    f"]))))

(deftest emit-while
  (is (= (c/emit-while "a" "b") (join "\n" ["while a:" "  b"]))))

(deftest emit-break
  (is (= (c/emit-break) "break")))

(deftest emit-continue
  (is (= (c/emit-continue) "continue")))

(deftest emit-def
  (is (= (c/emit-def "a" ["b"] "c") (join "\n" ["def a(b):" "  c"])))
  (is (= (c/emit-def "a" ["b" "c"] "d") (join "\n" ["def a(b, c):" "  d"]))))

(deftest emit-try
  (is (= (c/emit-try "a" nil nil) (join "\n" ["try:" "  a"])))
  (is (= (c/emit-try "a" nil "b")
         (join "\n" ["try:"
                     "  a"
                     "finally:"
                     "  b"])))
  (is (= (c/emit-try "a" [["b" nil "c"]] nil)
         (join "\n" ["try:"
                     "  a"
                     "except b:"
                     "  c"])))
  (is (= (c/emit-try "a" [["b" "c" "d"]] nil)
         (join "\n" ["try:"
                     "  a"
                     "except b as c:"
                     "  d"])))
  (is (= (c/emit-try "a" [["b" "c" "d"] ["e" nil "f"]] nil)
         (join "\n" ["try:"
                     "  a"
                     "except b as c:"
                     "  d"
                     "except e:"
                     "  f"])))
  (is (= (c/emit-try "a" [["b" nil "c"] ["d" "e" "f"]] "g")
         (join "\n" ["try:"
                     "  a"
                     "except b:"
                     "  c"
                     "except d as e:"
                     "  f"
                     "finally:"
                     "  g"]))))
