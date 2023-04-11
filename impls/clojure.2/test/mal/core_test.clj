(ns mal.core-test
  (:require [clojure.test :refer [deftest is]]
            [mal.core :as core]
            [mal.environ :as environ]))

(deftest core-eval
  (is (= (core/eval 42 (environ/make nil {})) 42))
  (is (= (core/eval '() (environ/make nil {})) '()))
  (is (= (core/eval (core/symbol "a")
                    (environ/make nil {(core/symbol "a") 42}))
         42))
  (is (thrown-with-msg? Exception #"Symbol 'a' not found"
        (core/eval (core/symbol "a") (environ/make nil {}))))
  (is (= (core/eval [1 "2" (core/symbol "c")]
                    (environ/make nil {(core/symbol "c") [3 4 5]}))
         [1 "2" [3 4 5]]))
  (is (= (core/eval {1 "2" 3 (core/symbol "c")}
                    (environ/make nil {(core/symbol "c") [4 5 6]}))
         {1 "2" 3 [4 5 6]}))
  (is (= (core/eval (list (core/symbol "foo") 7 (core/symbol "bar"))
           (environ/make nil
             {(core/symbol "foo") (fn [x y] (+ x y))
              (core/symbol "bar") 42}))
         49)))

(deftest eval-def
  (let [env (environ/make nil {})]
    (is (= (core/eval (list (core/symbol "def!")
                            (core/symbol "a")
                            42)
                      env)
           42))
    (is (= (deref env) {:outer nil :table {(core/symbol "a") 42}})))
  (let [env (environ/make nil {(core/symbol "+") +})]
    (is (= (core/eval (list (core/symbol "def!")
                            (core/symbol "a")
                            (list (core/symbol "+") 1 2))
                      env)
           3))
    (is (= (deref env) {:outer nil :table {(core/symbol "a") 3
                                           (core/symbol "+") +}}))))

(deftest eval-let
  (is (= (core/eval (list (core/symbol "let*") '() 42)
                    (environ/make nil {}))
         42))
  (let [env (environ/make nil {})]
    (is (= (core/eval (list (core/symbol "let*")
                            (list (core/symbol "a") 42)
                            (core/symbol "a"))
                      env)
           42))
    (is (= (deref env) {:outer nil :table {}})))
  (let [env (environ/make nil {(core/symbol "+") +})]
    (is (= (core/eval (list (core/symbol "let*")
                            (list (core/symbol "a") 1
                                  (core/symbol "b") 2)
                            (list (core/symbol "+") (core/symbol "a")
                                  (core/symbol "b")))
                      env)
           3))
    (is (= (deref env) {:outer nil :table {(core/symbol "+") +}}))))

(deftest eval-do
  (is (= (core/eval (list (core/symbol "do")) (environ/make nil {})) nil))
  (is (= (core/eval (list (core/symbol "do") 42) (environ/make nil {})) 42))
  (is (= (core/eval
           (list (core/symbol "do") 42 7 9001)
           (environ/make nil {}))
         9001))
  (let [env (environ/make nil {})]
    (is (= (core/eval
             (list (core/symbol "do")
                   (list (core/symbol "def!") (core/symbol "a") 42)
                   (core/symbol "a"))
             env)
           42))
    (is (= (deref env) {:outer nil
                        :table {(core/symbol "a") 42}}))))

(deftest eval-if
  (is (= (core/eval (list (core/symbol "if") true 1 2)
                    (environ/make nil {}))
         1))
  (is (= (core/eval (list (core/symbol "if") false 1 2)
                    (environ/make nil {}))
         2))
  (is (= (core/eval (list (core/symbol "if") true 1)
                    (environ/make nil {}))
         1))
  (is (= (core/eval (list (core/symbol "if") false 1)
                    (environ/make nil {}))
         nil))
  (let [env (environ/make nil {})]
    (is (= (core/eval (list (core/symbol "if") true
                            (list (core/symbol "def!") (core/symbol "a") 1)
                            (list (core/symbol "def!") (core/symbol "b") 2))
                      env)
           1))
    (is (= (deref env) {:outer nil :table {(core/symbol "a") 1}})))
  (let [env (environ/make nil {})]
    (is (= (core/eval (list (core/symbol "if") false
                            (list (core/symbol "def!") (core/symbol "a") 1)
                            (list (core/symbol "def!") (core/symbol "b") 2))
                      env)
           2))
    (is (= (deref env) {:outer nil :table {(core/symbol "b") 2}}))))
