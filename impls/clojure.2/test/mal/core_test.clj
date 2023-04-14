(ns mal.core-test
  (:require [clojure.test :refer [deftest is]]
            [mal.core :as core]
            [mal.environ :as environ]
            [mal.test.utils :refer [is-list? is-vector?]])
  (:import [mal.types Function]))

(defn sym$ [name]
  (core/symbol name))

(defn qq$ [form]
  (list (sym$ "quasiquote") form))

(defn unq$ [form]
  (list (sym$ "unquote") form))

(defn def$ [name value]
  (list (sym$ "def!") (sym$ name) value))

(defn let$ [bindings body]
  (list (sym$ "let*") bindings body))

(defn if$ [pred then else]
  (list (sym$ "if") pred then else))

(defn fn$ [params body]
  (list (sym$ "fn*") params body))

(defn defmacro$ [name value]
  (list (sym$ "defmacro!") (sym$ name) value))

(def basic-env
  (environ/make nil
    {(core/symbol "+") +
     (core/symbol "-") -
     (core/symbol "=") =
     (core/symbol "list") list
     (core/symbol "cons") core/cons
     (core/symbol "concat") core/concat
     (core/symbol "vec") core/vec}))

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
  (let [env (environ/make basic-env {})]
    (is (= (core/eval (list (core/symbol "def!")
                            (core/symbol "a")
                            (list (core/symbol "+") 1 2))
                      env)
           3))
    (is (= (deref env) {:outer basic-env :table {(core/symbol "a") 3}})))
  (let [env (environ/make basic-env {})]
    (is (= (core/eval
             (list (core/symbol "def!")
               (core/symbol "a")
               (list (core/symbol "list") 1 2))
             env)
           (list 1 2)))
    (is (= (deref env) {:outer basic-env
                        :table {(core/symbol "a") (list 1 2)}}))))

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
  (let [env (environ/make basic-env {})]
    (is (= (core/eval (list (core/symbol "let*")
                            (list (core/symbol "a") 1
                                  (core/symbol "b") 2)
                            (list (core/symbol "+") (core/symbol "a")
                                  (core/symbol "b")))
                      env)
           3))
    (is (= (deref env) {:outer basic-env :table {}})))
  (let [env (environ/make basic-env {})]
    (is (= (core/eval (list (core/symbol "let*")
                            (list (core/symbol "a") 1
                                  (core/symbol "b")
                                    (list (core/symbol "+")
                                          (core/symbol "a")
                                          1)
                                  (core/symbol "c")
                                    (list (core/symbol "+")
                                          (core/symbol "a")
                                          (core/symbol "b")))
                            (core/symbol "c"))
                      env)
           3))))

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

(deftest eval-fn
  (is (instance? Function
        (core/eval (list (core/symbol "fn*") '() 42)
                   (environ/make nil {}))))
  (is (= (core/eval (list (list (core/symbol "fn*") '() 42))
                    (environ/make nil {}))
         42))
  (is (= (core/eval (list (list (core/symbol "fn*")
                                (list (core/symbol "x"))
                                (core/symbol "x"))
                          42)
                    (environ/make nil {}))
         42))
  (let [env (environ/make nil {(core/symbol "x") 42})]
    (is (= (core/eval (list (list (core/symbol "fn*")
                                  (list (core/symbol "x")))
                            42)
                      env)
           nil))
    (is (= (deref env) {:outer nil :table {(core/symbol "x") 42}})))
  (let [env (environ/make nil {(core/symbol "x") 42})]
    (is (= (core/eval (list (list (core/symbol "fn*")
                                  (list (core/symbol "x"))
                                  (core/symbol "x"))
                            43)
                      env)
           43))
    (is (= (deref env) {:outer nil :table {(core/symbol "x") 42}})))
  (is (= (core/eval (list (list (core/symbol "fn*")
                                (list (core/symbol "x") (core/symbol "y"))
                                (list (core/symbol "+")
                                      (core/symbol "x")
                                      (core/symbol "y"))
                                )
                          1 2)
                    (environ/make basic-env {}))
         3))
  (is (= (core/eval (list (list (core/symbol "fn*")
                                (list (core/symbol "&") (core/symbol "xs"))
                                (core/symbol "xs"))
                          1 2 3 4 5)
                    (environ/make nil {}))
         [1 2 3 4 5])))

(deftest core-pr-str
  (is (= (core/pr-str) ""))
  (is (= (core/pr-str 1) "1"))
  (is (= (core/pr-str 1 2 3) "1 2 3"))
  (is (= (core/pr-str "a\"b") "\"a\\\"b\"")))

(deftest core-str
  (is (= (core/str) ""))
  (is (= (core/str 1) "1"))
  (is (= (core/str 1 2 3) "123"))
  (is (= (core/str "a\"b") "a\"b")))

(deftest eval-tail-call-optimization
  (let [env (environ/make basic-env {})]
    (core/eval
      (list (core/symbol "def!") (core/symbol "foo")
        (list (core/symbol "fn*") (list (core/symbol "n"))
          (list (core/symbol "if")
            (list (core/symbol "=") (core/symbol "n") 0)
            0
            (list (core/symbol "bar")
              (list (core/symbol "-") (core/symbol "n") 1)))))
      env)
    (core/eval
      (list (core/symbol "def!") (core/symbol "bar")
        (list (core/symbol "fn*") (list (core/symbol "n"))
          (list (core/symbol "if")
            (list (core/symbol "=") (core/symbol "n") 0)
            0
            (list (core/symbol "foo")
              (list (core/symbol "-") (core/symbol "n") 1)))))
      env)
    (is (= (core/eval (list (core/symbol "foo") 10000) env) 0))))

(deftest core-read-string
  (is (= (core/read-string "(foo [1 \"2\"] {abc :def})")
         (list (core/symbol "foo") [1 "2"]
               {(core/symbol "abc") (core/keyword "def")}))))

(deftest core-atom
  (is (core/atom? (core/atom 42)))
  (is (= (core/deref (core/atom 42)) 42))
  (is (= (core/deref (core/atom [1 2 3])) [1 2 3]))
  (let [a (core/atom 42)]
    (core/reset! a 43)
    (is (= (core/deref a) 43))
    (core/swap! a + 1 2 3)
    (is (= (core/deref a) 49)))
  (let [a (core/atom 42)
        foo (core/eval (core/read-string "(fn* [a b c] (+ a b c))")
                       basic-env)]
    (core/swap! a foo 1 2)
    (is (= (core/deref a) 45))))

(deftest core-cons
  (is-list? (core/cons 1 (list 2 3)) (list 1 2 3))
  (is-list? (core/cons 1 []) (list 1))
  (is-list? (core/cons [1] [2 3]) (list [1] 2 3))
  (is-list? (core/cons 1 [2 3]) (list 1 2 3)))

(deftest core-concat
  (is-list? (core/concat) (list))
  (is-list? (core/concat (list 1 2) (list 3 4)) (list 1 2 3 4))
  (is-list? (core/concat (list 1 2) (list 3 4) (list 5 6))
            (list 1 2 3 4 5 6))
  (is-list? (core/concat (list 1 2 3) (list 4 5 6) (list 7 8 9))
            (list 1 2 3 4 5 6 7 8 9))
  (is-list? (core/concat [1 2] (list 3 4) [5 6]) (list 1 2 3 4 5 6))
  (is-list? (core/concat (list 1 2) [3 4] (list 5 6)) (list 1 2 3 4 5 6))
  (is-list? (core/concat [1 2]) (list 1 2)))

(deftest core-list?
  (is (core/list? (list)))
  (is (core/list? (list 1 2 3)))
  (is (not (core/list? []))))

(deftest eval-quote
  (is (= (core/eval
           (list (core/symbol "quote") 42)
           (environ/make nil {}))
         42))
  (is (= (core/eval
           (list (core/symbol "quote") (list 1 2 3))
           (environ/make nil {}))
         (list 1 2 3)))
  (is (= (core/eval
           (list (core/symbol "quote") (core/symbol "foo"))
           (environ/make nil {}))
         (core/symbol "foo")))
  (is (= (core/eval
           (list (core/symbol "quote") (list (core/symbol "quote") 42))
           (environ/make nil {}))
         (list (core/symbol "quote") 42)))
  (is (= (core/eval
           (list (core/symbol "quote") [1 2 3])
           (environ/make nil {}))
         [1 2 3]))
  (is (= (core/eval
           (list (core/symbol "quote")
                 {(core/keyword "a") 1
                  (core/keyword "b") 2})
           (environ/make nil {}))
         {(core/keyword "a") 1
          (core/keyword "b") 2})))

(deftest core-quasiquote
  (is (= (core/quasiquote 42) 42))
  (is (= (core/quasiquote (core/symbol "foo"))
         (list (core/symbol "quote") (core/symbol "foo"))))
  (is (= (core/quasiquote (list)) (list)))
  (is (= (core/quasiquote (list (core/symbol "unquote") 42)) 42))
  (is (= (core/quasiquote (list 42)) (list (core/symbol "cons") 42 '())))
  (is (= (core/quasiquote (list 1 2))
         (list (core/symbol "cons") 1 (list (core/symbol "cons") 2 '()))))
  (is (= (core/quasiquote
           (list (list (core/symbol "splice-unquote")
                       (core/symbol "foo"))))
         (list (core/symbol "concat") (core/symbol "foo") '())))
  (is (= (core/quasiquote
           (list 42
                 (list (core/symbol "splice-unquote")
                       (core/symbol "foo"))))
         (list (core/symbol "cons") 42
           (list (core/symbol "concat") (core/symbol "foo") '()))))
  (is (= (core/quasiquote
           (list (list (core/symbol "splice-unquote")
                       (core/symbol "foo"))
                 42))
         (list (core/symbol "concat") (core/symbol "foo")
               (list (core/symbol "cons") 42 '())))))

(deftest eval-quasiquote
  (let [env (environ/make basic-env {(core/symbol "x") 42
                                     (core/symbol "l") (list 1 2 3)})
        eval-qq (fn [form]
                  (core/eval
                    (list (core/symbol "quasiquote") form)
                    env))]
    (is (= (eval-qq 42) 42))
    (is (= (eval-qq (core/symbol "x")) (core/symbol "x")))
    (is (= (eval-qq (list)) (list)))
    (is (= (eval-qq (list 1 2 3)) (list 1 2 3)))
    (is (= (eval-qq (list (core/symbol "unquote") 42)) 42))
    (is (= (eval-qq {"a" (core/symbol "b")}) {"a" (core/symbol "b")}))
    (is-vector?
      (eval-qq [(core/symbol "a")
               []
               (core/symbol "b")
               [(core/symbol "c")]
               (core/symbol "d")
               [(core/symbol "e") (core/symbol "f")]
               (core/symbol "g")])
      [(core/symbol "a")
       []
       (core/symbol "b")
       [(core/symbol "c")]
       (core/symbol "d")
       [(core/symbol "e") (core/symbol "f")]
       (core/symbol "g")])
    (is (= (eval-qq (list (core/symbol "unquote") (core/symbol "x"))) 42))
    (is (= (eval-qq (list (list (core/symbol "unquote") (core/symbol "x"))))
           (list 42)))
    (is (= (eval-qq (list (core/symbol "unquote") (core/symbol "l")))
           (list 1 2 3)))
    (is (= (eval-qq (list (list (core/symbol "splice-unquote")
                                (core/symbol "l"))))
           (list 1 2 3)))
    (is (= (eval-qq
             (list 1
                   (list (core/symbol "splice-unquote") (core/symbol "l"))
                   (list (core/symbol "unquote") (core/symbol "x"))))
           (list 1 1 2 3 42)))
    (is-vector?
      (eval-qq
        (vector
          (list 1
                (list (core/symbol "splice-unquote") (core/symbol "l"))
                (list (core/symbol "unquote") (core/symbol "x")))))
      (vector (list 1 1 2 3 42)))
    (is-list?
      (eval-qq
        (list
          (vector 1
                  (list (core/symbol "splice-unquote") (core/symbol "l"))
                  (list (core/symbol "unquote") (core/symbol "x")))))
      (list (vector 1 1 2 3 42)))
    (is (= (eval-qq (list 0 (core/symbol "unquote")))
           (list 0 (core/symbol "unquote"))))
    (is (= (eval-qq (list 0 (core/symbol "splice-unquote")))
           (list 0 (core/symbol "splice-unquote"))))))

(deftest eval-defmacro
  (let [env (environ/make nil {})
        params (list (sym$ "x"))
        body (qq$ (list (list (sym$ "unquote") (sym$ "x"))
                        (list (sym$ "unquote") (sym$ "x"))))]
    (core/eval
      (defmacro$ "dup" (fn$ params body))
      env)
    (is (nil? (-> env deref :outer)))
    (is (= (-> env deref :table count) 1))
    (is (= (-> env
               deref
               :table
               (get (sym$ "dup"))
               (select-keys [:macro? :params :body]))
           {:macro? true
            :params params
            :body body}))))

(deftest core-macroexpand
  (is (= (core/macroexpand 42 (environ/make nil {})) 42))
  (is (= (core/macroexpand (list 1 2) (environ/make nil {})) (list 1 2)))
  (is (= (core/macroexpand
           (list (sym$ "x") 42)
           (environ/make nil {}))
         (list (sym$ "x") 42)))
  (is (= (core/macroexpand
           (list (sym$ "x") 1)
           (environ/make nil {(sym$ "x") 2}))
         (list (sym$ "x") 1)))
  (is (= (core/macroexpand
           (list (sym$ "dup") (sym$ "x"))
           (environ/make nil
              {(sym$ "dup")
                 (core/make-fn* true (environ/make basic-env {})
                   (list (sym$ "arg"))
                   (qq$ (list (unq$ (sym$ "arg"))
                              (unq$ (sym$ "arg")))))}))
         (list (sym$ "x") (sym$ "x")))))

(deftest eval-macro
  (let [env (environ/make basic-env {})]
    (core/eval
      (defmacro$ "unless"
        (fn$ (list (sym$ "pred") (sym$ "then") (sym$ "else"))
          (if$ (sym$ "pred")
            (qq$ (unq$ (sym$ "else")))
            (qq$ (unq$ (sym$ "then"))))))
      env)
    (core/eval
      (list (sym$ "unless") false
        (def$ "a" 1)
        (def$ "b" 2))
      env)
    (is (= (->> env deref :table keys (map :name) sort) ["a" "unless"]))
    (is (= (-> env deref :table (get (sym$ "a"))) 1)))
  (let [env (environ/make basic-env {})]
    (core/eval
      (defmacro$ "just" (fn$ (list (sym$ "x"))
                          (qq$ (unq$ (sym$ "x")))))
      env)
    (is (= (core/eval (list (sym$ "just") 42) env) 42)))
  (let [env (environ/make basic-env {})]
    (core/eval
      (defmacro$ "identity" (fn$ (list (sym$ "x"))
                              (sym$ "x")))
      env)
    (is (= (core/eval
             (let$ (list (sym$ "a") 123)
               (list (sym$ "identity") (sym$ "a")))
             env)
           123))))
