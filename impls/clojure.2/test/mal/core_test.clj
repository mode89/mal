(ns mal.core-test
  (:require [clojure.test :refer [deftest is]]
            [mal.core :as core]
            [mal.test.utils :refer [is-list? is-vector?]])
  (:import [mal.types Function]))

(defn sym$ [name]
  (assert (string? name))
  (core/symbol name))

(defn kw$ [name]
  (assert (string? name))
  (core/keyword name))

(defn qq$ [form]
  (list (sym$ "quasiquote") form))

(defn unq$ [form]
  (list (sym$ "unquote") form))

(defn spunq$ [form]
  (list (sym$ "splice-unquote") form))

(defn def$ [name value]
  (assert (string? name))
  (list (sym$ "def!") (sym$ name) value))

(defn let$ [bindings body]
  (list (sym$ "let*") bindings body))

(defn if$
  ([pred then]
    (if$ pred then nil))
  ([pred then else]
    (list (sym$ "if") pred then else)))

(defn fn$
  ([params]
    (list (sym$ "fn*") params))
  ([params body]
    (list (sym$ "fn*") params body)))

(defn defmacro$ [name value]
  (assert (string? name))
  (list (sym$ "defmacro!") (sym$ name) value))

(defn do$ [& forms]
  (apply list (concat [(sym$ "do")] forms)))

(defn cons$ [x xs]
  (list (sym$ "cons") x xs))

(defn concat$ [& xs]
  (apply list (concat [(sym$ "concat")] xs)))

(def basic-env
  (core/env-make nil
    {(core/symbol "+") +
     (core/symbol "-") -
     (core/symbol "=") =
     (core/symbol "list") list
     (core/symbol "cons") core/cons
     (core/symbol "concat") core/concat
     (core/symbol "vec") core/vec}))

(deftest core-eval
  (is (= (core/eval 42 (core/env-make nil {})) 42))
  (is (= (core/eval '() (core/env-make nil {})) '()))
  (is (= (core/eval (sym$ "a")
                    (core/env-make nil {(sym$ "a") 42}))
         42))
  (try (core/eval (sym$ "a") (core/env-make nil {}))
    (catch Throwable ex
      (is (core/object-exception? ex))
      (is (= (core/object-exception-unwrap ex) "'a' not found"))))
  (is (= (core/eval [1 "2" (sym$ "c")]
                    (core/env-make nil {(sym$ "c") [3 4 5]}))
         [1 "2" [3 4 5]]))
  (is (= (core/eval {1 "2" 3 (sym$ "c")}
                    (core/env-make nil {(sym$ "c") [4 5 6]}))
         {1 "2" 3 [4 5 6]}))
  (is (= (core/eval (list (sym$ "foo") 7 (sym$ "bar"))
           (core/env-make nil
             {(sym$ "foo") (fn [x y] (+ x y))
              (sym$ "bar") 42}))
         49)))

(deftest env-make
  (is (= (deref (core/env-make nil {})) {:outer nil :table {}}))
  (is (= (deref (core/env-make nil {(core/symbol "a") 1
                                    (core/symbol "b") 2}))
         {:outer nil :table {(core/symbol "a") 1
                             (core/symbol "b") 2}}))
  (let [outer-env (core/env-make nil {(core/symbol "a") 1
                                      (core/symbol "b") 2})
        env (core/env-make outer-env {(core/symbol "c") 3
                                      (core/symbol "d") 4})]
    (is (= (deref env) {:outer outer-env
                        :table {(core/symbol "c") 3
                                (core/symbol "d") 4}}))
    (is (= (deref outer-env) {:outer nil
                              :table {(core/symbol "a") 1
                                      (core/symbol "b") 2}}))))

(deftest env-set!
  (let [env (core/env-make nil {})]
    (core/env-set! env (core/symbol "a") 1)
    (is (= (deref env) {:outer nil :table {(core/symbol "a") 1}})))
  (let [env (core/env-make nil {(core/symbol "a") 1})]
    (core/env-set! env (core/symbol "a") 2)
    (is (= (deref env) {:outer nil :table {(core/symbol "a") 2}})))
  (let [env (core/env-make nil {(core/symbol "a") 1})]
    (core/env-set! env (core/symbol "b") 2)
    (is (= (deref env) {:outer nil
                        :table {(core/symbol "a") 1
                                (core/symbol "b") 2}})))
  (let [outer-env (core/env-make nil {(core/symbol "a") 1})
        env (core/env-make outer-env {(core/symbol "b") 2})]
    (core/env-set! env (core/symbol "a") 3)
    (is (= (deref env) {:outer outer-env
                        :table {(core/symbol "b") 2
                                (core/symbol "a") 3}}))
    (is (= (deref outer-env) {:outer nil :table {(core/symbol "a") 1}}))))

(deftest env-get
  (let [env (core/env-make nil {(core/symbol "a") 1})]
    (is (= (core/env-get env (core/symbol "a")) 1))
    (try (core/env-get env (core/symbol "b"))
      (catch Throwable ex
        (is (core/object-exception? ex))
        (is (= (core/object-exception-unwrap ex) "'b' not found")))))
  (let [outer-env (core/env-make nil {(core/symbol "a") 1})
        env (core/env-make outer-env {(core/symbol "b") 2})]
    (is (= (core/env-get env (core/symbol "a")) 1))))

(deftest eval-def
  (let [env (core/env-make nil {})]
    (is (= (core/eval (def$ "a" 42) env) 42))
    (is (= (deref env) {:outer nil :table {(sym$ "a") 42}})))
  (let [env (core/env-make basic-env {})]
    (is (= (core/eval (def$ "a" (list (sym$ "+") 1 2)) env) 3))
    (is (= (deref env) {:outer basic-env :table {(sym$ "a") 3}})))
  (let [env (core/env-make basic-env {})]
    (is (= (core/eval (def$ "a" (list (sym$ "list") 1 2)) env)
           (list 1 2)))
    (is (= (deref env) {:outer basic-env :table {(sym$ "a") (list 1 2)}}))))

(deftest eval-let
  (is (= (core/eval (let$ '() 42) (core/env-make nil {})) 42))
  (let [env (core/env-make nil {})]
    (is (= (core/eval (let$ (list (sym$ "a") 42)
                        (sym$ "a"))
                      env)
           42))
    (is (= (deref env) {:outer nil :table {}})))
  (let [env (core/env-make basic-env {})]
    (is (= (core/eval (let$ (list (sym$ "a") 1
                                  (sym$ "b") 2)
                        (list (sym$ "+") (sym$ "a") (sym$ "b")))
                      env)
           3))
    (is (= (deref env) {:outer basic-env :table {}})))
  (let [env (core/env-make basic-env {})]
    (is (= (core/eval (let$ (list (sym$ "a") 1
                                  (sym$ "b") (list (sym$ "+") (sym$ "a") 1)
                                  (sym$ "c") (list (sym$ "+")
                                                   (sym$ "a")
                                                   (sym$ "b")))
                        (sym$ "c"))
                      env)
           3))))

(deftest eval-do
  (is (= (core/eval (do$) (core/env-make nil {})) nil))
  (is (= (core/eval (do$ 42) (core/env-make nil {})) 42))
  (is (= (core/eval (do$ 42 7 9001) (core/env-make nil {})) 9001))
  (let [env (core/env-make nil {})]
    (is (= (core/eval (do$ (def$ "a" 42) (sym$ "a")) env) 42))
    (is (= (deref env) {:outer nil :table {(sym$ "a") 42}}))))

(deftest eval-if
  (is (= (core/eval (if$ true 1 2) (core/env-make nil {})) 1))
  (is (= (core/eval (if$ false 1 2) (core/env-make nil {})) 2))
  (is (= (core/eval (if$ true 1) (core/env-make nil {})) 1))
  (is (= (core/eval (if$ false 1) (core/env-make nil {})) nil))
  (let [env (core/env-make nil {})]
    (is (= (core/eval (if$ true (def$ "a" 1) (def$ "b" 2)) env) 1))
    (is (= (deref env) {:outer nil :table {(sym$ "a") 1}})))
  (let [env (core/env-make nil {})]
    (is (= (core/eval (if$ false (def$ "a" 1) (def$ "b" 2)) env) 2))
    (is (= (deref env) {:outer nil :table {(sym$ "b") 2}}))))

(deftest eval-fn
  (is (instance? Function (core/eval (fn$ '() 42) (core/env-make nil {}))))
  (is (= (core/eval (list (fn$ '() 42)) (core/env-make nil {})) 42))
  (is (= (core/eval (list (fn$ (list (sym$ "x"))
                               (sym$ "x"))
                          42)
                    (core/env-make nil {}))
         42))
  (let [env (core/env-make nil {(sym$ "x") 42})]
    (is (= (core/eval (list (fn$ (list (sym$ "x"))) 42) env) nil))
    (is (= (deref env) {:outer nil :table {(sym$ "x") 42}})))
  (let [env (core/env-make nil {(sym$ "x") 42})]
    (is (= (core/eval (list (fn$ (list (sym$ "x")) (sym$ "x")) 43) env) 43))
    (is (= (deref env) {:outer nil :table {(sym$ "x") 42}})))
  (is (= (core/eval (list (fn$ (list (sym$ "x") (sym$ "y"))
                            (list (sym$ "+") (sym$ "x") (sym$ "y")))
                          1 2)
                    (core/env-make basic-env {}))
         3))
  (is (= (core/eval (list (fn$ (list (sym$ "&") (sym$ "xs")) (sym$ "xs"))
                          1 2 3 4 5)
                    (core/env-make nil {}))
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
  (let [env (core/env-make basic-env {})]
    (core/eval
      (def$ "foo" (fn$ (list (sym$ "n"))
                    (if$ (list (sym$ "=") (sym$ "n") 0)
                      0
                      (list (sym$ "bar") (list (sym$ "-") (sym$ "n") 1)))))
      env)
    (core/eval
      (def$ "bar" (fn$ (list (sym$ "n"))
                    (if$ (list (sym$ "=") (sym$ "n") 0)
                      0
                      (list (sym$ "foo") (list (sym$ "-") (sym$ "n") 1)))))
      env)
    (is (= (core/eval (list (sym$ "foo") 10000) env) 0))))

(deftest core-read-string
  (is (= (core/read-string "(foo [1 \"2\"] {abc :def})")
         (list (sym$ "foo") [1 "2"] {(sym$ "abc") (kw$ "def")}))))

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
           (core/env-make nil {}))
         42))
  (is (= (core/eval
           (list (core/symbol "quote") (list 1 2 3))
           (core/env-make nil {}))
         (list 1 2 3)))
  (is (= (core/eval
           (list (core/symbol "quote") (core/symbol "foo"))
           (core/env-make nil {}))
         (core/symbol "foo")))
  (is (= (core/eval
           (list (core/symbol "quote") (list (core/symbol "quote") 42))
           (core/env-make nil {}))
         (list (core/symbol "quote") 42)))
  (is (= (core/eval
           (list (core/symbol "quote") [1 2 3])
           (core/env-make nil {}))
         [1 2 3]))
  (is (= (core/eval
           (list (core/symbol "quote")
                 {(core/keyword "a") 1
                  (core/keyword "b") 2})
           (core/env-make nil {}))
         {(core/keyword "a") 1
          (core/keyword "b") 2})))

(deftest core-quasiquote
  (is (= (core/quasiquote 42) 42))
  (is (= (core/quasiquote (core/symbol "foo"))
         (list (core/symbol "quote") (core/symbol "foo"))))
  (is (= (core/quasiquote (list)) (list)))
  (is (= (core/quasiquote (list (core/symbol "unquote") 42)) 42))
  (is (= (core/quasiquote (list 42)) (list (core/symbol "cons") 42 '())))
  (is (= (core/quasiquote (list 1 2)) (cons$ 1 (cons$ 2 '()))))
  (is (= (core/quasiquote (list (spunq$ (sym$ "foo"))))
         (concat$ (sym$ "foo") '())))
  (is (= (core/quasiquote (list 42 (spunq$ (sym$ "foo"))))
         (cons$ 42 (concat$ (sym$ "foo") '()))))
  (is (= (core/quasiquote (list (spunq$ (sym$ "foo")) 42))
         (concat$ (sym$ "foo") (cons$ 42 '())))))

(deftest eval-quasiquote
  (let [env (core/env-make basic-env {(sym$ "x") 42
                                      (sym$ "l") (list 1 2 3)})
        eval-qq (fn [form] (core/eval (qq$ form) env))]
    (is (= (eval-qq 42) 42))
    (is (= (eval-qq (sym$ "x")) (sym$ "x")))
    (is (= (eval-qq (list)) (list)))
    (is (= (eval-qq (list 1 2 3)) (list 1 2 3)))
    (is (= (eval-qq (unq$ 42)) 42))
    (is (= (eval-qq {"a" (sym$ "b")}) {"a" (sym$ "b")}))
    (is-vector?
      (eval-qq [(sym$ "a") [] (sym$ "b") [(sym$ "c")] (sym$ "d")
                [(sym$ "e") (sym$ "f")] (sym$ "g")])
      [(sym$ "a") [] (sym$ "b") [(sym$ "c")] (sym$ "d")
       [(sym$ "e") (sym$ "f")] (sym$ "g")])
    (is (= (eval-qq (unq$ (sym$ "x"))) 42))
    (is (= (eval-qq (list (unq$ (sym$ "x")))) (list 42)))
    (is (= (eval-qq (unq$ (sym$ "l"))) (list 1 2 3)))
    (is (= (eval-qq (list (spunq$ (sym$ "l")))) (list 1 2 3)))
    (is (= (eval-qq (list 1 (spunq$ (sym$ "l")) (unq$ (sym$ "x"))))
           (list 1 1 2 3 42)))
    (is-vector?
      (eval-qq (vector (list 1 (spunq$ (sym$ "l")) (unq$ (sym$ "x")))))
      (vector (list 1 1 2 3 42)))
    (is-list?
      (eval-qq (list (vector 1 (spunq$ (sym$ "l")) (unq$ (sym$ "x")))))
      (list (vector 1 1 2 3 42)))
    (is (= (eval-qq (list 0 (sym$ "unquote")))
           (list 0 (sym$ "unquote"))))
    (is (= (eval-qq (list 0 (sym$ "splice-unquote")))
           (list 0 (sym$ "splice-unquote"))))))

(deftest eval-defmacro
  (let [env (core/env-make nil {})
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
  (is (= (core/macroexpand 42 (core/env-make nil {})) 42))
  (is (= (core/macroexpand (list 1 2) (core/env-make nil {})) (list 1 2)))
  (is (= (core/macroexpand
           (list (sym$ "x") 42)
           (core/env-make nil {}))
         (list (sym$ "x") 42)))
  (is (= (core/macroexpand
           (list (sym$ "x") 1)
           (core/env-make nil {(sym$ "x") 2}))
         (list (sym$ "x") 1)))
  (is (= (core/macroexpand
           (list (sym$ "dup") (sym$ "x"))
           (core/env-make nil
              {(sym$ "dup")
                 (core/make-fn* true (core/env-make basic-env {})
                   (list (sym$ "arg"))
                   (qq$ (list (unq$ (sym$ "arg"))
                              (unq$ (sym$ "arg")))))}))
         (list (sym$ "x") (sym$ "x")))))

(deftest eval-macro
  (let [env (core/env-make basic-env {})]
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
  (let [env (core/env-make basic-env {})]
    (core/eval
      (defmacro$ "just" (fn$ (list (sym$ "x"))
                          (qq$ (unq$ (sym$ "x")))))
      env)
    (is (= (core/eval (list (sym$ "just") 42) env) 42)))
  (let [env (core/env-make basic-env {})]
    (core/eval
      (defmacro$ "identity" (fn$ (list (sym$ "x"))
                              (sym$ "x")))
      env)
    (is (= (core/eval
             (let$ (list (sym$ "a") 123)
               (list (sym$ "identity") (sym$ "a")))
             env)
           123))))

(deftest core-nth
  (is (= (core/nth nil 0) nil))
  (is (= (core/nth nil 1) nil))
  (is (thrown? Exception (core/nth (list) 0)))
  (is (thrown? Exception (core/nth (list) 1)))
  (is (= (core/nth (list 42) 0) 42))
  (is (thrown? Exception (core/nth (list 42) 1)))
  (is (thrown? Exception (core/nth [] 0)))
  (is (thrown? Exception (core/nth [] 1)))
  (is (= (core/nth [42] 0) 42))
  (is (thrown? Exception (core/nth [42] 2)))
  (is (thrown? Exception (core/nth 42 0))))

(deftest core-first
  (is (= (core/first nil) nil))
  (is (= (core/first (list)) nil))
  (is (= (core/first (list 42)) 42))
  (is (= (core/first (list 10 11 12)) 10))
  (is (= (core/first []) nil))
  (is (= (core/first [42]) 42))
  (is (= (core/first [10 11 12]) 10))
  (is (thrown? Exception (core/first 42))))

(deftest core-rest
  (is-list? (core/rest nil) (list))
  (is-list? (core/rest (list)) (list))
  (is-list? (core/rest (list 1 2 3)) (list 2 3))
  (is-list? (core/rest []) (list))
  (is-list? (core/rest [10]) (list))
  (is-list? (core/rest [10 11 12]) (list 11 12))
  (is (thrown? Exception (core/rest 42))))
