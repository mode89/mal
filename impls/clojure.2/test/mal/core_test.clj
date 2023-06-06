(ns mal.core-test
  (:require [clojure.test :refer [deftest is]]
            [mal.core :as core]
            [mal.reader :as reader]
            [mal.test.utils :refer [is-list? is-vector? mock-eval-context
                                    mock-ns sample-eval-context
                                    quote$ qq$ unq$ spunq$ def$ let$ if$
                                    fn$ defmacro$ do$ concat$ list$ try$
                                    throw$ thrown-with-msg*]])
  (:import [mal.core Function Namespace]))

(def common-bindings
  {'+ +
   '- -
   '* *
   '= =
   'list list
   'cons core/cons
   'concat core/concat
   'vec core/vec})

(deftest core-eval
  (is (= (core/eval (mock-eval-context) nil 42) 42))
  (is (= (core/eval (mock-eval-context) nil '()) '()))
  (is (= (core/eval (mock-eval-context) [{'a 42}] 'a) 42))
  (is (= "'a' not found"
         (try (core/eval (mock-eval-context) [] 'a)
           (catch Throwable ex
             (core/object-exception-unwrap ex)))))
  (is (= [1 "2" [3 4 5]]
         (core/eval (mock-eval-context) [{'c [3 4 5]}]
           [1 "2" 'c])))
  (is (= {1 "2" 3 [4 5 6]}
         (core/eval (mock-eval-context) [{'c [4 5 6]}]
           {1 "2" 3 'c})))
  (is (= 49 (core/eval (mock-eval-context)
                       [{'foo (fn [x y] (+ x y))
                         'bar 42}]
              (list 'foo 7 'bar))))
  (is (thrown-with-msg* #"Can't call this"
        (core/eval (mock-eval-context) [{'foo 42}]
          (list 'foo))))
  (is (thrown-with-msg* #"Can't call this"
        (core/eval (mock-eval-context) [{'foo identity}]
          (list "foo" 42)))))

(deftest resolve-symbol
  (is (= 42 (core/resolve-symbol (mock-eval-context) [{'a 42}] 'a)))
  (is (= 9001 (core/resolve-symbol (mock-eval-context) [{'a 42}
                                                        {'b 9001}]
                'b)))
  (is (= "'c' not found"
         (try
           (core/resolve-symbol
             (mock-eval-context)
             [{'a 42}
              {'b 9001}]
             'c)
           (catch Throwable ex
             (core/object-exception-unwrap ex)))))
  (is (= 1234 (core/resolve-symbol
                (mock-eval-context
                  :ns-registry {"foo" {'c 1234}}
                  :current-ns "foo")
                [{'a 42}
                 {'b 9001}]
                'c)))
  (is (= "'d' not found"
         (try
           (core/resolve-symbol
             (mock-eval-context
               :ns-registry {"foo" {'c 1234}}
               :current-ns "foo")
             [{'a 42}
              {'b 9001}]
             'd)
           (catch Throwable ex
             (core/object-exception-unwrap ex)))))
  (is (re-find #"must be a symbol"
        (try (core/resolve-symbol
               (mock-eval-context
                 :ns-registry {"foo" {'x 42}}
                 :current-ns "foo")
               []
               "x")
          (catch Error e
            (.getMessage e)))))
  (is (re-find #"locals must be a sequential collection"
        (try (core/resolve-symbol
               (mock-eval-context)
               {"a" 1
                "b" 2}
               'b)
          (catch Error e
            (.getMessage e))))))

(deftest eval-def
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (is (= 42 (core/eval ctx [] (def$ "a" 42))))
    (is (= {:ns-registry {"user" {'a 42}}
            :current-ns "user"}
           (sample-eval-context ctx))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (is (= 3 (core/eval ctx [common-bindings]
               (def$ "a" (list '+ 1 2)))))
    (is (= {:ns-registry {"user" {'a 3}}
            :current-ns "user"}
           (sample-eval-context ctx))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (is (= (list 1 2) (core/eval ctx [common-bindings]
                        (def$ "a" (list 'list 1 2)))))
    (is (= {:ns-registry {"user" {'a (list 1 2)}}
            :current-ns "user"}
           (sample-eval-context ctx))))
  (is (re-find #"binding name must be a symbol"
        (try
          (core/eval
            (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")
            []
            (list 'def! "a" 42))
          (catch Error ex
            (.getMessage ex)))))
  (is (re-find #"def! expects 2 arguments"
        (try
          (core/eval
            (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")
            []
            (list 'def! 'x))
          (catch Error e
            (.getMessage e)))))
  (is (re-find #"def! expects 2 arguments"
        (try
          (core/eval
            (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")
            []
            (list 'def! 'x 42 43))
          (catch Error e
            (.getMessage e)))))
  (is (re-find #"no current namespace"
        (try
          (core/eval
            (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns nil)
            []
            (list 'def! 'x 42))
          (catch Error e
            (.getMessage e))))))

(deftest eval-let
  (let [ctx (mock-eval-context)]
    (is (= (core/eval ctx [] (let$ '() 42)) 42))
    (is (= {:ns-registry {} :current-ns nil}
           (sample-eval-context ctx))))
  (let [ctx (mock-eval-context)]
    (is (= 42 (core/eval ctx []
                (let$ (list 'a 42)
                  'a))))
    (is (= {:ns-registry {} :current-ns nil}
           (sample-eval-context ctx))))
  (let [ctx (mock-eval-context)]
    (is (= 3 (core/eval ctx [common-bindings]
               (let$ (list 'a 1
                           'b 2)
                 (list '+ 'a 'b)))))
    (is (= {:ns-registry {} :current-ns nil}
           (sample-eval-context ctx))))
  (let [ctx (mock-eval-context)]
    (is (= 3 (core/eval ctx [common-bindings]
               (let$ (list 'a 1
                           'b (list '+ 'a 1)
                           'c (list '+ 'a 'b))
                 'c))))
    (is (= {:ns-registry {} :current-ns nil}
           (sample-eval-context ctx))))
  (is (re-find #"let\* expects even number of forms in bindings"
        (try (core/eval (mock-eval-context) []
                (let$ (list 'a 1
                            'b)
                  'a))
          (catch Error e (.getMessage e)))))
  (is (re-find #"binding name must be a symbol"
        (try (core/eval (mock-eval-context) []
                (let$ (list 'a 1
                            42 2)
                  'a))
          (catch Error e (.getMessage e))))))

(deftest eval-do
  (let [ctx (mock-eval-context)]
    (is (= nil (core/eval ctx [] (do$))))
    (is (= 42 (core/eval ctx [] (do$ 42))))
    (is (= 9001 (core/eval ctx [] (do$ 42 7 9001))))
    (is (= {:ns-registry {} :current-ns nil}
           (sample-eval-context ctx))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (is (= 42 (core/eval ctx []
                (do$ (def$ "a" 42)
                     'a))))
    (is (= {:ns-registry {"user" {'a 42}}
            :current-ns "user"}
           (sample-eval-context ctx)))))

(deftest eval-if
  (let [ctx (mock-eval-context)]
    (is (= 1 (core/eval ctx [] (if$ true 1 2))))
    (is (= 2 (core/eval ctx [] (if$ false 1 2))))
    (is (= 1 (core/eval ctx [] (if$ true 1))))
    (is (= nil (core/eval ctx [] (if$ false 1)))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (is (= 1 (core/eval ctx []
               (if$ true
                 (def$ "a" 1)
                 (def$ "b" 2)))))
    (is (= {:ns-registry {"user" {'a 1}}
            :current-ns "user"}
           (sample-eval-context ctx))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (is (= 2 (core/eval ctx []
               (if$ false
                 (def$ "a" 1)
                 (def$ "b" 2)))))
    (is (= {:ns-registry {"user" {'b 2}}
            :current-ns "user"}
           (sample-eval-context ctx))))
  (is (re-find #"expects at least 2 arguments"
        (try (core/eval (mock-eval-context) []
                (list 'if true))
          (catch Error e (.getMessage e)))))
  (is (re-find #"expects at most 3 arguments"
        (try (core/eval (mock-eval-context) []
                (list 'if true 1 2 3))
          (catch Error e (.getMessage e))))))

(deftest eval-fn
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (is (instance? Function (core/eval ctx [] (fn$ '() 41))))
    (is (= 42 (core/eval ctx [] (list (fn$ '() 42)))))
    (is (= 43 (core/eval ctx []
                (list (fn$ (list 'x) 'x) 43))))
    (is (= nil (core/eval ctx [{'x 42}]
                 (list (fn$ (list 'x)) 43))))
    (is (= 43 (core/eval ctx [{'x 42}]
                (list (fn$ (list 'x) 'x) 43))))
    (is (= 3 (core/eval ctx [common-bindings]
               (list (fn$ (list 'x 'y)
                          (list '+ 'x 'y))
                     1 2))))
    (is (= [1 2 3 4 5]
           (core/eval ctx []
             (list (fn$ (list '& 'xs) 'xs)
                   1 2 3 4 5)))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" {'+ +}}
              :current-ns "user")]
    (is (= 3 (core/eval ctx []
               (list (fn$ (list 'x 'y)
                          (list '+ 'x 'y))
                     1 2)))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (is (= {:macro? false
            :params (list 'a 'b 'c)
            :body (list '+ 'a 'b 'c)
            :context (core/deref ctx)}
           (select-keys
             (core/eval ctx []
               (fn$ (list 'a 'b 'c)
                 (list '+ 'a 'b 'c)))
             [:macro? :params :body :context]))))
  (is (re-find #"function parameter must be a symbol"
        (try (core/eval (mock-eval-context) []
               (fn$ (list 42)))
          (catch Error e
            (.getMessage e)))))
  (is (re-find #"variadic parameters must be a symbol"
        (try (core/eval (mock-eval-context) []
               (fn$ (list 'x '& 42)))
          (catch Error e
            (.getMessage e)))))
  (is (re-find #"expected only one parameter after &"
        (try (core/eval (mock-eval-context) []
               (fn$ (list 'x '& 'y 'z)))
          (catch Error e
            (.getMessage e))))))

(deftest core-pr-str
  (is (= (core/pr-str) ""))
  (is (= (core/pr-str 1) "1"))
  (is (= (core/pr-str 1 2 3) "1 2 3"))
  (is (= (core/pr-str "a\"b") "\"a\\\"b\"")))

(deftest core-str
  (is (= (core/str) ""))
  (is (= (core/str 1) "1"))
  (is (= (core/str 1 2 3) "123"))
  (is (= (core/str "a\"b") "a\"b"))
  (is (= (core/str "a\"b" 1 2 3) "a\"b123"))
  (is (= "123[4 5 6](7 8 9)" (core/str 1 2 3 [4 5 6] (list 7 8 9))))
  (is (= "" (core/str nil)))
  (is (= "123" (core/str 1 nil 2 nil 3))))

(deftest eval-tail-call-optimization
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (core/eval ctx [common-bindings]
      (def$ "foo"
        (fn$ (list 'n)
          (if$ (list '= 'n 0)
            0
            (list 'bar (list '- 'n 1))))))
    (core/eval ctx [common-bindings]
      (def$ "bar"
        (fn$ (list 'n)
          (if$ (list '= 'n 0)
            0
            (list 'foo (list '- 'n 1))))))
    (is (= 0 (core/eval ctx [common-bindings]
               (list 'foo 10000))))))

(deftest core-atom
  (is (core/atom? (core/atom 42)))
  (is (= (core/deref (core/atom 42)) 42))
  (is (= (core/deref (core/atom [1 2 3])) [1 2 3]))
  (let [a (core/atom 42)]
    (core/reset! a 43)
    (is (= (core/deref a) 43))
    (core/swap! a + 1 2 3)
    (is (= (core/deref a) 49))))

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
  (is (= 42 (core/eval (mock-eval-context) [] (quote$ 42))))
  (is (= (list 1 2 3) (core/eval (mock-eval-context) []
                        (quote$ (list 1 2 3)))))
  (is (= 'foo (core/eval (mock-eval-context) []
                (quote$ 'foo))))
  (is (= (quote$ 42) (core/eval (mock-eval-context) []
                       (quote$ (quote$ 42)))))
  (is (= [1 2 3] (core/eval (mock-eval-context) [] (quote$ [1 2 3]))))
  (is (= {(core/keyword "a") 1
          (core/keyword "b") 2}
         (core/eval (mock-eval-context) []
           (quote$ {(core/keyword "a") 1
                    (core/keyword "b") 2}))))
  (is (re-find #"quote expects 1 argument"
        (try (core/eval (mock-eval-context) [] (list 'quote))
          (catch Error e (.getMessage e)))))
  (is (re-find #"quote expects 1 argument"
        (try (core/eval (mock-eval-context) []
               (list 'quote 'a 'b))
          (catch Error e (.getMessage e))))))

(deftest core-expand-quasiquote
  (is (= 42 (core/expand-quasiquote 42)))
  (is (= (quote$ 'foo) (core/expand-quasiquote 'foo)))
  (is (= (concat$) (core/expand-quasiquote (list))))
  (is (= 42 (core/expand-quasiquote (unq$ 42))))
  (is (= 'foo (core/expand-quasiquote (unq$ 'foo))))
  (is (= (concat$ (list$ 42)) (core/expand-quasiquote (list 42))))
  (is (= (concat$ (list$ 1 2)) (core/expand-quasiquote (list 1 2))))
  (is (= (concat$ 'foo)
         (core/expand-quasiquote (list (spunq$ 'foo)))))
  (is (= (concat$ (list$ 42) 'foo)
         (core/expand-quasiquote (list 42 (spunq$ 'foo)))))
  (is (= (concat$ 'foo (list$ 42))
         (core/expand-quasiquote (list (spunq$ 'foo) 42))))
  (is (re-find #"unquote expects exactly one argument"
        (try (core/expand-quasiquote (list 'unquote 1 2))
          (catch Error e (.getMessage e)))))
  (is (re-find #"splice-unquote used outside of list context"
        (try (core/expand-quasiquote (list 'splice-unquote 1))
          (catch Exception e (core/object-exception-unwrap e)))))
  (is (re-find #"splice-unquote expects exactly one argument"
        (try (core/expand-quasiquote
               (list (list 'splice-unquote 1 2)))
          (catch Error e (.getMessage e))))))

(deftest eval-quasiquote
  (let [eval-qq (fn [form]
                  (core/eval
                    (mock-eval-context
                      :ns-registry {"mal.core" common-bindings
                                    "foo" {}}
                      :current-ns "foo")
                    [{'x 42
                      'l (list 1 2 3)}]
                    (qq$ form)))]
    (is (= (eval-qq 42) 42))
    (is (= (eval-qq 'x) 'x))
    (is (= (eval-qq (list)) (list)))
    (is (= (eval-qq (list 1 2 3)) (list 1 2 3)))
    (is (= (eval-qq (unq$ 42)) 42))
    (is (= (eval-qq {"a" 'b}) {"a" 'b}))
    (is-vector?
      (eval-qq ['a [] 'b ['c] 'd ['e 'f] 'g])
      ['a [] 'b ['c] 'd ['e 'f] 'g])
    (is (= (eval-qq (unq$ 'x)) 42))
    (is (= (eval-qq (list (unq$ 'x))) (list 42)))
    (is (= (eval-qq (unq$ 'l)) (list 1 2 3)))
    (is (= (eval-qq (list (spunq$ 'l))) (list 1 2 3)))
    (is (= (eval-qq (list 1 (spunq$ 'l) (unq$ 'x)))
           (list 1 1 2 3 42)))
    (is-vector?
      (eval-qq (vector (list 1 (spunq$ 'l) (unq$ 'x))))
      (vector (list 1 1 2 3 42)))
    (is-list?
      (eval-qq (list (vector 1 (spunq$ 'l) (unq$ 'x))))
      (list (vector 1 1 2 3 42)))
    (is (= (eval-qq (list 0 'unquote))
           (list 0 'unquote)))
    (is (= (eval-qq (list 0 'splice-unquote))
           (list 0 'splice-unquote))))
  (is (re-find #"quasiquote expects 1 argument"
        (try (core/eval (mock-eval-context) [] (list 'quasiquote))
          (catch Error e (.getMessage e)))))
  (is (re-find #"quasiquote expects 1 argument"
        (try (core/eval (mock-eval-context) []
               (list 'quasiquote 'a 'b))
          (catch Error e (.getMessage e))))))

(deftest eval-defmacro
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")
        ns-bindings (-> ctx core/deref :current-ns :bindings)
        params (list 'x)
        body (qq$ (list (list 'unquote 'x)
                        (list 'unquote 'x)))]
    (core/eval ctx []
      (defmacro$ "dup" (fn$ params body)))
    (is (= ['dup] (-> ns-bindings core/deref keys)))
    (is (= {:macro? true
            :params params
            :body body
            :context (core/deref ctx)}
           (-> ns-bindings
               core/deref
               (get 'dup)
               (select-keys [:macro? :params :body :context])))))
  (is (re-find #"defmacro! expects 2 arguments"
        (try (core/eval (mock-eval-context) []
               (list 'defmacro! 'foo))
          (catch Error e (.getMessage e)))))
  (is (re-find #"defmacro! expects 2 arguments"
        (try (core/eval (mock-eval-context) []
               (list 'defmacro! 'foo
                     (fn$ [] 42) (fn$ [] 43)))
          (catch Error e (.getMessage e)))))
  (is (re-find #"name of macro must be a symbol"
        (try (core/eval (mock-eval-context) []
               (list 'defmacro! 42 (fn$ [] 42)))
          (catch Error e (.getMessage e)))))
  (is (re-find #"last argument to defmacro! must be a function"
        (try (core/eval (mock-eval-context) []
               (list 'defmacro! 'foo 42))
          (catch Error e (.getMessage e)))))
  (is (re-find #"no current namespace"
        (try (core/eval
               (mock-eval-context
                 :ns-registry {"user" nil}
                 :current-ns nil)
               []
               (list 'defmacro! 'foo (fn$ [] 42)))
          (catch Error e (.getMessage e))))))

(deftest core-macroexpand
  (is (= 42 (core/macroexpand (mock-eval-context) [] 42)))
  (is (= (list 1 2) (core/macroexpand (mock-eval-context) [] (list 1 2))))
  (is (= (list 'x 42)
         (core/macroexpand (mock-eval-context) []
           (list 'x 42))))
  (is (= (list 'x 1)
         (core/macroexpand (mock-eval-context) [{'x 2}]
           (list 'x 1))))
  (let [ctx (mock-eval-context
              :ns-registry {"mal.core" common-bindings
                            "user" nil}
              :current-ns "user")]
    (core/eval ctx [common-bindings]
      (defmacro$ "dup"
        (fn$ (list 'arg)
          (qq$ (list (unq$ 'arg)
            (unq$ 'arg))))))
    (is (= (list 'x 'x)
           (core/macroexpand ctx []
             (list 'dup 'x))))))

(deftest eval-macro
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")
        ns-bindings (-> ctx core/deref :current-ns :bindings)]
    (core/eval ctx []
      (defmacro$ "unless"
        (fn$ (list 'pred 'then 'else)
          (if$ 'pred
            (qq$ (unq$ 'else))
            (qq$ (unq$ 'then))))))
    (core/eval ctx []
      (list 'unless false
        (def$ "a" 1)
        (def$ "b" 2)))
    (is (= ["a" "unless"]
           (->> ns-bindings core/deref keys (map core/name) sort)))
    (is (= 1 (-> ns-bindings core/deref (get 'a)))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (core/eval ctx []
      (defmacro$ "just"
        (fn$ (list 'x)
          (qq$ (unq$ 'x)))))
    (is (= 42 (core/eval ctx [] (list 'just 42)))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (core/eval ctx []
      (defmacro$ "identity"
        (fn$ (list 'x)
          'x)))
    (is (= 123 (core/eval ctx []
                 (let$ (list 'a 123)
                   (list 'identity 'a))))))
  (let [ctx (mock-eval-context
              :ns-registry {"mal.core" common-bindings
                            "user" nil}
              :current-ns "user")]
    (core/eval ctx []
      (reader/read-string
        "(defmacro! unless
           (fn* (pred a b)
             `(if ~pred ~b ~a)))"))
    (is (= 8 (core/eval ctx [] (list 'unless true 7 8))))
    (is (= 7 (core/eval ctx [] (list 'unless false 7 8))))))

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

(deftest exceptions
  (is (= 42 (try (core/throw 42)
              (catch Throwable ex
                (core/object-exception-unwrap ex)))))
  (is (= 123 (core/eval (mock-eval-context) [] (try$ 123 "e" 456))))
  (is (= "exc is: 'abc' not found"
         (core/eval (mock-eval-context) [{'str core/str}]
           (try$ 'abc
             "exc" (list 'str "exc is: " 'exc)))))
  (is (= "c2" (core/eval (mock-eval-context) []
                (try$ (do$ (try$ "t1" "e" "c1")
                           (throw$ "e2"))
                  "e" "c2"))))
  (is (= "c2" (core/eval (mock-eval-context) []
              (try$
                (try$
                  (throw$ "e1")
                  "e" (throw$ "e2"))
                "e" "c2"))))
  (is (= "'xyz' not found"
         (try (core/eval (mock-eval-context) [] (try$ 'xyz))
           (catch Throwable ex
             (core/object-exception-unwrap ex)))))
  (is (= "abc" (try (core/throw (Exception. "abc"))
                    (catch Exception e
                      (.getMessage e)))))
  (is (re-find #"try\* expects at most 2 arguments"
        (try (core/eval (mock-eval-context) []
               (list 'try* 'a
                     (list 'catch* 'e 1)
                     (list 'catch* 'e 2)))
          (catch Error e (.getMessage e)))))
  (is (re-find #"try\* expects catch\* form as second argument"
        (try (core/eval (mock-eval-context) []
               (list 'try* 'a
                     (list 'catch 'e 42)))
          (catch Error e (.getMessage e)))))
  (is (re-find #"catch\* expects 3 arguments"
        (try (core/eval (mock-eval-context) []
               (list 'try* 'a
                     (list 'catch* 'e 1 2)))
          (catch Error e (.getMessage e)))))
  (is (re-find #"exception object must be a symbol"
        (try (core/eval (mock-eval-context) []
               (list 'try* 'a
                     (list 'catch* "e" 42)))
          (catch Error e (.getMessage e)))))
  (is (nil? (core/eval (mock-eval-context) [] (list 'try*)))))

(deftest core-apply
  (let [f (fn [a b c]
            (+ a (* b 2) (* c 3)))]
    (is (= 14 (core/apply f (list 1 2 3))))
    (is (= 20 (core/apply f 2 (list 3 4))))
    (is (= 26 (core/apply f 3 4 (list 5))))
    (is (= 14 (core/apply f [1 2 3])))
    (is (= 20 (core/apply f 2 [3 4])))
    (is (= 26 (core/apply f 3 4 [5]))))
  (let [f (core/eval (mock-eval-context) [common-bindings]
            (reader/read-string
              "(fn* [a b c]
                 (+ a (* b 2) (* c 3)))"))]
    (is (= 14 (core/apply f (list 1 2 3))))
    (is (= 20 (core/apply f 2 (list 3 4))))
    (is (= 26 (core/apply f 3 4 (list 5))))
    (is (= 14 (core/apply f [1 2 3])))
    (is (= 20 (core/apply f 2 [3 4])))
    (is (= 26 (core/apply f 3 4 [5]))))
  (is (re-find #"apply expects at least 2 arguments"
        (try (core/apply identity)
          (catch Error e (.getMessage e)))))
  (is (thrown-with-msg* #"last argument to apply must be a sequence"
        (core/apply identity 1 2 3)))
  (is (thrown-with-msg* #"Can't call this" (core/apply "identity" [1 2 3])))
  (is (thrown-with-msg* #"Can't call this" (core/apply 42 [4 5]))))

(deftest core-map
  (is-list? (core/map inc (list 1 2 3)) (list 2 3 4))
  (is-list? (core/map inc [1 2 3]) (list 2 3 4))
  (let [f (core/eval (mock-eval-context) [common-bindings]
            (reader/read-string "(fn* [x] (+ x 1))"))]
    (is-list? (core/map f (list 1 2 3)) (list 2 3 4))
    (is-list? (core/map f [1 2 3]) (list 2 3 4))))

(deftest core-keys
  (is-list? (core/keys {}) (list))
  (is-list? (core/keys {"a" 1}) (list "a")))

(deftest core-vals
  (is-list? (core/vals {}) (list))
  (is-list? (core/vals {"a" 1}) (list 1)))

(deftest core-keyword
  (is (= (core/name (core/keyword "abc")) "abc"))
  (is (= (core/keyword (core/keyword "xyz")) (core/keyword "xyz"))))

(deftest namespaces
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (is (identical? (-> ctx core/deref :current-ns)
                    (core/eval ctx [] '*ns*))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (is (identical?
          (-> ctx core/deref :ns-registry core/deref (get 'user))
          (core/eval ctx [] (list 'in-ns (quote$ 'user))))))
  (is (re-find #"namespace name must be a symbol"
        (try
          (core/eval (mock-eval-context) []
            (list 'in-ns "user"))
          (catch Error ex
            (.getMessage ex)))))
  (let [ctx (mock-eval-context
              :ns-registry {"foo" nil}
              :current-ns "foo")
        foo (-> ctx core/deref :current-ns)
        bar (core/eval ctx [] (list 'in-ns (quote$ 'bar)))]
    (is (instance? Namespace bar))
    (is (= 'bar (:name bar)))
    (is (identical?
          bar
          (-> ctx core/deref :ns-registry core/deref (get 'bar))))
    (is (identical? bar (-> ctx core/deref :current-ns)))
    (is (identical? bar (core/eval ctx [] '*ns*)))
    (is (identical?
          foo
          (-> ctx core/deref :ns-registry core/deref (get 'foo)))))
  (let [ctx (mock-eval-context
              :ns-registry {"foo" {'x 42}
                            "bar" {'x 43
                                   'y 44}}
              :current-ns "foo")]
    (is (= 42 (core/eval ctx [] 'x)))
    (is (= 42 (core/eval ctx [] 'foo/x)))
    (is (= 43 (core/eval ctx [] 'bar/x)))
    (is (re-find #"'y' not found"
          (try (core/eval ctx [] 'y)
            (catch Exception ex
              (core/object-exception-unwrap ex)))))
    (is (= 44 (core/eval ctx [] 'bar/y)))
    (is (re-find #"namespace 'baz' not found"
          (try (core/eval ctx [] 'baz/x)
            (catch Exception ex
              (core/object-exception-unwrap ex)))))
    (is (re-find #"'bar/z' not found"
          (try (core/eval ctx [] 'bar/z)
            (catch Exception ex
              (core/object-exception-unwrap ex))))))
  (is (re-find #"in-ns expects 1 argument"
        (try (core/eval (mock-eval-context) []
               (list 'in-ns))
          (catch Error e (.getMessage e)))))
  (is (re-find #"in-ns expects 1 argument"
        (try (core/eval (mock-eval-context) []
               (list 'in-ns
                     (quote$ 'foo)
                     (quote$ 'bar)))
          (catch Error e (.getMessage e))))))

(deftest functions
  (is (core/fn? (core/->Function false [] nil nil nil)))
  (is (core/fn? (fn [] nil))))

(deftest symbols
  (let [sym (core/symbol "foo")]
    (is (core/symbol? sym))
    (is (= "foo" (core/name sym)))
    (is (= nil (core/namespace sym))))
  (let [sym (core/symbol "foo/bar")]
    (is (core/symbol? sym))
    (is (= "bar" (core/name sym)))
    (is (= "foo" (core/namespace sym))))
  (let [sym (core/symbol "baz" "qux")]
    (is (core/symbol? sym))
    (is (= "baz" (core/namespace sym)))
    (is (= "qux" (core/name sym))))
  (let [sym (core/symbol nil "fred")]
    (is (core/symbol? sym))
    (is (= nil (core/namespace sym)))
    (is (= "fred" (core/name sym))))
  (let [sym (core/symbol "/")]
    (is (core/symbol? sym))
    (is (= "/" (core/name sym)))
    (is (= nil (core/namespace sym))))
  (is (core/simple-symbol? (core/symbol "foo")))
  (is (core/simple-symbol? (core/symbol nil "fred")))
  (is (not (core/simple-symbol? (core/symbol "foo/bar"))))
  (is (not (core/simple-symbol? (core/symbol "baz" "qux"))))
  (is (core/symbol? (core/symbol nil "foo")))
  (is (thrown? Exception
        (core/symbol 42)))
  (is (thrown? Exception
        (core/symbol nil 42)))
  (is (thrown? Exception
        (core/symbol 42 "foo"))))

(deftest core-pr-str*
  (is (= (core/pr-str* 1 true) "1"))
  (is (= (core/pr-str* "1" true) "\"1\""))
  (is (= "a" (core/pr-str* (core/symbol "a") false)))
  (is (= "a" (core/pr-str* (core/symbol "a") true)))
  (is (= "[1 \"2\" x]" (core/pr-str* [1 "2" (core/symbol "x")] true)))
  (is (= "(1 [\"2\" some-symbol])"
         (core/pr-str* (list 1 ["2" (core/symbol "some-symbol")]) true)))
  (is (= "foo/bar" (core/pr-str* (core/symbol "foo/bar") true)))
  (is (= (core/pr-str* "abc\"def" true) "\"abc\\\"def\""))
  (is (= (core/pr-str* "a\nb" true) "\"a\\nb\""))
  (is (= (core/pr-str* "a\tb" true) "\"a\\tb\""))
  (is (= (core/pr-str* "a\\b" true) "\"a\\\\b\""))
  (is (= (core/pr-str* {} true) "{}"))
  (is (= (core/pr-str* {1 2 3 4} true) "{1 2 3 4}"))
  (is (= "{1 [2 3]}" (core/pr-str* {1 [2 3]} true)))
  (is (= ":some.namespace/some-keyword"
         (core/pr-str* (core/keyword "some.namespace/some-keyword") true)))
  (is (= (core/pr-str* {(core/keyword "a") 1 (core/keyword "b") 2} true)
         "{:a 1 :b 2}"))
  (is (= (core/pr-str* nil true) "nil"))
  (is (= (core/pr-str* true true) "true"))
  (is (= (core/pr-str* false true) "false"))
  (is (= (core/pr-str* "abc\"def" false) "abc\"def"))
  (is (= (core/pr-str* "a\nb" false) "a\nb"))
  (is (= (core/pr-str* "a\tb" false) "a\tb"))
  (is (= (core/pr-str* "a\\b" false) "a\\b"))
  (is (= (core/pr-str* (list "a\"b" "c\nd" "e\\f" "g\th") false)
         "(a\"b c\nd e\\f g\th)"))
  (is (= (core/pr-str* ["a\"b" "c\nd" "e\\f" "g\th"] false)
         "[a\"b c\nd e\\f g\th]"))
  (is (= (core/pr-str* {"a\"b" "c\nd" "e\\f" "g\th"} false)
         "{a\"b c\nd e\\f g\th}"))
  (is (re-matches #"#function\[.*\]" (core/pr-str* + false)))
  (is (re-matches #"#macro\[.*\]"
        (core/pr-str*
          (core/eval
            (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")
            []
            (reader/read-string "(defmacro! foo (fn* [x] x))"))
          false)))
  (is (= "(atom 42)" (core/pr-str* (core/atom 42) false)))
  (is (= "#namespace[some.random.namespace.name]"
        (core/pr-str* (mock-ns "some.random.namespace.name" {}) false)))
  (is (re-matches #"#object\[.*\]" (core/pr-str* (Object.) false))))
