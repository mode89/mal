(ns mal.core-test
  (:require [clojure.test :refer [deftest is]]
            [mal.core :as core]
            [mal.test.utils :refer [is-list?
                                    is-vector?
                                    mock-eval-context
                                    sample-eval-context]])
  (:import [mal.types Function Keyword Namespace]))

(defn sym$ [name]
  (assert (string? name))
  (core/symbol name))

(defn kw$ [name]
  (assert (string? name))
  (core/keyword name))

(defn quote$ [form]
  (list (sym$ "quote") form))

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

(defn try$
  ([expr]
    (list (sym$ "try*") expr))
  ([expr ex catch-expr]
    (assert (string? ex))
    (list (sym$ "try*") expr
      (list (sym$ "catch*") (sym$ ex)
        catch-expr))))

(defn throw$ [obj]
  (list (sym$ "throw*") obj))

(def common-bindings
  {(core/symbol "+") +
   (core/symbol "-") -
   (core/symbol "*") *
   (core/symbol "=") =
   (core/symbol "list") list
   (core/symbol "cons") core/cons
   (core/symbol "concat") core/concat
   (core/symbol "vec") core/vec})

(deftest core-eval
  (is (= (core/eval (mock-eval-context) nil 42) 42))
  (is (= (core/eval (mock-eval-context) nil '()) '()))
  (is (= (core/eval (mock-eval-context) [{(sym$ "a") 42}] (sym$ "a")) 42))
  (is (= "'a' not found"
         (try (core/eval (mock-eval-context) [] (sym$ "a"))
           (catch Throwable ex
             (core/object-exception-unwrap ex)))))
  (is (= [1 "2" [3 4 5]]
         (core/eval (mock-eval-context) [{(sym$ "c") [3 4 5]}]
           [1 "2" (sym$ "c")])))
  (is (= {1 "2" 3 [4 5 6]}
         (core/eval (mock-eval-context) [{(sym$ "c") [4 5 6]}]
           {1 "2" 3 (sym$ "c")})))
  (is (= 49 (core/eval (mock-eval-context)
                       [{(sym$ "foo") (fn [x y] (+ x y))
                         (sym$ "bar") 42}]
              (list (sym$ "foo") 7 (sym$ "bar"))))))

(deftest resolve-symbol
  (is (= 42 (core/resolve-symbol (mock-eval-context) [{(sym$ "a") 42}]
              (sym$ "a"))))
  (is (= 9001 (core/resolve-symbol (mock-eval-context) [{(sym$ "a") 42}
                                                        {(sym$ "b") 9001}]
                (sym$ "b"))))
  (is (= "'c' not found"
         (try
           (core/resolve-symbol
             (mock-eval-context)
             [{(sym$ "a") 42}
              {(sym$ "b") 9001}]
             (sym$ "c"))
           (catch Throwable ex
             (core/object-exception-unwrap ex)))))
  (is (= 1234 (core/resolve-symbol
                (mock-eval-context
                  :ns-registry {"foo" {(sym$ "c") 1234}}
                  :current-ns "foo")
                [{(sym$ "a") 42}
                 {(sym$ "b") 9001}]
                (sym$ "c"))))
  (is (= "'d' not found"
         (try
           (core/resolve-symbol
             (mock-eval-context
               :ns-registry {"foo" {(sym$ "c") 1234}}
               :current-ns "foo")
             [{(sym$ "a") 42}
              {(sym$ "b") 9001}]
             (sym$ "d"))
           (catch Throwable ex
             (core/object-exception-unwrap ex)))))
  (is (re-find #"must be a symbol"
        (try (core/resolve-symbol
               (mock-eval-context
                 :ns-registry {"foo" {(sym$ "x") 42}}
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
               (sym$ "b"))
          (catch Error e
            (.getMessage e))))))

(deftest eval-def
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (is (= 42 (core/eval ctx [] (def$ "a" 42))))
    (is (= {:ns-registry {"user" {(sym$ "a") 42}}
            :current-ns "user"}
           (sample-eval-context ctx))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (is (= 3 (core/eval ctx [common-bindings]
               (def$ "a" (list (sym$ "+") 1 2)))))
    (is (= {:ns-registry {"user" {(sym$ "a") 3}}
            :current-ns "user"}
           (sample-eval-context ctx))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (is (= (list 1 2) (core/eval ctx [common-bindings]
                        (def$ "a" (list (sym$ "list") 1 2)))))
    (is (= {:ns-registry {"user" {(sym$ "a") (list 1 2)}}
            :current-ns "user"}
           (sample-eval-context ctx))))
  (is (re-find #"binding name must be a symbol"
        (try
          (core/eval
            (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")
            []
            (list (sym$ "def!") "a" 42))
          (catch Error ex
            (.getMessage ex)))))
  (is (re-find #"def! expects 2 arguments"
        (try
          (core/eval
            (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")
            []
            (list (sym$ "def!") (sym$ "x")))
          (catch Error e
            (.getMessage e)))))
  (is (re-find #"def! expects 2 arguments"
        (try
          (core/eval
            (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")
            []
            (list (sym$ "def!") (sym$ "x") 42 43))
          (catch Error e
            (.getMessage e)))))
  (is (re-find #"no current namespace"
        (try
          (core/eval
            (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns nil)
            []
            (list (sym$ "def!") (sym$ "x") 42))
          (catch Error e
            (.getMessage e))))))

(deftest eval-let
  (let [ctx (mock-eval-context)]
    (is (= (core/eval ctx [] (let$ '() 42)) 42))
    (is (= {:ns-registry {} :current-ns nil}
           (sample-eval-context ctx))))
  (let [ctx (mock-eval-context)]
    (is (= 42 (core/eval ctx []
                (let$ (list (sym$ "a") 42)
                  (sym$ "a")))))
    (is (= {:ns-registry {} :current-ns nil}
           (sample-eval-context ctx))))
  (let [ctx (mock-eval-context)]
    (is (= 3 (core/eval ctx [common-bindings]
               (let$ (list (sym$ "a") 1
                           (sym$ "b") 2)
                 (list (sym$ "+") (sym$ "a") (sym$ "b"))))))
    (is (= {:ns-registry {} :current-ns nil}
           (sample-eval-context ctx))))
  (let [ctx (mock-eval-context)]
    (is (= 3 (core/eval ctx [common-bindings]
               (let$ (list (sym$ "a") 1
                           (sym$ "b") (list (sym$ "+") (sym$ "a") 1)
                           (sym$ "c") (list (sym$ "+")
                                            (sym$ "a")
                                            (sym$ "b")))
                 (sym$ "c")))))
    (is (= {:ns-registry {} :current-ns nil}
           (sample-eval-context ctx)))))

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
                     (sym$ "a")))))
    (is (= {:ns-registry {"user" {(sym$ "a") 42}}
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
    (is (= {:ns-registry {"user" {(sym$ "a") 1}}
            :current-ns "user"}
           (sample-eval-context ctx))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (is (= 2 (core/eval ctx []
               (if$ false
                 (def$ "a" 1)
                 (def$ "b" 2)))))
    (is (= {:ns-registry {"user" {(sym$ "b") 2}}
            :current-ns "user"}
           (sample-eval-context ctx)))))

(deftest eval-fn
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (is (instance? Function (core/eval ctx [] (fn$ '() 41))))
    (is (= 42 (core/eval ctx [] (list (fn$ '() 42)))))
    (is (= 43 (core/eval ctx []
                (list (fn$ (list (sym$ "x")) (sym$ "x")) 43))))
    (is (= nil (core/eval ctx [{(sym$ "x") 42}]
                 (list (fn$ (list (sym$ "x"))) 43))))
    (is (= 43 (core/eval ctx [{(sym$ "x") 42}]
                (list (fn$ (list (sym$ "x")) (sym$ "x")) 43))))
    (is (= 3 (core/eval ctx [common-bindings]
               (list (fn$ (list (sym$ "x") (sym$ "y"))
                          (list (sym$ "+") (sym$ "x") (sym$ "y")))
                     1 2))))
    (is (= [1 2 3 4 5]
           (core/eval ctx []
             (list (fn$ (list (sym$ "&") (sym$ "xs")) (sym$ "xs"))
                   1 2 3 4 5)))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" {(sym$ "+") +}}
              :current-ns "user")]
    (is (= 3 (core/eval ctx []
               (list (fn$ (list (sym$ "x") (sym$ "y"))
                          (list (sym$ "+") (sym$ "x") (sym$ "y")))
                     1 2)))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (is (= {:macro? false
            :params (list (sym$ "a") (sym$ "b") (sym$ "c"))
            :body (list (sym$ "+") (sym$ "a") (sym$ "b") (sym$ "c"))
            :context (core/deref ctx)}
           (select-keys
             (core/eval ctx []
               (fn$ (list (sym$ "a") (sym$ "b") (sym$ "c"))
                 (list (sym$ "+") (sym$ "a") (sym$ "b") (sym$ "c"))))
             [:macro? :params :body :context]))))
  (is (re-find #"function parameter must be a symbol"
        (try (core/eval (mock-eval-context) []
               (fn$ (list 42)))
          (catch Error e
            (.getMessage e)))))
  (is (re-find #"variadic parameters must be a symbol"
        (try (core/eval (mock-eval-context) []
               (fn$ (list (sym$ "x") (sym$ "&") 42)))
          (catch Error e
            (.getMessage e)))))
  (is (re-find #"expected only one parameter after &"
        (try (core/eval (mock-eval-context) []
               (fn$ (list (sym$ "x") (sym$ "&") (sym$ "y") (sym$ "z"))))
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
        (fn$ (list (sym$ "n"))
          (if$ (list (sym$ "=") (sym$ "n") 0)
            0
            (list (sym$ "bar") (list (sym$ "-") (sym$ "n") 1))))))
    (core/eval ctx [common-bindings]
      (def$ "bar"
        (fn$ (list (sym$ "n"))
          (if$ (list (sym$ "=") (sym$ "n") 0)
            0
            (list (sym$ "foo") (list (sym$ "-") (sym$ "n") 1))))))
    (is (= 0 (core/eval ctx [common-bindings]
               (list (sym$ "foo") 10000))))))

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
  (is (= (sym$ "foo") (core/eval (mock-eval-context) []
                        (quote$ (sym$ "foo")))))
  (is (= (quote$ 42) (core/eval (mock-eval-context) []
                       (quote$ (quote$ 42)))))
  (is (= [1 2 3] (core/eval (mock-eval-context) [] (quote$ [1 2 3]))))
  (is (= {(core/keyword "a") 1
          (core/keyword "b") 2}
         (core/eval (mock-eval-context) []
           (quote$ {(core/keyword "a") 1
                    (core/keyword "b") 2})))))

(deftest core-quasiquote
  (is (= (core/quasiquote 42) 42))
  (is (= (core/quasiquote (sym$ "foo")) (quote$ (sym$ "foo"))))
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
  (let [eval-qq (fn [form]
                  (core/eval
                    (mock-eval-context)
                    [{(sym$ "x") 42
                      (sym$ "l") (list 1 2 3)}
                     common-bindings]
                    (qq$ form)))]
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
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")
        ns-bindings (-> ctx core/deref :current-ns :bindings)
        params (list (sym$ "x"))
        body (qq$ (list (list (sym$ "unquote") (sym$ "x"))
                        (list (sym$ "unquote") (sym$ "x"))))]
    (core/eval ctx []
      (defmacro$ "dup" (fn$ params body)))
    (is (= [(sym$ "dup")]
           (-> ns-bindings core/deref keys)))
    (is (= {:macro? true
            :params params
            :body body
            :context (core/deref ctx)}
           (-> ns-bindings
               core/deref
               (get (sym$ "dup"))
               (select-keys [:macro? :params :body :context]))))))

(deftest core-macroexpand
  (is (= 42 (core/macroexpand (mock-eval-context) [] 42)))
  (is (= (list 1 2) (core/macroexpand (mock-eval-context) [] (list 1 2))))
  (is (= (list (sym$ "x") 42)
         (core/macroexpand (mock-eval-context) []
           (list (sym$ "x") 42))))
  (is (= (list (sym$ "x") 1)
         (core/macroexpand (mock-eval-context) [{(sym$ "x") 2}]
           (list (sym$ "x") 1))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (core/eval ctx [common-bindings]
      (defmacro$ "dup"
        (fn$ (list (sym$ "arg"))
          (qq$ (list (unq$ (sym$ "arg"))
            (unq$ (sym$ "arg")))))))
    (is (= (list (sym$ "x") (sym$ "x"))
           (core/macroexpand ctx []
             (list (sym$ "dup") (sym$ "x")))))))

(deftest eval-macro
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")
        ns-bindings (-> ctx core/deref :current-ns :bindings)]
    (core/eval ctx []
      (defmacro$ "unless"
        (fn$ (list (sym$ "pred") (sym$ "then") (sym$ "else"))
          (if$ (sym$ "pred")
            (qq$ (unq$ (sym$ "else")))
            (qq$ (unq$ (sym$ "then")))))))
    (core/eval ctx []
      (list (sym$ "unless") false
        (def$ "a" 1)
        (def$ "b" 2)))
    (is (= ["a" "unless"]
           (->> ns-bindings core/deref keys (map :name) sort)))
    (is (= 1 (-> ns-bindings core/deref (get (sym$ "a"))))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (core/eval ctx []
      (defmacro$ "just"
        (fn$ (list (sym$ "x"))
          (qq$ (unq$ (sym$ "x"))))))
    (is (= 42 (core/eval ctx [] (list (sym$ "just") 42)))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (core/eval ctx []
      (defmacro$ "identity"
        (fn$ (list (sym$ "x"))
          (sym$ "x"))))
    (is (= 123 (core/eval ctx []
                 (let$ (list (sym$ "a") 123)
                   (list (sym$ "identity") (sym$ "a")))))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" common-bindings}
              :current-ns "user")]
    (core/eval ctx []
      (core/read-string
        "(defmacro! unless
           (fn* (pred a b)
             `(if ~pred ~b ~a)))"))
    (is (= 8 (core/eval ctx [] (list (sym$ "unless") true 7 8))))
    (is (= 7 (core/eval ctx [] (list (sym$ "unless") false 7 8))))))

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
         (core/eval (mock-eval-context) [{(sym$ "str") core/str}]
           (try$ (sym$ "abc")
             "exc" (list (sym$ "str") "exc is: " (sym$ "exc"))))))
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
         (try (core/eval (mock-eval-context) [] (try$ (sym$ "xyz")))
           (catch Throwable ex
             (core/object-exception-unwrap ex)))))
  (is (= "abc" (try (core/throw (Exception. "abc"))
                    (catch Exception e
                      (.getMessage e))))))

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
            (core/read-string
              "(fn* [a b c]
                 (+ a (* b 2) (* c 3)))"))]
    (is (= 14 (core/apply f (list 1 2 3))))
    (is (= 20 (core/apply f 2 (list 3 4))))
    (is (= 26 (core/apply f 3 4 (list 5))))
    (is (= 14 (core/apply f [1 2 3])))
    (is (= 20 (core/apply f 2 [3 4])))
    (is (= 26 (core/apply f 3 4 [5])))))

(deftest core-map
  (is-list? (core/map inc (list 1 2 3)) (list 2 3 4))
  (is-list? (core/map inc [1 2 3]) (list 2 3 4))
  (let [f (core/eval (mock-eval-context) [common-bindings]
            (core/read-string "(fn* [x] (+ x 1))"))]
    (is-list? (core/map f (list 1 2 3)) (list 2 3 4))
    (is-list? (core/map f [1 2 3]) (list 2 3 4))))

(deftest core-keys
  (is-list? (core/keys {}) (list))
  (is-list? (core/keys {"a" 1}) (list "a")))

(deftest core-vals
  (is-list? (core/vals {}) (list))
  (is-list? (core/vals {"a" 1}) (list 1)))

(deftest core-keyword
  (is (instance? Keyword (core/keyword "a")))
  (is (= (:name (core/keyword "abc")) "abc"))
  (is (= (core/keyword (core/keyword "xyz")) (core/keyword "xyz"))))

(deftest namespaces
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (is (identical? (-> ctx core/deref :current-ns)
                    (core/eval ctx [] (sym$ "*ns*")))))
  (let [ctx (mock-eval-context
              :ns-registry {"user" nil}
              :current-ns "user")]
    (is (identical?
          (-> ctx core/deref :ns-registry core/deref (get (sym$ "user")))
          (core/eval ctx [] (list (sym$ "in-ns") (quote$ (sym$ "user")))))))
  (is (re-find #"namespace name must be a symbol"
        (try
          (core/eval (mock-eval-context) []
            (list (sym$ "in-ns") "user"))
          (catch Error ex
            (.getMessage ex)))))
  (let [ctx (mock-eval-context
              :ns-registry {"foo" nil}
              :current-ns "foo")
        foo (-> ctx core/deref :current-ns)
        bar (core/eval ctx [] (list (sym$ "in-ns") (quote$ (sym$ "bar"))))]
    (is (instance? Namespace bar))
    (is (= (sym$ "bar") (:name bar)))
    (is (identical?
          bar
          (-> ctx core/deref :ns-registry core/deref (get (sym$ "bar")))))
    (is (identical? bar (-> ctx core/deref :current-ns)))
    (is (identical? bar (core/eval ctx [] (sym$ "*ns*"))))
    (is (identical?
          foo
          (-> ctx core/deref :ns-registry core/deref (get (sym$ "foo"))))))
  (let [ctx (mock-eval-context
              :ns-registry {"foo" {(sym$ "x") 42}
                            "bar" {(sym$ "x") 43
                                   (sym$ "y") 44}}
              :current-ns "foo")]
    (is (= 42 (core/eval ctx [] (sym$ "x"))))
    (is (= 42 (core/eval ctx [] (sym$ "foo/x"))))
    (is (= 43 (core/eval ctx [] (sym$ "bar/x"))))
    (is (re-find #"'y' not found"
          (try (core/eval ctx [] (sym$ "y"))
            (catch Exception ex
              (core/object-exception-unwrap ex)))))
    (is (= 44 (core/eval ctx [] (sym$ "bar/y"))))
    (is (re-find #"namespace 'baz' not found"
          (try (core/eval ctx [] (sym$ "baz/x"))
            (catch Exception ex
              (core/object-exception-unwrap ex)))))
    (is (re-find #"'bar/z' not found"
          (try (core/eval ctx [] (sym$ "bar/z"))
            (catch Exception ex
              (core/object-exception-unwrap ex)))))))
