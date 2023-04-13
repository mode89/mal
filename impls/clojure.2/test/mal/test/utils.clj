(ns mal.test.utils)

(defmacro catch-ex-info [& body]
  `(try
     ~@body
     (catch clojure.lang.ExceptionInfo e#
       [(ex-message e#) (ex-data e#)])))

(defmacro is-list? [expr value]
  `(do (clojure.test/is (mal.core/list? ~expr))
       (clojure.test/is (= ~value ~expr))))

(defmacro is-vector? [expr value]
  `(do (clojure.test/is (mal.core/vector? ~expr))
       (clojure.test/is (= ~value ~expr))))
