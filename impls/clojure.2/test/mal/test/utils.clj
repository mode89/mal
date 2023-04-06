(ns mal.test.utils)

(defmacro catch-ex-info [& body]
  `(try
     ~@body
     (catch clojure.lang.ExceptionInfo e#
       [(ex-message e#) (ex-data e#)])))
