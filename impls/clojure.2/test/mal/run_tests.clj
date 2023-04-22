(ns mal.run-tests
  (:require [eftest.runner :refer [find-tests run-tests]]
            [mal.core-test]
            [mal.lexer-test]
            [mal.parsing-test]
            [mal.printer-test]
            [mal.reader-test]
            [mal.python.compiler-test]))

(defn -main [& _args]
  (run-tests
    (find-tests
      ['mal.core-test
       'mal.lexer-test
       'mal.parsing-test
       'mal.printer-test
       'mal.reader-test
       'mal.python.compiler-test])))
