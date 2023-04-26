(ns mal.run-tests
  (:require [eftest.runner :refer [find-tests run-tests]]
            [mal.core-test]
            [mal.lexer-test]
            [mal.parsing-test]
            [mal.printer-test]
            [mal.python.compiler-test]
            [mal.reader-test]
            [mal.types-test]))

(defn -main [& _args]
  (run-tests
    (find-tests
      ['mal.core-test
       'mal.lexer-test
       'mal.parsing-test
       'mal.printer-test
       'mal.python.compiler-test
       'mal.reader-test
       'mal.types-test])))
