(ns mal.run-tests
  (:require [clojure.test :refer [run-all-tests]]
            [mal.core-test]
            [mal.environ-test]
            [mal.lexer-test]
            [mal.parsing-test]
            [mal.printer-test]
            [mal.reader-test]))

(defn -main [& _args]
  (run-all-tests #"mal.*-test"))
