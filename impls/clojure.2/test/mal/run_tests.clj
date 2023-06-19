(in-ns 'mal.run-tests)

(clojure.core/load "clojure/core")

(clojure.core/require
  '[eftest.runner :refer [find-tests run-tests]]
  '[mal.core-test]
  '[mal.lexer-test]
  '[mal.parsing-test]
  '[mal.python.compiler-test]
  '[mal.reader-test])

(clojure.core/defn -main [& _args]
  (run-tests
    (find-tests
      ['mal.core-test
       'mal.lexer-test
       'mal.parsing-test
       'mal.python.compiler-test
       'mal.reader-test])))
