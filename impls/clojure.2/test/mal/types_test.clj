(ns mal.types-test
  (:require [clojure.test :refer [deftest is]]
            [mal.types :as types])
  (:import [mal.types Symbol]))

(deftest functions
  (is (types/fn? (types/->Function false [] nil nil nil)))
  (is (types/fn? (fn [] nil))))

(deftest symbols
  (let [sym (types/symbol "foo")]
    (is (instance? Symbol sym))
    (is (= "foo" (:name sym)))
    (is (= nil (:namespace sym))))
  (let [sym (types/symbol "foo/bar")]
    (is (instance? Symbol sym))
    (is (= "bar" (:name sym)))
    (is (= "foo" (:namespace sym))))
  (let [sym (types/symbol "/")]
    (is (instance? Symbol sym))
    (is (= "/" (:name sym)))
    (is (= nil (:namespace sym))))
  (is (types/symbol? (Symbol. nil "foo")))
  (is (thrown-with-msg? Error #"Symbol name must be a string\. Got: 42"
        (types/symbol 42))))
