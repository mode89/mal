(ns mal.environ-test
  (:require [clojure.test :refer [deftest is]]
            [mal.core :as core]
            [mal.environ :as environ]))

(deftest make
  (is (= (deref (environ/make nil {})) {:outer nil :table {}}))
  (is (= (deref (environ/make nil {(core/symbol "a") 1
                                   (core/symbol "b") 2}))
         {:outer nil :table {(core/symbol "a") 1
                             (core/symbol "b") 2}}))
  (let [outer-env (environ/make nil {(core/symbol "a") 1
                                     (core/symbol "b") 2})
        env (environ/make outer-env {(core/symbol "c") 3
                                     (core/symbol "d") 4})]
    (is (= (deref env) {:outer outer-env
                        :table {(core/symbol "c") 3
                                (core/symbol "d") 4}}))
    (is (= (deref outer-env) {:outer nil
                              :table {(core/symbol "a") 1
                                      (core/symbol "b") 2}}))))

(deftest env-set!
  (let [env (environ/make nil {})]
    (environ/set! env (core/symbol "a") 1)
    (is (= (deref env) {:outer nil :table {(core/symbol "a") 1}})))
  (let [env (environ/make nil {(core/symbol "a") 1})]
    (environ/set! env (core/symbol "a") 2)
    (is (= (deref env) {:outer nil :table {(core/symbol "a") 2}})))
  (let [env (environ/make nil {(core/symbol "a") 1})]
    (environ/set! env (core/symbol "b") 2)
    (is (= (deref env) {:outer nil
                        :table {(core/symbol "a") 1
                                (core/symbol "b") 2}})))
  (let [outer-env (environ/make nil {(core/symbol "a") 1})
        env (environ/make outer-env {(core/symbol "b") 2})]
    (environ/set! env (core/symbol "a") 3)
    (is (= (deref env) {:outer outer-env
                        :table {(core/symbol "b") 2
                                (core/symbol "a") 3}}))
    (is (= (deref outer-env) {:outer nil :table {(core/symbol "a") 1}}))))

(deftest env-get
  (let [env (environ/make nil {(core/symbol "a") 1})]
    (is (= (environ/get env (core/symbol "a")) 1))
    (is (thrown-with-msg? Exception #"Symbol 'b' not found"
          (environ/get env (core/symbol "b")))))
  (let [outer-env (environ/make nil {(core/symbol "a") 1})
        env (environ/make outer-env {(core/symbol "b") 2})]
    (is (= (environ/get env (core/symbol "a")) 1))))
