(ns mal.types
  (:refer-clojure :exclude [fn? symbol symbol?])
  (:require [clojure.string :refer [index-of]]))

(defrecord Atom [value])
(defrecord Function [macro? params body context make-locals])
(defrecord Keyword [name])
(defrecord Symbol [namespace name])
(defrecord Namespace [name bindings])
(defrecord EvalContext [ns-registry current-ns])

(defn fn? [x]
  (or (and (instance? Function x)
           (not (:macro? x)))
      (clojure.core/fn? x)))

(defn symbol [name]
  (assert (string? name) (str "Symbol name must be a string. Got: " name))
  (let [separator (index-of name \/)]
    (if (and (some? separator)
             (> (count name) 1))
      (Symbol. (subs name 0 separator) (subs name (inc separator)))
      (Symbol. nil name))))

(defn symbol? [x]
  (instance? Symbol x))
