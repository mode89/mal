(ns mal.core
  (:refer-clojure :exclude [keyword keyword? symbol symbol?])
  (:require [mal.types])
  (:import [mal.types Keyword Symbol]))

(defn keyword [name]
  (Keyword. name))

(defn keyword? [x]
  (instance? Keyword x))

(defn symbol [name]
  (Symbol. name))

(defn symbol? [x]
  (instance? Symbol x))
