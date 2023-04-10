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

(defn debug-macro [x]
  (let [m (meta x)
        tag (:tag m)
        prefix (if (some? tag)
                 (str tag)
                 nil)]
    `(let [x# ~x
           prefix# ~prefix]
       (if (some? prefix#)
         (println prefix# x#)
         (println x#))
       x#)))
