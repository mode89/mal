(ns mal.types)

(defrecord Atom [value])
(defrecord Function [macro? params body make-env])
(defrecord Keyword [name])
(defrecord Symbol [name])
