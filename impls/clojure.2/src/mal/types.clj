(ns mal.types)

(defrecord Atom [value])
(defrecord Function [params body make-env])
(defrecord Keyword [name])
(defrecord Symbol [name])
