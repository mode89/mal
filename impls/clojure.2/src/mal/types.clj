(ns mal.types)

(defrecord Symbol [name])
(defrecord Keyword [name])
(defrecord Function [params body make-env])
