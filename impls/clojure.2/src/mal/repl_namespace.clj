(ns mal.repl-namespace
  (:require [mal.core :as core]
            [mal.reader :as reader]))

(def repl-namespace
  (core/->Namespace
    (core/symbol "mal.core")
    (core/atom
      {(core/symbol "list") core/list
       (core/symbol "list?") core/list?
       (core/symbol "empty?") core/empty?
       (core/symbol "count") core/count
       (core/symbol "=") core/=
       (core/symbol "<") core/<
       (core/symbol "<=") core/<=
       (core/symbol ">") core/>
       (core/symbol ">=") core/>=
       (core/symbol "+") core/+
       (core/symbol "-") core/-
       (core/symbol "*") core/*
       (core/symbol "/") core//
       (core/symbol "pr-str") core/pr-str
       (core/symbol "prn") core/prn
       (core/symbol "str") core/str
       (core/symbol "println") core/println
       (core/symbol "slurp") core/slurp
       (core/symbol "atom") core/atom
       (core/symbol "atom?") core/atom?
       (core/symbol "deref") core/deref
       (core/symbol "reset!") core/reset!
       (core/symbol "swap!") core/swap!
       (core/symbol "cons") core/cons
       (core/symbol "concat") core/concat
       (core/symbol "vec") core/vec
       (core/symbol "fn?") core/fn?
       (core/symbol "macro?") core/macro?
       (core/symbol "nth") core/nth
       (core/symbol "first") core/first
       (core/symbol "rest") core/rest
       (core/symbol "throw") core/throw
       (core/symbol "apply") core/apply
       (core/symbol "map") core/map
       (core/symbol "nil?") core/nil?
       (core/symbol "true?") core/true?
       (core/symbol "false?") core/false?
       (core/symbol "symbol") core/symbol
       (core/symbol "symbol?") core/symbol?
       (core/symbol "keyword") core/keyword
       (core/symbol "keyword?") core/keyword?
       (core/symbol "vector") core/vector
       (core/symbol "vector?") core/vector?
       (core/symbol "sequential?") core/sequential?
       (core/symbol "hash-map") core/hash-map
       (core/symbol "map?") core/map?
       (core/symbol "assoc") core/assoc
       (core/symbol "dissoc") core/dissoc
       (core/symbol "get") core/get
       (core/symbol "contains?") core/contains?
       (core/symbol "keys") core/keys
       (core/symbol "vals") core/vals
       (core/symbol "read-string") reader/read-string})))
