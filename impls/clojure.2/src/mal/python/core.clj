(in-ns 'mal.core)

(import [dataclasses make_dataclass])
(import [pyrsistent plist pmap pvector pset PVector PList PMap PSet])
(import [threading Lock])

; (def ^:macro defrecord #'clojure.core/defrecord)
; (def ^:macro defn #'clojure.core/defn)

(def! identical?
  (fn* [x y]
    (___python_expression x " is " y)))

(def! =
  (fn* [left right]
    (___python_expression left " == " right)))

(def! >
  (fn* [left right]
    (___python_expression left " > " right)))

(def! <
  (fn* [left right]
    (___python_expression left " < " right)))

(def! >=
  (fn* [left right]
    (___python_expression left " >= " right)))

(def! <=
  (fn* [left right]
    (___python_expression left " <= " right)))

(def! +
  (fn* [left right]
    (___python_expression left " + " right)))

(def! -
  (fn* [left right]
    (___python_expression left " - " right)))

(def! *
  (fn* [left right]
    (___python_expression left " * " right)))

(def! /
  (fn* [left right]
    (___python_expression left " / " right)))

(def! inc
  (fn* [x]
    (___python_expression x " + 1")))

(def! instance?
  (fn* [type obj]
    (___python_expression "isinstance(" obj ", " type ")")))

(def! type (___python_expression "type"))

(def! array
  (fn* [& args]
    (___python_expression "[*" args "]")))

(def! Symbol
  (make_dataclass "Symbol" (array "namespace" "name")))

(def! symbol
  (fn* [ns name]
    (new Symbol ns name)))

(def! symbol?
  (fn* [x]
    (instance? Symbol x)))

(def! simple-symbol?
  (fn* [x]
    (if (symbol? x)
      (= (___python_expression x ".namespace") nil)
      false)))

(def! Keyword
  (make_dataclass "Keyword" (array "namespace" "name")))

(def! keyword
  (fn* [ns name]
    (new Keyword ns name)))

(def! keyword?
  (fn* [x]
    (instance? Keyword x)))

(def! get-name
  (fn* [x]
    (___python_expression x ".name")))

(def! get-namespace
  (fn* [x]
    (___python_expression x ".namespace")))

(def! native-fn? (___python_expression "callable"))

(def! to-string (___python_expression "str"))

(def! string?
  (fn* [x]
    (instance? (___python_expression "str") x)))

(def! join
  (fn* [sep coll]
    (___python_expression sep ".join(" coll ")")))

(def! substring
  (fn* [string start end]
    (___python_expression string "[" start ":" end "]")))

(def! boolean?
  (fn* [x]
    (instance? (___python_expression "bool") x)))

(def! number?
  (fn* [x]
    (if (instance? (___python_expression "int") x)
      true
      (instance? (___python_expression "float") x))))

(def! list
  (fn* [& args]
    (plist args)))

(def! list?
  (fn* [coll]
    (instance? PList coll)))

(def! vec
  (fn* [coll]
    (pvector coll)))

(def! vector
  (fn* [& args]
    (pvector args)))

(def! vector?
  (fn* [x]
    (instance? PVector x)))

(def! hash-map
  (fn* [& args]
    (pmap
      (let* [range* (___python_expression "range(0, len(" args "), 2)")]
        (___python_expression
          "{" args "[i]: " args "[i + 1] for i in " range* "}")))))

(def! map?
  (fn* [x]
    (instance? PMap x)))

(def! hash-set
  (fn* [& args]
    (pset args)))

(def! set
  (fn* [coll]
    (pset coll)))

(def! set?
  (fn* [x]
    (instance? PSet x)))

(def! apply
  (fn* [f args]
    (___python_expression f "(*" args ")")))

; (def seq clojure.core/seq)
; (def seq? clojure.core/seq?)
; (def meta clojure.core/meta)

; (def assoc clojure.core/assoc)
; (def dissoc clojure.core/dissoc)
; (def contains? clojure.core/contains?)
; (def get clojure.core/get)
; (def empty? clojure.core/empty?)
; (def count clojure.core/count)
; (def reverse clojure.core/reverse)
; (def first clojure.core/first)
; (def second clojure.core/second)
; (def rest clojure.core/rest)
; (def cons clojure.core/cons)
; (def drop clojure.core/drop)
; (def nth clojure.core/nth)
; (def map clojure.core/map)
; (def reduce clojure.core/reduce)
; (def reduce-kv clojure.core/reduce-kv)
; (def into clojure.core/into)

(def! even?
  (fn* [x]
    (___python_expression x " % 2 == 0")))

(def! print
  (fn* [x]
    (___python_expression "print(" x ")")))

(def! slurp
  (fn* [path]
    (let* [file (___python_expression "open(" path ", \"r\")")
          content (___python_expression file ".read()")]
      (do (___python_expression file ".close()")
          content))))

(def! ObjectException
  (make_dataclass "ObjectException" (array "object")))

(def! object-exception
  (fn* [obj]
    (___python_expression "Exception(" ObjectException "(" obj "))")))

(def! object-exception?
  (fn* [ex]
    (if (instance? (___python_expression "Exception") ex)
      (if (= (___python_expression "len(" ex ".args)") 1)
        (instance? ObjectException (___python_expression ex ".args[0]"))
        false)
      false)))

(def! object-exception-unwrap
  (fn* [ex]
    (___python_expression ex ".args[0].object")))

(def! -raise
  (fn* [ex]
    (inline-python
      [:raise ex]
      [:value "None"])))

(def! throw
  (fn* [obj]
    (if (instance? (___python_expression "BaseException") obj)
      (-raise obj)
      (-raise (object-exception obj)))))

; (defn keys [m]
;   (if (empty? m)
;     (list)
;     (clojure.core/keys m)))

; (defn vals [m]
;   (if (empty? m)
;     (list)
;     (clojure.core/vals m)))

(def! Atom
  (make_dataclass "Atom" (array "value" "lock")))

(def! atom
  (fn* [x]
    (new Atom x (Lock))))

(def! atom?
  (fn* [x]
    (instance? Atom x)))

(def! deref
  (fn* [a]
    (inline-python
      [:call [:dot [:dot a "lock"] "acquire"]]
      [:assign [:value "value"] [:dot a "value"]]
      [:call [:dot [:dot a "lock"] "release"]]
      [:value "value"])))

(def! reset!
  (fn* [a x]
    (inline-python
      [:call [:dot [:dot a "lock"] "acquire"]]
      [:assign [:dot a "value"] x]
      [:call [:dot [:dot a "lock"] "release"]]
      x)))

(def! swap!
  (fn* [a f]
    (inline-python
      [:call [:dot [:dot a "lock"] "acquire"]]
      [:assign [:value "new_value"] [:call f [:dot a "value"]]]
      [:assign [:dot a "value"] [:value "new_value"]]
      [:call [:dot [:dot a "lock"] "release"]]
      [:value "new_value"])))
