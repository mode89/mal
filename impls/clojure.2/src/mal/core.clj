(in-ns 'mal.core)

; >>> clojure specific

(clojure.core/require 'mal.clojure.impl)
(clojure.core/alias 'impl 'mal.clojure.impl)

(def ^:macro defn #'clojure.core/defn)
(def ^:macro or #'clojure.core/or)
(def ^:macro and #'clojure.core/and)
(def ^:macro assert #'clojure.core/assert)
(def ^:macro let #'clojure.core/let)
(def ^:macro defrecord #'impl/defrecord)
(def ^:macro fn #'clojure.core/fn)
(def ^:macro declare #'clojure.core/declare)
(def ^:macro when #'clojure.core/when)
(def ^:macro when-some #'clojure.core/when-some)
(def ^:macro if-some #'clojure.core/if-some)
(def ^:macro case #'clojure.core/case)
(def ^:macro cond #'clojure.core/cond)
(def ^:macro condp #'clojure.core/condp)
(def ^:macro -> #'clojure.core/->)
(def ^:macro loop #'clojure.core/loop)

(def concat clojure.core/concat)
(def partition clojure.core/partition)
(def partition-by clojure.core/partition-by)
(def butlast clojure.core/butlast)
(def last clojure.core/last)

; <<< clojure specific

(defrecord Function [macro? params body context make-locals])
(defrecord Keyword [name])
(defrecord Symbol [namespace name])
(defrecord Namespace [name bindings])
(defrecord EvalContext [ns-registry current-ns])

(def identical? impl/identical?)
(def instance? impl/instance?)
(def type impl/type)
(def throw impl/throw)
(def meta impl/meta)
(def string? impl/string?)
(def subs impl/subs)
(def inc impl/inc)
(def boolean? impl/boolean?)
(def number? impl/number?)
(def atom impl/atom)
(def atom? impl/atom?)
(def deref impl/deref)
(def reset! impl/reset!)
(def list impl/list)
(def list? impl/list?)
(def vec impl/vec)
(def vector impl/vector)
(def vector? impl/vector?)
(def hash-map impl/hash-map)
(def map? impl/map?)
(def hash-set impl/hash-set)
(def set? impl/set?)
(def seq impl/seq)
(def seq? impl/seq?)
(def = impl/equal?)
(def > impl/greater?)
(def < impl/less?)
(def >= impl/greater-equal?)
(def <= impl/less-equal?)
(def + impl/plus)
(def - impl/minus)
(def * impl/multiply)
(def / impl/divide)
(def assoc impl/assoc)
(def dissoc impl/dissoc)
(def contains? impl/contains?)
(def get impl/get)
(def empty? impl/empty?)
(def count impl/count)
(def reverse impl/reverse)
(def first impl/first)
(def second impl/second)
(def rest impl/rest)
(def cons impl/cons)
(def drop impl/drop)
(def nth impl/nth)
(def reduce impl/reduce)
(def reduce-kv impl/reduce-kv)
(def into impl/into)
(def keys impl/keys)
(def vals impl/vals)
(def even? impl/even?)
(def print impl/print)
(def slurp impl/slurp)

(def object-exception impl/object-exception)
(def object-exception? impl/object-exception?)
(def object-exception-unwrap impl/object-exception-unwrap)

(declare eval)
(declare map)
(declare pr-str)
(declare str)

(defn nil? [x]
  (identical? x nil))

(defn true? [x]
  (identical? x true))

(defn false? [x]
  (identical? x false))

(defn not [x]
  (if x false true))

(defn some? [x]
  (not (nil? x)))

(defn fn? [x]
  (or (and (instance? Function x)
           (not (:macro? x)))
      (impl/native-fn? x)))

(defn symbol
  ([name]
    (assert (string? name) (str "Symbol name must be a string. Got: " name))
    (let [separator (impl/index-of name \/)]
      (if (and (some? separator)
               (> (count name) 1))
        (new Symbol (subs name 0 separator) (subs name (inc separator)))
        (new Symbol nil name))))
  ([ns name]
    (assert (string? name) (str "Symbol name must be a string. Got: " name))
    (new Symbol
      (when (some? ns)
        (assert (string? ns)
          (str "Symbol namespace name must be a string. Got: " ns))
        ns)
      name)))

(defn symbol? [x]
  (instance? Symbol x))

(defn simple-symbol? [x]
  (and (symbol? x)
       (nil? (:namespace x))))

(defn keyword [name]
  (if (impl/instance? Keyword name)
    name
    (new Keyword name)))

(defn keyword? [x]
  (instance? Keyword x))

(defn macro? [x]
  (and (instance? Function x)
       (:macro? x)))

(defn sequential? [x]
  (or (list? x)
      (vector? x)))

(defn seqable? [x]
  (or (nil? x)
      (list? x)
      (vector? x)
      (map? x)
      (set? x)
      (seq? x)))

(defn apply [f & args]
  (assert (> (count args) 0) "apply expects at least 2 arguments")
  (let [args* (let [rev-args (reverse args)
                    last-arg (first rev-args)]
                (assert (seqable? last-arg)
                  "last argument to apply must be a sequence")
                (reduce
                  (fn [acc x]
                    (cons x acc))
                  last-arg
                  (rest rev-args)))]
    (cond
      (instance? Function f)
        (let [make-locals (:make-locals f)]
          (eval (atom (:context f)) (make-locals args*) (:body f)))
      (impl/native-fn? f)
        (impl/apply f args*)
      :else
        (impl/throw (str "Can't call this: " (pr-str f))))))

(defn swap! [a f & args]
  (assert (atom? a))
  (impl/swap! a
    (fn [x]
      (apply f x args))))

(defn -pr-char-readable [ch]
  (case ch
    \"       "\\\""
    \newline "\\n"
    \tab     "\\t"
    \\       "\\\\"
    ch))

(defn map [f coll]
  (impl/map
    (if (impl/native-fn? f)
      f
      (fn [x]
        (apply f (list x))))
    coll))

(defn mapcat [f & colls]
  (apply concat (apply map f colls)))

(defn ns-make [name]
  (assert (symbol? name) "namespace name must be a symbol")
  (new Namespace name (atom {})))

(defn ns-bind [ns name value]
  (assert (symbol? name) "binding name must be a symbol")
  (swap! (:bindings ns) assoc name value))

(defn ns-find-or-create [ctx name]
  (let [registry-atom (-> ctx deref :ns-registry)
        registry (deref registry-atom)]
    (if (contains? registry name)
      (get registry name)
      (let [new-ns (ns-make name)]
        (swap! registry-atom assoc name new-ns)
        new-ns))))

(defn throw-not-found [sym]
  (impl/throw (str "'" sym "' not found")))

(defn resolve-symbol [ctx locals sym]
  (assert (symbol? sym) "must be a symbol")
  (assert (sequential? locals) "locals must be a sequential collection")
  (if-some [sym-ns-name (:namespace sym)]
    (if-some [sym-ns (-> ctx deref
                         :ns-registry deref
                         (get (symbol sym-ns-name)))]
      (let [bindings (-> sym-ns :bindings deref)
            simp-sym (symbol (:name sym))]
        (if (contains? bindings simp-sym)
          (get bindings simp-sym)
          (throw-not-found sym)))
      (impl/throw (str "namespace '" sym-ns-name "' not found")))
    (loop [locals* locals]
      (if (empty? locals*)
        (if-some [current-ns (-> ctx deref :current-ns)]
          (let [bindings (-> current-ns :bindings deref)]
            (if (contains? bindings sym)
              (get bindings sym)
              (throw-not-found sym)))
          (throw-not-found sym))
        (let [bindings (first locals*)]
          (if (contains? bindings sym)
            (get bindings sym)
            (recur (rest locals*))))))))

(defn fn-env-template
  "Returns a map of parameter names to functions that extract the
  corresponding argument from the argument list."
  [parameters]
  (loop [params parameters
         param-index 0
         template {}]
    (if (empty? params)
      template
      (let [param (first params)]
        (assert (symbol? param) "function parameter must be a symbol")
        (if (= (:name param) "&")
          (let [var-params (second params)]
            (assert (symbol? var-params)
                    "variadic parameters must be a symbol")
            (assert (= (count params) 2)
                    "expected only one parameter after &")
            (assoc template var-params
                   (fn [args]
                     (drop param-index args))))
          (recur (rest params)
                 (inc param-index)
                 (assoc template param
                        (fn [args]
                          (nth args param-index)))))))))

(defn fn-env-bindings
  "Returns a map of parameter names to the corresponding argument values."
  [env-template args]
  (reduce-kv
    (fn [env param arg-extractor]
      (assoc env param (arg-extractor args)))
    {}
    env-template))

(defn make-fn* [ctx locals macro? params body]
  (assert (instance? EvalContext ctx))
  (assert (sequential? locals))
  (let [env-template (fn-env-template params)]
    (new Function
      macro?
      params
      body
      ctx
      (fn [args]
        (cons (fn-env-bindings env-template args)
              locals)))))

(declare expand-quasiquote)

(defn expand-quasiquote-list [form]
  (let [splice-unquote? (fn [element]
                          (and (list? element)
                               (= (symbol "splice-unquote")
                                  (first element))))]
    (apply list ; form must be stored as a list
      (cons
        (symbol "mal.core/concat")
        (mapcat
          (fn [segment]
            (if (splice-unquote? (first segment))
              (map
                (fn [element]
                  (assert (= (count element) 2)
                          "splice-unquote expects exactly one argument")
                  (second element))
                segment)
              (list ; this list is for preventing mapcat from flattening
                (apply list ; form must stored as a list
                  (cons
                    ; when macroexpanding, this sequence will be converted
                    ; to a list, to be consumed by concat
                    (symbol "mal.core/list")
                    (map expand-quasiquote segment))))))
          (partition-by splice-unquote? form))))))

(defn expand-quasiquote [form]
  (cond
    (list? form)
    (cond
      (= (symbol "unquote") (first form))
      (let [unquoted (second form)]
        (assert (= (count form) 2) "unquote expects exactly one argument")
        unquoted)

      (= (symbol "splice-unquote") (first form))
      (impl/throw "splice-unquote used outside of list context")

      :else
      (expand-quasiquote-list form))

    (vector? form)
    (list (symbol "mal.core/vec") (expand-quasiquote-list form))

    (map? form)
    (list (symbol "quote") form)

    (symbol? form)
    (list (symbol "quote") form)

    :else
    form))

(defn macroexpand [ctx locals form]
  (if (list? form)
    (let [head (first form)]
      (if (symbol? head)
        (if-some [macro (try (resolve-symbol ctx locals head)
                             (catch Exception _ nil))]
          (if (macro? macro)
            (let [mctx (:context macro)]
              (assert (identical? (:ns-registry mctx)
                                  (:ns-registry (deref ctx))))
              (apply macro (rest form)))
            form)
          form)
        form))
    form))

(defn eval [ctx locals form0]
  (let [form (macroexpand ctx locals form0)]
    (cond
      (list? form)
        (if (empty? form)
          form
          (let [head (first form)
                args (rest form)]
            (condp = head
              (symbol "def!")
                (let [name (first args)
                      value-ast (second args)]
                  (assert (= (count args) 2) "def! expects 2 arguments")
                  (let [value (eval ctx locals value-ast)
                        current-ns (-> ctx deref :current-ns)]
                    (assert (some? current-ns) "no current namespace")
                    (ns-bind current-ns name value)
                    value))
              (symbol "defmacro!")
                (let [name (first args)
                      f (eval ctx locals (second args))]
                  (assert (= (count args) 2) "defmacro! expects 2 arguments")
                  (assert (symbol? name) "name of macro must be a symbol")
                  (assert (fn? f)
                          "last argument to defmacro! must be a function")
                  (let [macro (assoc f :macro? true)
                        current-ns (-> ctx deref :current-ns)]
                    (assert (some? current-ns) "no current namespace")
                    (swap! (:bindings current-ns) assoc name macro)
                    macro))
              (symbol "let*")
                (let [bindings (first args)
                      body (second args)]
                  (assert (even? (count bindings))
                          "let* expects even number of forms in bindings")
                  (recur
                    ctx
                    (reduce
                      (fn [locals' [name value]]
                        (assert (symbol? name)
                                "binding name must be a symbol")
                        (cons {name (eval ctx locals' value)} locals'))
                      locals
                      (partition 2 bindings))
                    body))
              (symbol "do")
                (let [butlast-forms (butlast args)
                      last-form (last args)]
                  (loop [forms butlast-forms]
                    (when-some [form (first forms)]
                      (eval ctx locals form)
                      (recur (rest forms))))
                  (recur ctx locals last-form))
              (symbol "if")
                (let [nargs (count args)]
                  (assert (>= nargs 2) "if-form expects at least 2 arguments")
                  (assert (<= nargs 3) "if-form expects at most 3 arguments")
                  (if (eval ctx locals (first args))
                    (recur ctx locals (second args))
                    (if (= nargs 3)
                      (recur ctx locals (nth args 2))
                      nil)))
              (symbol "fn*")
                (make-fn*
                  (deref ctx)
                  locals
                  false
                  (first args)
                  (second args))
              (symbol "quote")
                (do (assert (= (count args) 1) "quote expects 1 argument")
                    (first args))
              (symbol "quasiquote")
                (do (assert (= (count args) 1)
                      "quasiquote expects 1 argument")
                    (recur ctx locals (expand-quasiquote (first args))))
              (symbol "quasiquoteexpand")
                (do (assert (= (count args) 1))
                    (expand-quasiquote (first args)))
              (symbol "macroexpand")
                (do (assert (= (count args) 1))
                    (macroexpand ctx locals (first args)))
              (symbol "try*")
                (let [try-expr (first args)]
                  (if-some [catch-form (second args)]
                    (do (assert (<= (count args) 2)
                          "try* expects at most 2 arguments")
                        (assert (= (first catch-form) (symbol "catch*"))
                          "try* expects catch* form as second argument")
                        (assert (= (count catch-form) 3)
                          "catch* expects 3 arguments")
                        (try
                          (eval ctx locals try-expr)
                          (catch Throwable ex0
                            (let [ex-binding (second catch-form)
                                  _ (assert (symbol? ex-binding)
                                      "exception object must be a symbol")
                                  catch-body (nth catch-form 2)
                                  ex (if (object-exception? ex0)
                                       (object-exception-unwrap ex0)
                                       ex0)]
                              (eval ctx
                                    (cons {ex-binding ex} locals)
                                    catch-body)))))
                    (do (assert (<= (count args) 1))
                        (recur ctx locals try-expr))))
              (symbol "in-ns")
                (let [ns-name (eval ctx locals (first args))]
                  (assert (= (count args) 1) "in-ns expects 1 argument")
                  (let [ns (ns-find-or-create ctx ns-name)]
                    (swap! ctx assoc :current-ns ns)
                    ns))
              (let [f (eval ctx locals head)
                    args (map (fn [x] (eval ctx locals x)) args)]
                (cond
                  (instance? Function f)
                    (let [fctx (:context f)
                          make-locals (:make-locals f)]
                      (assert (identical?
                                (:ns-registry fctx)
                                (:ns-registry (deref ctx))))
                      (recur (atom fctx)
                             (make-locals args)
                             (:body f)))
                  (impl/native-fn? f)
                    (impl/apply f args)
                  :else
                    (impl/throw (str "Can't call this: " (pr-str f))))))))
      (symbol? form)
        (if (= (symbol "*ns*") form)
          (-> ctx deref :current-ns)
          (resolve-symbol ctx locals form))
      (vector? form)
        (vec (map (fn [x] (eval ctx locals x)) form))
      (map? form)
        (into {}
          (map
            (fn [[k v]]
              [(eval ctx locals k)
               (eval ctx locals v)])
            form))
      :else
        form)))

(defn pr-str* [object print-readably]
  (cond
    (nil? object)
      "nil"
    (boolean? object)
      (impl/str object)
    (number? object)
      (impl/str object)
    (string? object)
      (if print-readably
        (apply impl/str
          (concat [\"] (map -pr-char-readable object) [\"]))
        object)
    (symbol? object)
      (impl/str
        (when-some [ns (:namespace object)]
          (str ns "/"))
        (:name object))
    (keyword? object)
      (impl/str \: (:name object))
    (list? object)
      (impl/str \(
           (impl/join " "
             (map (fn [x]
                    (pr-str* x print-readably))
                  object))
           \) )
    (vector? object)
      (impl/str \[
           (impl/join " "
             (map (fn [x]
                    (pr-str* x print-readably))
                  object))
           \] )
    (map? object)
      (impl/str \{
           (impl/join " "
             (map (fn [x]
                    (pr-str* x print-readably))
                  (apply concat (seq object))))
           \} )
    (fn? object)
      (impl/str "#function[" (impl/str object) "]")
    (macro? object)
      (impl/str "#macro[" (impl/str object) "]")
    (atom? object)
      (impl/str "(atom " (deref object) ")")
    (instance? Namespace object)
      (impl/str "#namespace[" (-> object :name :name) "]")
    :else
      (impl/str "#object["
                (impl/str (type object))
                " "
                (impl/str object)
                "]")))

(defn pr-str [& args]
  (impl/join " "
    (map (fn [x]
           (pr-str* x true))
         args)))

(defn str [& args]
  (impl/join ""
    (map (fn [x]
           (if (some? x)
             (pr-str* x false)
             ""))
         args)))

(defn prn [& args]
  (print (apply pr-str args))
  (print \newline))

(defn println [& args]
  (print
    (impl/join " "
      (map (fn [x]
             (pr-str* x false))
           args)))
  (print \newline))
