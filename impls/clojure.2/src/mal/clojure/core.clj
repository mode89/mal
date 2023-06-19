(in-ns 'mal.core)

(def ^:macro defrecord #'clojure.core/defrecord)
(def ^:macro defn #'clojure.core/defn)
(def ^:macro fn #'clojure.core/fn)
(def ^:macro let #'clojure.core/let)
(def ^:macro declare #'clojure.core/declare)
(def ^:macro or #'clojure.core/or)
(def ^:macro and #'clojure.core/and)
(def ^:macro assert #'clojure.core/assert)
(def ^:macro when #'clojure.core/when)
(def ^:macro when-some #'clojure.core/when-some)
(def ^:macro if-some #'clojure.core/if-some)
(def ^:macro case #'clojure.core/case)
(def ^:macro cond #'clojure.core/cond)
(def ^:macro condp #'clojure.core/condp)
(def ^:macro loop #'clojure.core/loop)
(def ^:macro doseq #'clojure.core/doseq)
(def ^:macro -> #'clojure.core/->)

(def identical? clojure.core/identical?)
(def instance? clojure.core/instance?)
(def symbol clojure.core/symbol)
(def symbol? clojure.core/symbol?)
(def simple-symbol? clojure.core/simple-symbol?)
(def keyword clojure.core/keyword)
(def keyword? clojure.core/keyword?)
(def name clojure.core/name)
(def namespace clojure.core/namespace)
(def char? clojure.core/char?)
(def string? clojure.core/string?)
(def join clojure.string/join)
(def index-of clojure.string/index-of)
(def subs clojure.core/subs)
(def inc clojure.core/inc)
(def atom clojure.core/atom)
(def deref clojure.core/deref)
(def reset! clojure.core/reset!)
(def boolean? clojure.core/boolean?)
(def number? clojure.core/number?)
(def list clojure.core/list)
(def vec clojure.core/vec)
(def vector clojure.core/vector)
(def vector? clojure.core/vector?)
(def hash-map clojure.core/hash-map)
(def hash-set clojure.core/hash-set)
(def set? clojure.core/set?)
(def seq? clojure.core/seq?)
(def seq clojure.core/seq)
(def type clojure.core/type)
(def meta clojure.core/meta)
(def swap! clojure.core/swap!)
(def assoc clojure.core/assoc)
(def dissoc clojure.core/dissoc)
(def contains? clojure.core/contains?)
(def get clojure.core/get)
(def empty? clojure.core/empty?)
(def = clojure.core/=)
(def > clojure.core/>)
(def < clojure.core/<)
(def >= clojure.core/>=)
(def <= clojure.core/<=)
(def + clojure.core/+)
(def - clojure.core/-)
(def * clojure.core/*)
(def / clojure.core//)
(def count clojure.core/count)
(def reverse clojure.core/reverse)
(def first clojure.core/first)
(def second clojure.core/second)
(def rest clojure.core/rest)
(def cons clojure.core/cons)
(def drop clojure.core/drop)
(def nth clojure.core/nth)
(def map clojure.core/map)
(def reduce clojure.core/reduce)
(def reduce-kv clojure.core/reduce-kv)
(def into clojure.core/into)
(def concat clojure.core/concat)
(def partition clojure.core/partition)
(def partition-by clojure.core/partition-by)
(def butlast clojure.core/butlast)
(def last clojure.core/last)
(def even? clojure.core/even?)
(def print clojure.core/print)
(def slurp clojure.core/slurp)

(defrecord Function [macro? params body context make-locals])
(defrecord Namespace [name bindings])
(defrecord EvalContext [ns-registry current-ns])

(declare eval)
(declare map)
(declare pr-str)
(declare str)

(def OBJECT-EXCEPTION (new Object))

(defn object-exception [obj]
  (clojure.core/ex-info "object exception"
    {:object obj :tag OBJECT-EXCEPTION}))

(defn object-exception? [ex]
  (clojure.core/and
    (instance? clojure.lang.ExceptionInfo ex)
    (clojure.core/=
      OBJECT-EXCEPTION
      (clojure.core/-> ex clojure.core/ex-data :tag))))

(defn object-exception-unwrap [ex]
  (clojure.core/-> ex clojure.core/ex-data :object))

(defn throw [obj]
  (if (instance? Throwable obj)
    (throw obj)
    (throw (object-exception obj))))

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
      (clojure.core/fn? x)))

(defn macro? [x]
  (and (instance? Function x)
       (:macro? x)))

(defn atom? [x]
  (instance? clojure.lang.Atom x))

(defn list? [x]
  (clojure.core/seq? x))

(defn map? [x]
  (clojure.core/or
    (instance? clojure.lang.PersistentArrayMap x)
    (instance? clojure.lang.PersistentHashMap x)))

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

(defn keys [m]
  (if (empty? m)
    (list)
    (clojure.core/keys m)))

(defn vals [m]
  (if (empty? m)
    (list)
    (clojure.core/vals m)))

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
      (clojure.core/fn? f)
        (clojure.core/apply f args*)
      :else
        (mal.core/throw (str "Can't call this: " (pr-str f))))))

(defn swap! [a f & args]
  (assert (atom? a))
  (clojure.core/swap! a
    (fn [x]
      (apply f x args))))

(defn -pr-char-readable [ch]
  (case ch
    \"       "\\\""
    \newline "\\n"
    \tab     "\\t"
    \\       "\\\\"
    (clojure.core/str ch)))

(defn map [f coll]
  (clojure.core/map
    (if (clojure.core/fn? f)
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
  (mal.core/throw (str "'" sym "' not found")))

(defn resolve-symbol [ctx locals sym]
  (assert (symbol? sym) "must be a symbol")
  (assert (sequential? locals) "locals must be a sequential collection")
  (if-some [sym-ns-name (namespace sym)]
    (if-some [sym-ns (-> ctx deref
                         :ns-registry deref
                         (get (symbol sym-ns-name)))]
      (let [bindings (-> sym-ns :bindings deref)
            simp-sym (symbol (name sym))]
        (if (contains? bindings simp-sym)
          (get bindings simp-sym)
          (throw-not-found sym)))
      (mal.core/throw (str "namespace '" sym-ns-name "' not found")))
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
        (if (= param '&)
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
                               (= 'splice-unquote (first element))))]
    (apply list ; form must be stored as a list
      (cons
        'mal.core/concat
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
                    'mal.core/list
                    (map expand-quasiquote segment))))))
          (partition-by splice-unquote? form))))))

(defn expand-quasiquote [form]
  (cond
    (list? form)
    (cond
      (= 'unquote (first form))
      (let [unquoted (second form)]
        (assert (= (count form) 2) "unquote expects exactly one argument")
        unquoted)

      (= 'splice-unquote (first form))
      (mal.core/throw "splice-unquote used outside of list context")

      :else
      (expand-quasiquote-list form))

    (vector? form)
    (list 'mal.core/vec (expand-quasiquote-list form))

    (map? form)
    (list 'quote form)

    (symbol? form)
    (list 'quote form)

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
              'def!
              (let [name (first args)
                    value-ast (second args)]
                (assert (= (count args) 2) "def! expects 2 arguments")
                (let [value (eval ctx locals value-ast)
                      current-ns (-> ctx deref :current-ns)]
                  (assert (some? current-ns) "no current namespace")
                  (ns-bind current-ns name value)
                  value))

              'defmacro!
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

              'let*
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

              'do
              (let [butlast-forms (butlast args)
                    last-form (last args)]
                (loop [forms butlast-forms]
                  (when-some [form (first forms)]
                    (eval ctx locals form)
                    (recur (rest forms))))
                (recur ctx locals last-form))

              'if
              (let [nargs (count args)]
                (assert (>= nargs 2) "if-form expects at least 2 arguments")
                (assert (<= nargs 3) "if-form expects at most 3 arguments")
                (if (eval ctx locals (first args))
                  (recur ctx locals (second args))
                  (if (= nargs 3)
                    (recur ctx locals (nth args 2))
                    nil)))

              'fn*
              (make-fn*
                (deref ctx)
                locals
                false
                (first args)
                (second args))

              'quote
              (do (assert (= (count args) 1) "quote expects 1 argument")
                  (first args))

              'quasiquote
              (do (assert (= (count args) 1)
                    "quasiquote expects 1 argument")
                  (recur ctx locals (expand-quasiquote (first args))))

              'quasiquoteexpand
              (do (assert (= (count args) 1))
                  (expand-quasiquote (first args)))

              'macroexpand
              (do (assert (= (count args) 1))
                  (macroexpand ctx locals (first args)))

              'try*
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

              'in-ns
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
                  (clojure.core/fn? f)
                    (apply f args)
                  :else
                    (mal.core/throw
                      (str "Can't call this: " (pr-str f))))))))
      (symbol? form)
        (if (= '*ns* form)
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
      (clojure.core/str object)
    (number? object)
      (clojure.core/str object)
    (char? object)
      (clojure.core/str object)
    (string? object)
      (if print-readably
        (join "" (concat ["\""] (map -pr-char-readable object) ["\""]))
        object)
    (symbol? object)
      (str (when-some [ns (namespace object)]
             (str ns "/"))
           (name object))
    (keyword? object)
      (str \:
           (when-some [ns (namespace object)]
             (str ns "/"))
           (name object))
    (list? object)
      (str \(
           (join " "
             (map (fn [x]
                    (pr-str* x print-readably))
                  object))
           \) )
    (vector? object)
      (str \[
           (join " "
             (map (fn [x]
                    (pr-str* x print-readably))
                  object))
           \] )
    (map? object)
      (str \{
           (join " "
             (map (fn [x]
                    (pr-str* x print-readably))
                  (apply concat (seq object))))
           \} )
    (fn? object)
      (str "#function[" (clojure.core/str object) "]")
    (macro? object)
      (str "#macro[" (clojure.core/str object) "]")
    (atom? object)
      (str "(atom " (deref object) ")")
    (instance? Namespace object)
      (str "#namespace[" (-> object :name name) "]")
    :else
      (str "#object["
           (clojure.core/str (type object))
           " "
           (clojure.core/str object)
           "]")))

(defn pr-str [& args]
  (join " "
    (map (fn [x]
           (pr-str* x true))
         args)))

(defn str [& args]
  (join ""
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
    (join " "
      (map (fn [x]
             (pr-str* x false))
           args)))
  (print \newline))

(defn debug-macro [x]
  (clojure.core/let [m (meta x)
        tag (:tag m)
        prefix (if (clojure.core/some? tag)
                 (clojure.core/str tag)
                 nil)]
    `(clojure.core/let [x# ~x
           prefix# ~prefix]
       (if (clojure.core/some? prefix#)
         (clojure.core/println prefix# x#)
         (clojure.core/println x#))
       x#)))
