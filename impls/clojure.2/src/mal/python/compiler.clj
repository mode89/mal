(ns mal.python.compiler
  (:require [clojure.string :refer [join triml]]
            [mal.core :as core]))

(defrecord CompileContext [ns-registry current-ns locals counter])

(defmacro assert-symbol [s]
  `(let [s# ~s]
     (assert (string? s#))
     (assert (not-any? (fn [c#] (= c# \newline)) s#))
     (assert (-> s# triml seq))))

(defmacro assert-line [s]
  `(let [s# ~s]
     (assert (string? s#))
     (assert (not-any? (fn [c#] (= c# \newline)) s#))
     (assert (-> s# triml seq))))

(defn indent-lines [lines prefix]
  (map (fn [line]
         (assert-line line)
         (str prefix line))
       lines))

(defn indent-lines1 [lines]
  (indent-lines lines "  "))

(defn emit-assign [name value]
  (assert-symbol name)
  (assert-line value)
  [(str name " = " value)])

(defn emit-call [name args kwargs]
  (assert-symbol name)
  (assert (map? kwargs))
  (let [kwargs* (sort (map (fn [[k v]]
                             (assert-symbol k)
                             (assert-line v)
                             (str k "=" v))
                           kwargs))]
    [(str name "(" (join ", " (concat args kwargs*)) ")")]))

(defn emit-if [condition then elifs else]
  (assert-line condition)
  (assert (seq then))
  (concat
    [(str "if " condition ":")]
    (indent-lines1 then)
    (when (some? elifs)
      (mapcat
        (fn [[condition body]]
          (assert-line condition)
          (assert (seq body))
          (cons (str "elif " condition ":")
                (indent-lines1 body)))
        elifs))
    (when (some? else)
      (assert (seq else))
      (cons "else:" (indent-lines1 else)))))

(defn emit-while [condition body]
  (assert-line condition)
  (assert (seq body))
  (cons (str "while " condition ":")
        (indent-lines1 body)))

(defn emit-break []
  ["break"])

(defn emit-continue []
  ["continue"])

(defn emit-def [name params body]
  (assert-symbol name)
  (assert (seq body))
  (let [params* (map (fn [param]
                       (assert-symbol param)
                       param)
                     params)]
    (cons
      (str "def " name "(" (join ", " params*) "):")
      (indent-lines1 body))))

(defn emit-return [value]
  (assert-line value)
  [(str "return " value)])

(defn emit-try [body excepts finally]
  (assert (seq body))
  (concat
    ["try:"]
    (indent-lines1 body)
    (when (some? excepts)
      (mapcat
        (fn [[exception binding body]]
          (assert-symbol exception)
          (assert (seq body))
          (cons
            (str "except " exception
                 (when (some? binding)
                   (assert-symbol binding)
                   (str " as " binding))
                 ":")
            (indent-lines1 body)))
           excepts))
    (when (some? finally)
      (assert (seq finally))
      (cons "finally:" (indent-lines1 finally)))))

(defn emit
  "Emit list of python source lines for the given python-like AST."
  [ast]
  (let [tag (first ast)]
    (assert (keyword? tag))
    (case tag
      ; Assign an expression to a variable
      :assign (let [[name value] (rest ast)]
                (assert (= :expr (first value)))
                (emit-assign name (first (emit value))))
      :call (let [[name args kwargs] (rest ast)]
              (emit-call name
                (map (fn [arg]
                       (assert (= :expr (first arg)))
                       (first (emit arg)))
                     args)
                (into {}
                  (map (fn [[k v]]
                         (assert (= :expr (first v)))
                         [k (first (emit v))]))
                    kwargs)))
      ; Either a string literal or a call expression
      :expr (let [expr (second ast)]
              (cond
                (string? expr) [expr]
                (= :call (first expr)) (emit expr)
                :else (throw (ex-info "Invalid expression" {:expr expr}))))
      ; A list of statements. Can't be empty.
      :block (let [statements (rest ast)]
               (assert (seq statements))
               (mapcat emit statements))
      :if (let [[condition then elifs else] (rest ast)]
            (assert (= :expr (first condition)))
            (assert (= :block (first then)))
            (emit-if (first (emit condition))
              (emit then)
              (map (fn [[condition body]]
                     (assert (= :expr (first condition)))
                     (assert (= :block (first body)))
                     [(first (emit condition)) (emit body)])
                   elifs)
              (when (some? else)
                (assert (= :block (first else)))
                (emit else))))
      :while (let [[condition body] (rest ast)]
               (assert (= :expr (first condition)))
               (assert (= :block (first body)))
               (emit-while (first (emit condition)) (emit body)))
      :break (emit-break)
      :continue (emit-continue)
      :def (let [[name params body] (rest ast)]
             (assert (= :block (first body)))
             (emit-def name params (emit body)))
      :return (let [value (second ast)]
                (assert (= :expr (first value)))
                (emit-return (first (emit value))))
      :try (let [[body excepts finally] (rest ast)]
             (assert (= :block (first body)))
             (emit-try (emit body)
               (map (fn [[exception binding body]]
                      (assert (= :block (first body)))
                      [exception binding (emit body)])
                    excepts)
               (when (some? finally)
                 (emit finally)))))))

(defn mangle [sym]
  (assert (core/symbol? sym) "must be a symbol")
  (str
    (when-some [ns (:namespace sym)]
      (str ns "."))
    (:name sym)))

(defn temp-name [counter]
  (assert (int? counter) "counter must be an integer")
  (str "___temp_" counter))

(defn gen-temp-name [ctx]
  [(temp-name (:counter ctx))
   (update ctx :counter inc)])

(defn globals [name]
  (assert (string? name))
  (str "globals()[\"" name "\"]"))

(defn- throw-not-found [sym]
  (core/throw
    (str "'"
         (when-some [namespace (:namespace sym)]
           (str namespace "/"))
         (:name sym)
         "' not found")))

(defn resolve-symbol-name [ctx sym]
  (assert (core/symbol? sym) "must be a symbol")
  (assert (set? (:locals ctx)) "locals must be a set")
  (cond
    (some? (:namespace sym))
      (let [sym-ns-name (:namespace sym)
            sym-ns (get (:ns-registry ctx) (core/symbol sym-ns-name))]
        (if (some? sym-ns)
          (let [bindings (:bindings sym-ns)
                sym-name (:name sym)
                simp-sym (core/symbol sym-name)]
            (assert (set? bindings) "namespace bindings must be a set")
            (if (contains? bindings simp-sym)
              (mangle sym)
              (throw-not-found sym)))
          (core/throw (str "namespace '" sym-ns-name "' not found"))))
    (contains? (:locals ctx) sym)
      (mangle sym)
    (some? (:current-ns ctx))
      (let [current-ns (get (:ns-registry ctx) (:current-ns ctx))
            bindings (:bindings current-ns)]
        (assert (some? current-ns) "current namespace not found")
        (assert (set? bindings) "namespace bindings must be a set")
        (if (contains? bindings sym)
          (mangle sym)
          (throw-not-found sym)))
    :else
      (throw-not-found sym)))

(defn transform
  "Transform lisp AST into python AST"
  [ctx form]
  {:pre [(instance? CompileContext ctx)]
   :post [(= :expr (first (first %)))
          (instance? CompileContext (nth % 2))]}
  (cond
    (list? form)
      (if (empty? form)
        [[:expr "list()"] nil ctx]
        (let [head (first form)
              args (rest form)]
          (condp = head
            (core/symbol "def!")
              (let [name (first args)
                    value-form (second args)]
                (assert (core/symbol? name)
                        "def! expects a symbol as the first argument")
                (assert (= (count args) 2) "def! expects 2 arguments")
                (let [[val-expr val-body ctx2] (transform ctx value-form)
                      current-ns (:current-ns ctx)]
                  (assert (some? current-ns) "no current namespace")
                  [[:expr (mangle name)]
                   (conj val-body [:assign (globals (mangle name)) val-expr])
                   (update-in ctx2
                     [:ns-registry current-ns :bindings]
                     conj name)]))
            (core/symbol "do")
              (if (empty? args)
                [[:expr "None"] nil ctx]
                (loop [args args
                       body nil
                       ctx ctx]
                  (let [[res res-body ctx2] (transform ctx (first args))]
                    (if (= 1 (count args))
                      (let [[temp ctx3] (gen-temp-name ctx2)]
                        [[:expr temp]
                         (concat
                           body
                           res-body
                           [[:assign temp res]])
                         ctx3])
                      (recur
                        (rest args)
                        (concat
                          body
                          res-body
                          [res])
                        ctx2))))))))
    (core/symbol? form)
      [[:expr (resolve-symbol-name ctx form)] nil ctx]
    :else
      [[:expr (pr-str form)] nil ctx]))
