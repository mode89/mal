(declare atom?)
(declare native-eval-try-catch)
(declare pr-str)
(declare str)
(declare throw)

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

(defn -not-found [sym]
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
          (-not-found sym)))
      (mal.core/throw (str "namespace '" sym-ns-name "' not found")))
    (loop [locals* locals]
      (if (empty? locals*)
        (if-some [current-ns (-> ctx deref :current-ns)]
          (let [bindings (-> current-ns :bindings deref)]
            (if (contains? bindings sym)
              (get bindings sym)
              (-not-found sym)))
          (-not-found sym))
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

(defn macroexpand-1 [ctx locals form]
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

(defn macroexpand [ctx locals form]
  (loop [form* form]
    (let [form** (macroexpand-1 ctx locals form*)]
      (if (identical? form* form**)
        form*
        (recur form**)))))

(defn macroexpand-all [ctx locals form]
  (let [form* (macroexpand ctx locals form)]
    (if (list? form*)
      (let [head (first form*)
            args (rest form*)]
        (cons head (map #(macroexpand-all ctx locals %) args)))
      form*)))

(defn eval [ctx locals form0]
  (let [form (macroexpand-1 ctx locals form0)]
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
                      (native-eval-try-catch
                        ctx locals try-expr catch-form))
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
                  (native-fn? f)
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
