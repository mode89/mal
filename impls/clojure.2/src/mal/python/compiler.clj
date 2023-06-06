(ns mal.python.compiler
  (:require [clojure.string :refer [join triml]]
            [mal.core :as core]))

(def SPECIAL-NAMES
  #{"False"
    "None"
    "True"
    "and"
    "as"
    "assert"
    "break"
    "class"
    "continue"
    "def"
    "del"
    "elif"
    "else"
    "except"
    "finally"
    "for"
    "from"
    "getattr"
    "global"
    "globals"
    "hasattr"
    "if"
    "import"
    "in"
    "is"
    "lambda"
    "list"
    "map"
    "nonlocal"
    "not"
    "or"
    "pass"
    "raise"
    "return"
    "set"
    "setattr"
    "str"
    "try"
    "while"
    "with"
    "yield"})

(defn munge-special-name [name]
  (assert (string? name))
  (str "___" name))

(def RESERVED-NAMES (set (map munge-special-name SPECIAL-NAMES)))

(def MUNGE-MAP
  {\- "_"
   \. "_DOT_"
   \: "_COLON_"
   \+ "_PLUS_"
   \> "_GT_"
   \< "_LT_"
   \= "_EQ_"
   \~ "_TILDE_"
   \! "_BANG_"
   \@ "_AT_"
   \# "_SHARP_"
   \' "_SQUOTE_"
   \" "_DQUOTE_"
   \% "_PERCENT_"
   \^ "_CARET_"
   \& "_AMPER_"
   \* "_STAR_"
   \| "_BAR_"
   \{ "_LBRACE_"
   \} "_RBRACE_"
   \[ "_LBRACK_"
   \] "_RBRACK_"
   \/ "_SLASH_"
   \\ "_BSLASH_"
   \? "_QMARK_"
   \$ "_DOLLAR_"})

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

(defn expression? [ast]
  (let [tag (first ast)]
    (or (= :value tag)
        (= :call tag))))

(defn emit-assign [left right]
  (assert-symbol left)
  (assert-line right)
  [(str left " = " right)])

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
    (assert (keyword? tag) (str "Invalid tag: " tag))
    (case tag
      ; Assign an expression to a variable
      :assign (let [[left right] (rest ast)]
                (assert (expression? left))
                (assert (expression? right))
                (emit-assign (first (emit left)) (first (emit right))))
      :call (let [[name & args] (rest ast)
                  [pargs kwargs] (let [kwargs (last args)]
                                   (if (map? kwargs)
                                     [(butlast args) kwargs]
                                     [args {}]))]
              (assert (expression? name))
              (emit-call (first (emit name))
                (map (fn [arg]
                       (assert (expression? arg))
                       (first (emit arg)))
                     pargs)
                (into {}
                  (map (fn [[k v]]
                         (assert (expression? v))
                         [k (first (emit v))])
                       kwargs))))
      :value (let [value (second ast)]
               (assert (string? value))
               [value])
      ; A list of statements. Can't be empty.
      :block (let [statements (rest ast)]
               (assert (seq statements))
               (mapcat emit statements))
      :if (let [[condition then elifs else] (rest ast)]
            (assert (expression? condition))
            (assert (= :block (first then)))
            (emit-if (first (emit condition))
              (emit then)
              (map (fn [[condition body]]
                     (assert (expression? condition))
                     (assert (= :block (first body)))
                     [(first (emit condition)) (emit body)])
                   elifs)
              (when (some? else)
                (assert (= :block (first else)))
                (emit else))))
      :while (let [[condition body] (rest ast)]
               (assert (expression? condition))
               (assert (= :block (first body)))
               (emit-while (first (emit condition)) (emit body)))
      :break (emit-break)
      :continue (emit-continue)
      :def (let [[name params body] (rest ast)]
             (assert (= :block (first body)))
             (emit-def name params (emit body)))
      :return (let [value (second ast)]
                (assert (expression? value))
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

(defn switch-ns [ctx ns]
  (assert (core/simple-symbol? ns) "namespace name must be a simple symbol")
  (-> ctx
      (update :ns-registry
        assoc ns (get-in ctx [:ns-registry ns] {:bindings {}}))
      (assoc :current-ns ns)))

(defn munge-name [name]
  (assert (string? name))
  (cond
    (contains? SPECIAL-NAMES name)
    (munge-special-name name)

    (contains? RESERVED-NAMES name)
    (core/throw (str "name '" name "' is reserved"))

    :else
    (apply str
      (map
        (fn [c]
          (if (contains? MUNGE-MAP c)
            (get MUNGE-MAP c)
            c))
        name))))

(defn munge-symbol [sym]
  (assert (core/symbol? sym) "must be a symbol")
  (munge-name (core/str sym)))

(defn temp-name [counter]
  (assert (int? counter) "counter must be an integer")
  (str "___temp_" counter))

(defn gen-temp-name [ctx]
  [(temp-name (:counter ctx))
   (update ctx :counter inc)])

(defn globals [name]
  (assert (string? name))
  [:value (str "globals()[\"" name "\"]")])

(defn- throw-not-found [sym]
  (core/throw (core/str "'" sym "' not found")))

(defn resolve-symbol-name [ctx sym]
  (assert (core/symbol? sym) "must be a symbol")
  (assert (map? (:locals ctx)) "locals must be a map")
  (cond
    (some? (core/namespace sym))
    (let [sym-ns-name (core/namespace sym)
          sym-ns (get (:ns-registry ctx) (core/symbol sym-ns-name))]
      (if (some? sym-ns)
        (let [bindings (:bindings sym-ns)
              sym-name (core/name sym)
              simp-sym (core/symbol sym-name)]
          (assert (map? bindings) "namespace bindings must be a map")
          (if (contains? bindings simp-sym)
            (-> bindings (get simp-sym) :python-name)
            (throw-not-found sym)))
        (core/throw (str "namespace '" sym-ns-name "' not found"))))

    (contains? (:locals ctx) sym)
    (-> ctx :locals (get sym) :python-name)

    (some? (:current-ns ctx))
    (let [current-ns (get (:ns-registry ctx) (:current-ns ctx))
          bindings (:bindings current-ns)]
      (assert (some? current-ns) "current namespace not found")
      (assert (map? bindings) "namespace bindings must be a map")
      (if (contains? bindings sym)
        (-> bindings (get sym) :python-name)
        (throw-not-found sym)))

    :else
    (throw-not-found sym)))

(declare transform)

(defn transform-def [ctx args]
  (let [name (first args)
        value-form (second args)
        current-ns (:current-ns ctx)]
    (assert (some? current-ns) "no current namespace")
    (assert (core/simple-symbol? name)
      "def! expects a simple symbol as the first argument")
    (assert (= (count args) 2) "def! expects 2 arguments")
    (let [[val-expr val-body ctx2] (transform ctx value-form)
          munged (munge-symbol (core/symbol (core/name current-ns)
                                            (core/name name)))]
      [[:value munged]
       (concat
         val-body
         [[:assign (globals munged) val-expr]])
       (update-in ctx2
         [:ns-registry current-ns :bindings]
         assoc name {:python-name munged})])))

(defn transform-defmacro [ctx args]
  (let [name (first args)
        f (second args)]
    (assert (= (count args) 2) "defmacro! expects exactly 2 arguments")
    (assert (core/symbol? name)
      "defmacro! expects a symbol as the first argument")
    (assert (and (seq? f) (= 'fn* (first f)))
      "defmacro! expects fn* as the second argument")
    (let [[res body ctx*] (transform ctx f)
          current-ns (:current-ns ctx)
          _ (assert (some? current-ns) "no current namespace")
          python-name (munge-symbol (core/symbol (core/name current-ns)
                                                 (core/name name)))]
      [[:value python-name]
       (concat
         body
         [[:assign (globals python-name) res]
          [:call [:value "setattr"]
            [:value python-name]
            [:value "___is_mal_macro"]
            [:value "True"]]])
       (update-in ctx*
         [:ns-registry current-ns :bindings]
         assoc name {:python-name python-name})])))

(defn make-let-func [ctx bindings body]
  (loop [bindings* bindings
         fbody [:block]
         ctx* ctx]
    (if (empty? bindings*)
      (let [[body-res body-do ctx**] (transform ctx* body)]
        [(concat
           fbody
           body-do
           [[:return body-res]])
         (assoc ctx** :locals (:locals ctx))])
      (let [[name value] (first bindings*)
            _ (assert (core/simple-symbol? name)
                "binding name must be a simple symbol")
            python-name (munge-symbol name)
            [value-res value-do ctx**] (transform ctx* value)]
        (recur
          (rest bindings*)
          (concat
            fbody
            value-do
            [[:assign [:value python-name] value-res]])
          (update ctx** :locals assoc name {:python-name python-name}))))))

(defn transform-let [ctx args]
  (let [bindings-spec (first args)
        bindings (partition 2 bindings-spec)
        body-form (second args)
        [temp-func ctx2] (gen-temp-name ctx)
        [temp-func-body ctx3] (make-let-func
                                ctx2 bindings body-form)]
    (assert (< 0 (count args)) "no bindings provided")
    (assert (even? (count bindings-spec))
            "let* expects even number of forms in bindings")
    (assert (> 3 (count args))
            "let* expects only one form in body")
    [[:call temp-func]
     [[:def temp-func []
        temp-func-body]]
     ctx3]))

(defn transform-do [ctx args]
  (if (empty? args)
    [[:value "None"] nil ctx]
    (loop [args args
           body nil
           ctx ctx]
      (let [[res res-body ctx2] (transform ctx (first args))]
        (if (= 1 (count args))
          [res (concat body res-body) ctx2]
          (recur (rest args)
            (concat
              body
              res-body
              [res])
            ctx2))))))

(defn transform-if [ctx args]
  (let [[result ctx2] (gen-temp-name ctx)
        [cond cond-body ctx3] (transform ctx2 (first args))
        [then then-body ctx4] (transform ctx3 (second args))
        [else else-body ctx5] (transform ctx4 (nth args 2 nil))]
    (assert (> (count args) 1) "if expects at least 2 arguments")
    (assert (< (count args) 4) "if expects at most 3 arguments")
    [[:value result]
     (concat
       cond-body
       [[:if cond
          (cons :block
            (concat
              then-body
              [[:assign result then]]))
          nil
          (cons :block
            (concat
              else-body
              [[:assign result else]]))]])
     ctx5]))

(defn handle-fn-params [params]
  (loop [params params
         py-params []
         locals {}]
    (if (empty? params)
      [py-params locals]
      (let [param (first params)]
        (assert (core/simple-symbol? param)
          "function parameter must be a simple symbol")
        (if (= param '&)
          (let [variadic-param (second params)
                _ (assert (core/simple-symbol? variadic-param)
                    "variadic parameter must be a simple symbol")
                python-name (munge-symbol variadic-param)]
            (assert (= (count params) 2)
              "expected only one parameter after &")
            [(conj py-params (str "*" python-name))
             (assoc locals variadic-param {:python-name python-name})])
          (let [python-name (munge-symbol param)]
            (recur
              (rest params)
              (conj py-params python-name)
              (assoc locals param {:python-name python-name}))))))))

(defn transform-fn [ctx args]
  (let [params (first args)
        body (second args)
        [py-params locals] (handle-fn-params params)
        [func ctx2] (gen-temp-name ctx)
        [fbody-res fbody ctx3] (transform
                                 (update ctx2 :locals merge locals)
                                 body)]
    (assert (>= 2 (count args)) "fn* expects at most 2 arguments")
    [[:value func]
     [[:def func py-params
        (cons :block
          (concat fbody
            [[:return fbody-res]]))]]
     (assoc ctx3 :locals (:locals ctx))]))

(defn quote-expr [ctx x]
  (let [resolve* (fn [name]
                   (resolve-symbol-name ctx
                     (core/symbol "mal.core" name)))]
    (cond
      (nil? x) [:value "None"]
      (boolean? x) [:value (if x "True" "False")]
      (number? x) [:value (core/str x)]
      (string? x) [:value (core/pr-str x)]
      (core/keyword? x) [:call [:value (resolve* "keyword")]
                          [:value (core/pr-str (core/name x))]]
      (core/symbol? x) [:call [:value (resolve* "symbol")]
                         [:value (core/str "\"" x "\"")]]
      (list? x) (concat [:call [:value (resolve* "list")]]
                        (map #(quote-expr ctx %) x))
      (core/vector? x) (concat [:call [:value (resolve* "vector")]]
                               (map #(quote-expr ctx %) x))
      (core/map? x) (concat [:call [:value (resolve* "hash-map")]]
                            (apply concat
                              (sort
                                (map (fn [[k v]]
                                       [(quote-expr ctx k)
                                        (quote-expr ctx v)])
                                     x))))
      (set? x) (concat [:call [:value (resolve* "hash-set")]]
                       (sort (map #(quote-expr ctx %) x)))
      :else (core/throw
              (str "don't know how to quote this: "
                   (core/pr-str x))))))

(defn transform-call [ctx form]
  (let [head (first form)
        [head-res head-do ctx2] (transform ctx head)]
    (loop [args (rest form)
           ctx* ctx2
           args-res []
           args-do []]
      (if (empty? args)
        (if (= :value (first head-res))
          [(concat [:call head-res] args-res)
           (concat head-do args-do)
           ctx*]
          (let [[head-res-temp ctx**] (gen-temp-name ctx*)]
            [(concat [:call head-res-temp] args-res)
             (concat
               head-do
               [[:assign head-res-temp head-res]]
               args-do)
             ctx**]))
        (let [[arg-res arg-do ctx**] (transform ctx* (first args))]
          (recur (rest args)
                 ctx**
                 (conj args-res arg-res)
                 (concat args-do arg-do)))))))

(defn transform-try [ctx args]
  (let [catch-form? (fn [form]
                      (and (list? form)
                           (= 'catch* (first form))))
        [try-expr catch-form] (case (count args)
                                0 [nil nil]
                                1 (let [arg (first args)]
                                    (if (catch-form? arg)
                                      [nil arg]
                                      [arg nil]))
                                2 (do (assert (catch-form? (second args))
                                        (str "try* expects catch* as "
                                             "the second argument"))
                                      [(first args) (second args)])
                                (core/throw
                                  "try* expects at most 2 arguments"))]
    (if (some? catch-form)
      (let [ex-binding (second catch-form)
            _ (assert (core/simple-symbol? ex-binding)
                "exception object must be a simple symbol")
            ex-python-name (munge-symbol ex-binding)
            _ (assert (<= (count catch-form) 3)
                "catch* expects at most 2 arguments")
            catch-expr (nth catch-form 2 nil)
            [result ctx2] (gen-temp-name ctx)
            [tres tbody ctx3] (transform ctx2 try-expr)
            [cres cbody ctx4] (transform
                                (update ctx3 :locals
                                  assoc ex-binding
                                    {:python-name ex-python-name})
                                catch-expr)]
        [[:value result]
         [[:try
            (cons :block
              (concat
                tbody
                [[:assign [:value result] tres]]))
            [["Exception" ex-python-name
               (cons :block
                 (concat
                   cbody
                   [[:assign [:value result] cres]]))]]]]
         (assoc ctx4 :locals (:locals ctx))])
      (transform ctx try-expr))))

(defn transform-import [ctx specs]
  (assert (> (count specs) 0) "import expects at least one argument")
  (assert (some? (:current-ns ctx)) "import must be called in a namespace")
  (loop [specs* specs
         body []
         ctx1 ctx]
    (if (empty? specs*)
      [[:value "None"] body ctx1]
      (let [[module & names] (first specs*)
            _ (assert (core/simple-symbol? module)
                "module name must be a simple symbol")
            [imports ctx2] (gen-temp-name ctx1)
            body* (conj body
                    [:assign [:value imports]
                      [:call [:value "__import__"]
                        [:value (core/str "\"" module "\"")]]])
            ctx3 (update-in ctx2 [:ns-registry (:current-ns ctx2) :bindings]
                   merge (into {}
                           (map
                             (fn [name]
                               (assert (core/simple-symbol? name)
                                 "imported name must be a simple symbol")
                               [name {:python-name (str imports "."
                                                        (core/name name))}])
                             names)))]
        (recur (rest specs*) body* ctx3)))))

(defn transform
  "Transform lisp AST into python AST"
  [ctx form]
  {:pre [(instance? CompileContext ctx)]
   :post [(expression? (first %))
          (instance? CompileContext (nth % 2))]}
  (cond
    (list? form)
    (if (empty? form)
      [[:call (resolve-symbol-name ctx 'list)] nil ctx]
      (let [head (first form)
            args (rest form)]
        (condp = head
          'def! (transform-def ctx args)
          'defmacro! (transform-defmacro ctx args)
          'let* (transform-let ctx args)
          'do (transform-do ctx args)
          'if (transform-if ctx args)
          'fn* (transform-fn ctx args)
          'quote (let [value (first args)]
                   (assert (= 1 (count args))
                     "quote expects one argument")
                   [(quote-expr ctx value) nil ctx])
          'quasiquote (let [form* (first args)]
                        (assert (= 1 (count args))
                          (str "quasiquote expects exactly "
                               "one argument"))
                        (transform ctx
                          (core/expand-quasiquote form*)))
          'try* (transform-try ctx args)
          'catch* (core/throw "catch* used outside of try*")
          'import (transform-import ctx args)
          (transform-call ctx form))))

    (core/symbol? form)
    [[:value (resolve-symbol-name ctx form)] nil ctx]

    (vector? form)
    (core/throw "not impelemented")

    (map? form)
    (core/throw "not implemented")

    :else
    [(quote-expr ctx form) nil ctx]))
