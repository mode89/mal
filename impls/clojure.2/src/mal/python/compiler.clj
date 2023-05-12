(ns mal.python.compiler
  (:require [clojure.set :refer [union]]
            [clojure.string :refer [join triml]]
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

(defn expression? [ast]
  (let [tag (first ast)]
    (or (= :value tag)
        (= :call tag))))

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
                (assert (expression? value))
                (emit-assign name (first (emit value))))
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
  (core/throw (core/str "'" sym "' not found")))

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

(declare transform)

(defn transform-def [ctx args]
  (let [name (first args)
        value-form (second args)]
    (assert (core/symbol? name)
            "def! expects a symbol as the first argument")
    (assert (= (count args) 2) "def! expects 2 arguments")
    (let [[val-expr val-body ctx2] (transform ctx value-form)
          current-ns (:current-ns ctx)]
      (assert (some? current-ns) "no current namespace")
      [[:value (mangle name)]
       (conj val-body [:assign (globals (mangle name)) val-expr])
       (update-in ctx2
         [:ns-registry current-ns :bindings]
         conj name)])))

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
            [value-res value-do ctx**] (transform ctx* value)]
        (assert (core/simple-symbol? name)
          "binding name must be a simple symbol")
        (recur
          (rest bindings*)
          (concat
            fbody
            value-do
            [[:assign (mangle name) value-res]])
          (update ctx** :locals conj name))))))

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
        [else else-body ctx5] (transform ctx4
                                (when (> (count args) 2)
                                  (nth args 2)))]
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
         locals #{}]
    (if (empty? params)
      [py-params locals]
      (let [param (first params)]
        (assert (core/simple-symbol? param)
          "function parameter must be a simple symbol")
        (if (= (:name param) "&")
          (let [var-params (second params)]
            (assert (= (count params) 2)
              "expected only one parameter after &")
            (assert (core/simple-symbol? var-params)
              "variadic parameter must be a simple symbol")
            [(conj py-params
                   (str "*" (mangle var-params)))
             (conj locals var-params)])
          (recur
            (rest params)
            (conj py-params (mangle param))
            (conj locals param)))))))

(defn transform-fn [ctx args]
  (let [params (first args)
        body (second args)
        [py-params locals] (handle-fn-params params)
        [func ctx2] (gen-temp-name ctx)
        [fbody-res fbody ctx3] (transform
                                 (update ctx2
                                   :locals union locals)
                                 body)]
    (assert (>= 2 (count args)) "fn* expects at most 2 arguments")
    [[:value func]
     [[:def func py-params
        (cons :block
          (concat fbody
            [[:return fbody-res]]))]]
     (assoc ctx3 :locals (:locals ctx))]))

(defn quote-expr [x]
  (cond
    (nil? x) [:value "None"]
    (boolean? x) [:value (if x "True" "False")]
    (number? x) [:value (str x)]
    (string? x) [:value (core/pr-str x)]
    (core/keyword? x) [:call [:value "keyword"]
                        [:value (core/pr-str (:name x))]]
    (core/symbol? x) [:call [:value "symbol"]
                       [:value (str "\"" (core/pr-str x) "\"")]]
    (core/list? x) (concat [:call [:value "list"]] (map quote-expr x))
    (core/vector? x) (concat [:call [:value "vector"]] (map quote-expr x))
    (core/map? x) (concat [:call [:value "hash_map"]]
                          (apply concat
                            (sort
                              (map (fn [[k v]]
                                     [(quote-expr k) (quote-expr v)])
                                   x))))
    (set? x) (concat [:call [:value "hash_set"]] (sort (map quote-expr x)))
    :else (core/throw
            (str "don't know how to quote this: "
                 (core/pr-str x)))))

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

(defn transform
  "Transform lisp AST into python AST"
  [ctx form]
  {:pre [(instance? CompileContext ctx)]
   :post [(expression? (first %))
          (instance? CompileContext (nth % 2))]}
  (cond
    (list? form)
    (if (empty? form)
      [[:value "list()"] nil ctx]
      (let [head (first form)
            args (rest form)]
        (condp = head
          (core/symbol "def!") (transform-def ctx args)
          (core/symbol "let*") (transform-let ctx args)
          (core/symbol "do") (transform-do ctx args)
          (core/symbol "if") (transform-if ctx args)
          (core/symbol "fn*") (transform-fn ctx args)
          (core/symbol "quote") (let [value (first args)]
                                  (assert (= 1 (count args))
                                    "quote expects one argument")
                                  [(quote-expr value) nil ctx])
          (transform-call ctx form))))

    (core/symbol? form)
    [[:value (resolve-symbol-name ctx form)] nil ctx]

    (nil? form)
    [[:value "None"] nil ctx]

    (boolean? form)
    [[:value (if form "True" "False")] nil ctx]

    :else
    [[:value (pr-str form)] nil ctx]))
