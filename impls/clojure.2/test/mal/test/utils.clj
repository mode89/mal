(ns mal.test.utils)

(alias 'core 'mal.core)

(defmacro catch-ex-info [& body]
  `(try
     ~@body
     (catch clojure.lang.ExceptionInfo e#
       [(ex-message e#) (ex-data e#)])))

(defmacro is-list? [expr value]
  `(do (clojure.test/is (mal.core/list? ~expr))
       (clojure.test/is (= ~value ~expr))))

(defmacro is-vector? [expr value]
  `(do (clojure.test/is (mal.core/vector? ~expr))
       (clojure.test/is (= ~value ~expr))))

(defn sample-namespace [ns]
  (-> ns :bindings core/deref))

(defn sample-eval-context [ctx]
  {:ns-registry (->> ctx
                     core/deref
                     :ns-registry
                     core/deref
                     (map (fn [[ns-name ns]]
                            [(core/name ns-name) (sample-namespace ns)]))
                     (into {}))
   :current-ns (when-some [ns (-> ctx core/deref :current-ns)]
                 (core/name (:name ns)))})

(defn mock-ns [name bindings]
  (core/->Namespace (core/symbol name) (core/atom bindings)))

(defn mock-eval-context [& {:keys [ns-registry current-ns]}]
  (let [registry (into {}
                   (map (fn [[name bindings]]
                          [(core/symbol name) (mock-ns name bindings)])
                        ns-registry))]
    (core/atom
      (core/->EvalContext
        (core/atom registry)
        (when (some? current-ns)
          (get registry (core/symbol current-ns)))))))

(defn quote$ [form]
  (list 'quote form))

(defn qq$ [form]
  (list 'quasiquote form))

(defn unq$ [form]
  (list 'unquote form))

(defn spunq$ [form]
  (list 'splice-unquote form))

(defn def$ [name value]
  (assert (string? name))
  (list 'def! (core/symbol name) value))

(defn let$ [bindings body]
  (list 'let* bindings body))

(defn if$
  ([pred then]
    (list 'if pred then))
  ([pred then else]
    (list 'if pred then else)))

(defn fn$
  ([params]
    (list 'fn* params))
  ([params body]
    (list 'fn* params body)))

(defn defmacro$ [name value]
  (assert (string? name))
  (list 'defmacro! (core/symbol name) value))

(defn do$ [& forms]
  (apply list (concat ['do] forms)))

(defn cons$ [x xs]
  (list 'cons x xs))

(defn concat$ [& xs]
  (apply list (concat ['mal.core/concat] xs)))

(defn list$ [& xs]
  (cons 'mal.core/list xs))

(defn try$
  ([expr]
    (list 'try* expr))
  ([expr ex catch-expr]
    (assert (string? ex))
    (list 'try* expr
      (list 'catch* (core/symbol ex)
        catch-expr))))

(defn throw$ [obj]
  (list 'throw* obj))

(defmacro thrown-with-msg* [message & body]
  `(re-find ~message
     (try ~@body
       (catch Error ex#
         (.getMessage ex#))
       (catch Exception ex#
         (if (mal.core/object-exception? ex#)
           (mal.core/object-exception-unwrap ex#)
           (.getMessage ex#))))))
