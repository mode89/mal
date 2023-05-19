(ns mal.test.utils
  (:require [mal.core :as core]))

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
                            [(:name ns-name) (sample-namespace ns)]))
                     (into {}))
   :current-ns (-> ctx core/deref :current-ns :name :name)})

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

(defn sym$ [name]
  (assert (string? name))
  (core/symbol name))

(defn kw$ [name]
  (assert (string? name))
  (core/keyword name))

(defn quote$ [form]
  (list (sym$ "quote") form))

(defn qq$ [form]
  (list (sym$ "quasiquote") form))

(defn unq$ [form]
  (list (sym$ "unquote") form))

(defn spunq$ [form]
  (list (sym$ "splice-unquote") form))

(defn def$ [name value]
  (assert (string? name))
  (list (sym$ "def!") (sym$ name) value))

(defn let$ [bindings body]
  (list (sym$ "let*") bindings body))

(defn if$
  ([pred then]
    (list (sym$ "if") pred then))
  ([pred then else]
    (list (sym$ "if") pred then else)))

(defn fn$
  ([params]
    (list (sym$ "fn*") params))
  ([params body]
    (list (sym$ "fn*") params body)))

(defn defmacro$ [name value]
  (assert (string? name))
  (list (sym$ "defmacro!") (sym$ name) value))

(defn do$ [& forms]
  (apply list (concat [(sym$ "do")] forms)))

(defn cons$ [x xs]
  (list (sym$ "cons") x xs))

(defn concat$ [& xs]
  (apply list (concat [(sym$ "mal.core/concat")] xs)))

(defn list$ [& xs]
  (cons (sym$ "mal.core/list") xs))

(defn try$
  ([expr]
    (list (sym$ "try*") expr))
  ([expr ex catch-expr]
    (assert (string? ex))
    (list (sym$ "try*") expr
      (list (sym$ "catch*") (sym$ ex)
        catch-expr))))

(defn throw$ [obj]
  (list (sym$ "throw*") obj))

(defmacro thrown-with-msg* [message & body]
  `(re-find ~message
     (try ~@body
       (catch Error ex#
         (.getMessage ex#))
       (catch Exception ex#
         (if (mal.core/object-exception? ex#)
           (mal.core/object-exception-unwrap ex#)
           (.getMessage ex#))))))
