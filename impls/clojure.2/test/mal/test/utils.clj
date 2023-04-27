(ns mal.test.utils
  (:require [mal.core :as core]
            [mal.types :as types]))

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
  (types/->Namespace (core/symbol name) (core/atom bindings)))

(defn mock-eval-context [& {:keys [ns-registry current-ns]}]
  (let [registry (into {}
                   (map (fn [[name bindings]]
                          [(core/symbol name) (mock-ns name bindings)])
                        ns-registry))]
    (core/atom
      (types/->EvalContext
        (core/atom registry)
        (when (some? current-ns)
          (get registry (core/symbol current-ns)))))))
