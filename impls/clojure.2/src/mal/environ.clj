(ns mal.environ
  (:refer-clojure :exclude [get set!]))

(defn make [outer bindings]
  (atom {:outer outer
         :table bindings}))

(defn set! [env key value]
  (swap! env assoc-in [:table key] value))

(defn get [env key]
  (let [e @env
        table (:table e)
        outer (:outer e)]
    (if (contains? table key)
      (clojure.core/get table key)
      (if (some? outer)
        (mal.environ/get outer key)
        (throw (new Exception
                    (str "Symbol '" (:name key) "' not found")))))))
