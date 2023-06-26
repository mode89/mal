(in-ns 'mal.main)

(clojure.core/load "clojure/core")
(clojure.core/refer 'mal.core :only
  '[apply atom cons concat defn doseq empty? first get let loop
    map->EvalContext println rest second slurp str symbol = -> CORE-NS])

(clojure.core/require
  '[clojure.stacktrace :refer [print-stack-trace]]
  '[mal.python.compiler :as pyc]
  '[mal.reader :refer [read-string*]])

(def COLOR-LUT
  {:blue "\033[34m"
   :red "\033[31m"
   :green "\033[32m"
   :yellow "\033[33m"})

(defn color [c & text]
  (apply str (concat [(get COLOR-LUT c)] text ["\033[0m"])))

(defn -main [& args]
  (try
    (let [content (slurp "src/mal/python/core.clj")
          eval-ctx (map->EvalContext
                     {:ns-registry (atom {(:name CORE-NS) CORE-NS})
                      :current-ns CORE-NS})]
      (loop [forms (read-string* content)
             comp-ctx (pyc/map->CompileContext
                        {:ns-registry {}
                         :current-ns nil
                         :locals {}
                         :counter 10000})]
        (if (empty? forms)
          (println (color :green "******** Done ********"))
          (let [form (first forms)
                head (first form)]
            (if (= 'in-ns head)
              (let [ns (-> form second second)]
                (recur (rest forms) (pyc/switch-ns comp-ctx ns)))
              (let [[result body comp-ctx*] (pyc/transform comp-ctx form)]
                (println "form:" form)
                (println "body:" body)
                (println "result:" result)
                (doseq [line (pyc/emit (cons :block body))]
                  (println (color :blue line)))
                (println (color :blue (first (pyc/emit result))))
                (recur (rest forms) comp-ctx*)))))))
    (catch Throwable ex
      (print-stack-trace ex))))
