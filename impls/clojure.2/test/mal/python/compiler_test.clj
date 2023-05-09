(ns mal.python.compiler-test
  (:require [clojure.test :refer [deftest is]]
            [mal.core :as core]
            [mal.python.compiler :as c]
            [mal.test.utils :refer [def$ do$ let$ sym$]]))

(defn mock-compile-context [& {:keys [ns-registry
                                      current-ns
                                      locals
                                      counter]}]
  (assert (or (nil? locals) (set? locals)))
  (let [registry (into {}
                   (map (fn [[name bindings]]
                          (assert (set? bindings))
                          [(core/symbol name)
                           {:name name
                            :bindings (into #{}
                                        (map core/symbol bindings))}])
                        ns-registry))]
    (c/map->CompileContext
      {:ns-registry registry
       :current-ns (when (some? current-ns)
                     (assert (contains? ns-registry current-ns))
                     (core/symbol current-ns))
       :locals (into #{} (map core/symbol locals))
       :counter (if (some? counter) counter 0)})))

(deftest emit-assign
  (is (= (c/emit-assign "a" "b") ["a = b"])))

(deftest emit-call
  (is (= (c/emit-call "foo" ["a" "b"] {}) ["foo(a, b)"]))
  (is (= (c/emit-call "foo" (list "a" "b") {}) ["foo(a, b)"]))
  (is (= (c/emit-call "foo" ["a" "b"] {"c" "d" "e" "f"})
         ["foo(a, b, c=d, e=f)"]))
  (is (= (c/emit-call "foo" ["a" "b"] {"e" "f" "c" "d"})
         ["foo(a, b, c=d, e=f)"])))

(deftest emit-if
  (is (= (c/emit-if "a" ["b"] nil nil) ["if a:" "  b"]))
  (is (= (c/emit-if "a" ["b"] nil ["c"])
         ["if a:"
          "  b"
          "else:"
          "  c"]))
  (is (= (c/emit-if "a" ["b"] [["c" ["d"]]] nil)
         ["if a:"
          "  b"
          "elif c:"
          "  d"]))
  (is (= (c/emit-if "a" ["b"] [["c" ["d"]]] ["e"])
         ["if a:"
          "  b"
          "elif c:"
          "  d"
          "else:"
          "  e"]))
  (is (= (c/emit-if "a" ["b"] [["c" ["d"]] ["e" ["f"]]] nil)
         ["if a:"
          "  b"
          "elif c:"
          "  d"
          "elif e:"
          "  f"]))
  (is (= (c/emit-if "a" ["b"] [["c" ["d"]] ["e" ["f"]]] ["g"])
         ["if a:"
          "  b"
          "elif c:"
          "  d"
          "elif e:"
          "  f"
          "else:"
          "  g"]))
  (is (= (let [then (c/emit-if "a" ["b"] nil ["c"])
               else (c/emit-if "d" ["e"] nil ["f"])]
           (c/emit-if "g" then nil else))
         ["if g:"
          "  if a:"
          "    b"
          "  else:"
          "    c"
          "else:"
          "  if d:"
          "    e"
          "  else:"
          "    f"])))

(deftest emit-while
  (is (= (c/emit-while "a" ["b"]) ["while a:" "  b"])))

(deftest emit-break
  (is (= (c/emit-break) ["break"])))

(deftest emit-continue
  (is (= (c/emit-continue) ["continue"])))

(deftest emit-def
  (is (= (c/emit-def "a" ["b"] ["c"]) ["def a(b):" "  c"]))
  (is (= (c/emit-def "a" ["b" "c"] ["d"]) ["def a(b, c):" "  d"])))

(deftest emit-return
  (is (= (c/emit-return "a") ["return a"])))

(deftest emit-try
  (is (= (c/emit-try ["a"] nil nil) ["try:" "  a"]))
  (is (= (c/emit-try ["a"] nil ["b"])
         ["try:"
          "  a"
          "finally:"
          "  b"]))
  (is (= (c/emit-try ["a"] [["b" nil ["c"]]] nil)
         ["try:"
          "  a"
          "except b:"
          "  c"]))
  (is (= (c/emit-try ["a"] [["b" "c" ["d"]]] nil)
         ["try:"
          "  a"
          "except b as c:"
          "  d"]))
  (is (= (c/emit-try ["a"] [["b" "c" ["d"]] ["e" nil ["f"]]] nil)
         ["try:"
          "  a"
          "except b as c:"
          "  d"
          "except e:"
          "  f"]))
  (is (= (c/emit-try ["a"] [["b" nil ["c"]] ["d" "e" ["f"]]] ["g"])
         ["try:"
          "  a"
          "except b:"
          "  c"
          "except d as e:"
          "  f"
          "finally:"
          "  g"])))

(deftest emit
  (is (= (c/emit [:assign "a" [:value "b"]]) ["a = b"]))
  (is (= (c/emit [:call "foo" [[:value "a"] [:value "b"]]
                              {"c" [:value "42"]}])
         ["foo(a, b, c=42)"]))
  (is (= (c/emit [:block
                   [:assign "a" [:value "b"]]
                   [:call "foo" [[:value "a"] [:value "b"]]
                                {"c" [:value "42"]}]])
         ["a = b"
          "foo(a, b, c=42)"]))
  (is (= (c/emit [:block
                   [:block
                     [:assign "a" [:value "b"]]
                     [:assign "c" [:value "d"]]]
                   [:block
                     [:assign "e" [:value "f"]]
                     [:assign "g" [:value "h"]]]])
         ["a = b"
          "c = d"
          "e = f"
          "g = h"]))
  (is (= (c/emit [:if [:value "a"]
                   [:block
                     [:assign "b" [:value "c"]]] nil nil])
         ["if a:" "  b = c"]))
  (is (= (c/emit [:if [:value "a"]
                   [:block
                     [:assign "b" [:value "c"]]]
                   nil
                   [:block
                     [:assign "d" [:value "e"]]]])
         ["if a:"
          "  b = c"
          "else:"
          "  d = e"]))
  (is (= (c/emit [:if [:value "cond1"]
                   [:block
                     [:assign "a" [:value "b"]]
                     [:call "foo" [[:value "a"] [:value "b"]] {}]]
                   [[[:value "cond2"]
                       [:block
                         [:assign "f" [:value "g"]]
                         [:assign "h" [:value "i"]]]]
                    [[:value "cond3"]
                       [:block
                         [:call "baz" [[:value "j"] [:value "k"]] {}]
                         [:assign "l" [:value "m"]]]]]
                   [:block
                     [:call "bar" [[:value "x"] [:value "y"]]
                                  {"z" [:value "42"]}]
                     [:assign "p" [:value "q"]]]])
         ["if cond1:"
          "  a = b"
          "  foo(a, b)"
          "elif cond2:"
          "  f = g"
          "  h = i"
          "elif cond3:"
          "  baz(j, k)"
          "  l = m"
          "else:"
          "  bar(x, y, z=42)"
          "  p = q"]))
  (is (= (c/emit [:while [:value "a"]
                   [:block
                     [:assign "b" [:value "c"]]]])
         ["while a:" "  b = c"]))
  (is (= (c/emit [:while [:value "a"]
                   [:block
                     [:assign "b" [:value "c"]]
                     [:if [:call "foo"
                            [[:value "a"] [:value "b"]]
                            {"c" [:value "42"]}]
                       [:block
                         [:break]]
                       nil
                       [:block
                         [:continue]]]]])
         ["while a:"
          "  b = c"
          "  if foo(a, b, c=42):"
          "    break"
          "  else:"
          "    continue"]))
  (is (= (c/emit [:def "foo" ["a" "b"]
                   [:block
                     [:assign "c" [:value "d"]]
                     [:return [:value "e"]]]])
         ["def foo(a, b):"
          "  c = d"
          "  return e"]))
  (is (= (c/emit [:try
                   [:block
                     [:assign "a" [:value "b"]]]
                   nil
                   [:block
                     [:assign "c" [:value "d"]]]])
         ["try:"
          "  a = b"
          "finally:"
          "  c = d"]))
  (is (= (c/emit [:try
                   [:block
                     [:assign "a" [:value "b"]]
                     [:call "foo" [[:value "a"] [:value "b"]]
                                  {"c" [:value "42"]}]]
                   [["Exception" nil
                      [:block
                        [:assign "d" [:value "e"]]
                        [:return [:value "f"]]]]
                    ["ValueError" "e"
                      [:block
                        [:assign "g" [:value "h"]]
                        [:return [:value "i"]]]]
                    ["TypeError" "e"
                      [:block
                        [:assign "j" [:value "k"]]]]]
                   [:block
                     [:assign "l" [:value "m"]]
                     [:return [:value "n"]]]])
         ["try:"
          "  a = b"
          "  foo(a, b, c=42)"
          "except Exception:"
          "  d = e"
          "  return f"
          "except ValueError as e:"
          "  g = h"
          "  return i"
          "except TypeError as e:"
          "  j = k"
          "finally:"
          "  l = m"
          "  return n"])))

(deftest mangle
  (is (= "foo" (c/mangle (sym$ "foo"))))
  (is (= "foo.bar.baz" (c/mangle (sym$ "foo.bar/baz"))))
  (is (re-find #"must be a symbol"
        (try (c/mangle "foo")
          (catch Error e (.getMessage e))))))

(deftest resolve-symbol-name
  (is (= "foo" (c/resolve-symbol-name
                 (mock-compile-context :locals #{"foo"})
                 (sym$ "foo"))))
  (is (= "bar" (c/resolve-symbol-name
                 (mock-compile-context
                   :ns-registry {"foo" #{"bar"}}
                   :current-ns "foo")
                 (sym$ "bar"))))
  (is (= "baz.qux" (c/resolve-symbol-name
                     (mock-compile-context
                       :ns-registry {"foo" #{"bar"}
                                     "baz" #{"qux"}}
                       :current-ns "foo")
                     (sym$ "baz/qux"))))
  (is (re-find #"must be a symbol"
        (try (c/resolve-symbol-name
               (mock-compile-context :locals #{"foo"})
               "foo")
          (catch Error e (.getMessage e)))))
  (is (re-find #"locals must be a set"
        (try (c/resolve-symbol-name
               (c/map->CompileContext
                 {:ns-registry nil
                  :current-ns nil
                  :locals ["foo"]
                  :counter 0})
               (sym$ "foo"))
          (catch Error e (.getMessage e)))))
  (is (re-find #"'bar' not found"
        (try (c/resolve-symbol-name
               (mock-compile-context
                 :ns-registry {"foo" #{"baz"}}
                 :current-ns "foo")
               (sym$ "bar"))
          (catch Exception e (core/object-exception-unwrap e)))))
  (is (re-find #"'baz/bar' not found"
        (try (c/resolve-symbol-name
               (mock-compile-context
                 :ns-registry {"foo" #{"bar"}
                               "baz" #{"qux"}}
                 :current-ns "foo")
               (sym$ "baz/bar"))
          (catch Exception e (core/object-exception-unwrap e)))))
  (is (re-find #"namespace 'baz' not found"
        (try (c/resolve-symbol-name
               (mock-compile-context
                 :ns-registry {"foo" #{"bar"}}
                 :current-ns "foo")
               (sym$ "baz/bar"))
          (catch Exception e (core/object-exception-unwrap e)))))
  (is (re-find #"'fred' not found"
        (try (c/resolve-symbol-name
               (mock-compile-context
                 :ns-registry {"foo" #{"bar"}}
                 :current-ns nil
                 :locals #{"baz" "qux"})
               (sym$ "fred"))
          (catch Exception e (core/object-exception-unwrap e)))))
  (is (re-find #"namespace bindings must be a set"
        (try (c/resolve-symbol-name
               (c/map->CompileContext
                 {:ns-registry {(sym$ "foo") [(sym$ "bar")]}
                  :current-ns (sym$ "foo")
                  :locals #{}
                  :counter 0})
               (sym$ "bar"))
          (catch Error e (.getMessage e)))))
  (is (re-find #"namespace bindings must be a set"
        (try (c/resolve-symbol-name
               (c/map->CompileContext
                 {:ns-registry {(sym$ "foo") #{}
                                (sym$ "bar") [(sym$ "baz")]}
                  :current-ns (sym$ "foo")
                  :locals #{}
                  :counter 0})
               (sym$ "bar/baz"))
          (catch Error e (.getMessage e)))))
  (is (re-find #"current namespace not found"
        (try (c/resolve-symbol-name
               (c/map->CompileContext
                 {:ns-registry {(sym$ "foo") #{(sym$ "bar")}}
                  :current-ns (sym$ "baz")
                  :locals #{}
                  :counter 0})
               (sym$ "bar"))
          (catch Error e (.getMessage e))))))

(deftest transform
  (let [ctx (mock-compile-context)]
    (is (= [[:value "42"] nil ctx] (c/transform ctx 42)))
    (is (= [[:value "\"42\""] nil ctx] (c/transform ctx "42")))
    (is (= [[:value "None"] nil ctx] (c/transform ctx nil)))
    (is (= [[:value "True"] nil ctx] (c/transform ctx true)))
    (is (= [[:value "False"] nil ctx] (c/transform ctx false))))
  (let [ctx (mock-compile-context :locals #{"foo"})]
    (is (= [[:value "foo"] nil ctx] (c/transform ctx (sym$ "foo"))))))

(deftest transform-def
  (let [ctx (mock-compile-context
              :ns-registry {"foo" #{}}
              :current-ns "foo")]
    (is (= [[:value "bar"]
            [[:assign (c/globals "bar") [:value "42"]]]
            (mock-compile-context
              :ns-registry {"foo" #{"bar"}}
              :current-ns "foo")]
           (c/transform ctx (def$ "bar" 42)))))
  (is (re-find #"def! expects a symbol as the first argument"
        (try (c/transform
               (mock-compile-context
                 :ns-registry {"foo" #{}}
                 :current-ns "foo")
               (list (sym$ "def!") "bar" 42))
          (catch Error e (.getMessage e)))))
  (is (re-find #"def! expects 2 arguments"
        (try (c/transform
               (mock-compile-context
                 :ns-registry {"foo" #{}}
                 :current-ns "foo")
               (list (sym$ "def!") (sym$ "bar")))
          (catch Error e (.getMessage e)))))
  (is (re-find #"def! expects 2 arguments"
        (try (c/transform
               (mock-compile-context
                 :ns-registry {"foo" #{}}
                 :current-ns "foo")
               (list (sym$ "def!") (sym$ "bar") 42 43))
          (catch Error e (.getMessage e)))))
  (is (re-find #"no current namespace"
        (try (c/transform
               (mock-compile-context
                 :ns-registry {"foo" #{}})
               (def$ "bar" 42))
          (catch Error e (.getMessage e))))))

(deftest transform-do
  (let [ctx (mock-compile-context
              :ns-registry {"foo" #{}}
              :current-ns "foo")]
    (is (= [[:value "None"] nil ctx] (c/transform ctx (do$))))
    (is (= [[:value "bar"]
            [[:assign (c/globals "bar") [:value "42"]]]
            (mock-compile-context
              :ns-registry {"foo" #{"bar"}}
              :current-ns "foo")]
           (c/transform ctx (do$ (def$ "bar" 42)))))
    (is (= [[:value "baz"]
            [[:assign (c/globals "bar") [:value "42"]]
             [:value "bar"]
             [:assign (c/globals "baz") [:value "43"]]]
            (mock-compile-context
              :ns-registry {"foo" #{"bar" "baz"}}
              :current-ns "foo")]
           (c/transform ctx
             (do$ (def$ "bar" 42)
                  (def$ "baz" 43)))))))

(deftest transform-let
  (let [ctx (mock-compile-context
              :ns-registry {"foo" #{}}
              :current-ns "foo")]
    (is (= [[:value (c/temp-name 0)]
            [[:def (c/temp-name 1) []
               [:block [:return [:value "None"]]]]
             [:assign (c/temp-name 0) [:call (c/temp-name 1) nil {}]]]
            (assoc ctx :counter 2)]
           (c/transform ctx (list (sym$ "let*") []))))
    (is (= [[:value (c/temp-name 0)]
            [[:def (c/temp-name 1) []
               [:block
                 [:assign "a" [:value "42"]]
                 [:return [:value "a"]]]]
             [:assign (c/temp-name 0) [:call (c/temp-name 1) nil {}]]]
            (assoc ctx :counter 2)]
           (c/transform ctx
             (let$ [(sym$ "a") 42]
               (sym$ "a")))))
    (is (= [[:value (c/temp-name 0)]
            [[:def (c/temp-name 1) []
               [:block
                 [:assign (c/globals "b") [:value "42"]]
                 [:assign "a" [:value "b"]]
                 [:assign (c/globals "d") [:value "43"]]
                 [:assign "c" [:value "d"]]
                 [:value "a"]
                 [:return [:value "c"]]]]
             [:assign (c/temp-name 0) [:call (c/temp-name 1) nil {}]]]
            (mock-compile-context
              :ns-registry {"foo" #{"b" "d"}}
              :current-ns "foo"
              :counter 2)]
           (c/transform ctx
             (let$ [(sym$ "a") (def$ "b" 42)
                    (sym$ "c") (def$ "d" 43)]
               (do$
                 (sym$ "a")
                 (sym$ "c"))))))
    (is (= [[:value (c/temp-name 0)]
            [[:def (c/temp-name 1) []
               [:block
                 [:assign "a" [:value "42"]]
                 [:assign "b" [:value "a"]]
                 [:return [:value "b"]]]]
             [:assign (c/temp-name 0) [:call (c/temp-name 1) nil {}]]]
            (assoc ctx :counter 2)]
           (c/transform ctx
             (let$ [(sym$ "a") 42
                    (sym$ "b") (sym$ "a")]
               (sym$ "b")))))
    (is (re-find #"expects even number of forms"
          (try (c/transform ctx
                 (let$ [(sym$ "a") 42
                        (sym$ "b")]
                   (sym$ "a")))
            (catch Error e (.getMessage e)))))
    (is (re-find #"no bindings provided"
          (try (c/transform ctx
                 (list (sym$ "let*")))
            (catch Error e (.getMessage e)))))
    (is (re-find #"expects only one form in body"
          (try (c/transform ctx
                 (list (sym$ "let*") [(sym$ "a") 42
                                      (sym$ "b") 43]
                       (sym$ "a")
                       (sym$ "b")))
            (catch Error e (.getMessage e)))))
    (is (re-find #"binding name must be a simple symbol"
          (try (c/transform ctx
                 (let$ ["a" 42]
                   (sym$ "a")))
            (catch Error e (.getMessage e)))))
    (is (re-find #"binding name must be a simple symbol"
          (try (c/transform ctx
                 (let$ [(sym$ "bar/qux") 42]
                   (sym$ "bar/qux")))
            (catch Error e (.getMessage e)))))
    (is (re-find #"'b' not found"
          (try (c/transform ctx
                 (let$ [(sym$ "a") 42]
                   (sym$ "b")))
            (catch Exception e (core/object-exception-unwrap e)))))))
