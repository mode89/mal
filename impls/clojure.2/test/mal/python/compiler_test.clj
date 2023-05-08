(ns mal.python.compiler-test
  (:require [clojure.test :refer [deftest is]]
            [mal.core :as core]
            [mal.python.compiler :as c]
            [mal.test.utils :refer [def$ do$ sym$]]))

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
  (is (= (c/emit [:assign "a" [:expr "b"]]) ["a = b"]))
  (is (= (c/emit [:call "foo" [[:expr "a"] [:expr "b"]]
                              {"c" [:expr "42"]}])
         ["foo(a, b, c=42)"]))
  (is (= (c/emit [:block
                   [:assign "a" [:expr "b"]]
                   [:call "foo" [[:expr "a"] [:expr "b"]]
                                {"c" [:expr "42"]}]])
         ["a = b"
          "foo(a, b, c=42)"]))
  (is (= (c/emit [:block
                   [:block
                     [:assign "a" [:expr "b"]]
                     [:assign "c" [:expr "d"]]]
                   [:block
                     [:assign "e" [:expr "f"]]
                     [:assign "g" [:expr "h"]]]])
         ["a = b"
          "c = d"
          "e = f"
          "g = h"]))
  (is (= (c/emit [:if [:expr "a"]
                   [:block
                     [:assign "b" [:expr "c"]]] nil nil])
         ["if a:" "  b = c"]))
  (is (= (c/emit [:if [:expr "a"]
                   [:block
                     [:assign "b" [:expr "c"]]]
                   nil
                   [:block
                     [:assign "d" [:expr "e"]]]])
         ["if a:"
          "  b = c"
          "else:"
          "  d = e"]))
  (is (= (c/emit [:if [:expr "cond1"]
                   [:block
                     [:assign "a" [:expr "b"]]
                     [:call "foo" [[:expr "a"] [:expr "b"]] {}]]
                   [[[:expr "cond2"]
                       [:block
                         [:assign "f" [:expr "g"]]
                         [:assign "h" [:expr "i"]]]]
                    [[:expr "cond3"]
                       [:block
                         [:call "baz" [[:expr "j"] [:expr "k"]] {}]
                         [:assign "l" [:expr "m"]]]]]
                   [:block
                     [:call "bar" [[:expr "x"] [:expr "y"]]
                                  {"z" [:expr "42"]}]
                     [:assign "p" [:expr "q"]]]])
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
  (is (= (c/emit [:while [:expr "a"]
                   [:block
                     [:assign "b" [:expr "c"]]]])
         ["while a:" "  b = c"]))
  (is (= (c/emit [:while [:expr "a"]
                   [:block
                     [:assign "b" [:expr "c"]]
                     [:if [:expr [:call "foo"
                                   [[:expr "a"] [:expr "b"]]
                                   {"c" [:expr "42"]}]]
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
                     [:assign "c" [:expr "d"]]
                     [:return [:expr "e"]]]])
         ["def foo(a, b):"
          "  c = d"
          "  return e"]))
  (is (= (c/emit [:try
                   [:block
                     [:assign "a" [:expr "b"]]]
                   nil
                   [:block
                     [:assign "c" [:expr "d"]]]])
         ["try:"
          "  a = b"
          "finally:"
          "  c = d"]))
  (is (= (c/emit [:try
                   [:block
                     [:assign "a" [:expr "b"]]
                     [:call "foo" [[:expr "a"] [:expr "b"]]
                                  {"c" [:expr "42"]}]]
                   [["Exception" nil
                      [:block
                        [:assign "d" [:expr "e"]]
                        [:return [:expr "f"]]]]
                    ["ValueError" "e"
                      [:block
                        [:assign "g" [:expr "h"]]
                        [:return [:expr "i"]]]]
                    ["TypeError" "e"
                      [:block
                        [:assign "j" [:expr "k"]]]]]
                   [:block
                     [:assign "l" [:expr "m"]]
                     [:return [:expr "n"]]]])
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
    (is (= [[:expr "42"] nil ctx] (c/transform ctx 42)))
    (is (= [[:expr "\"42\""] nil ctx] (c/transform ctx "42"))))
  (let [ctx (mock-compile-context :locals #{"foo"})]
    (is (= [[:expr "foo"] nil ctx] (c/transform ctx (sym$ "foo"))))))

(deftest transform-def
  (let [ctx (mock-compile-context
              :ns-registry {"foo" #{}}
              :current-ns "foo")]
    (is (= [[:expr "bar"]
            [[:assign (c/globals "bar") [:expr "42"]]]
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
              :current-ns "foo")
        temp0 (c/temp-name 0)]
    (is (= [[:expr "None"] nil ctx] (c/transform ctx (do$))))
    (is (= [[:expr temp0]
            [[:assign (c/globals "bar") [:expr "42"]]
             [:assign temp0 [:expr "bar"]]]
            (mock-compile-context
              :ns-registry {"foo" #{"bar"}}
              :current-ns "foo"
              :counter 1)]
           (c/transform ctx (do$ (def$ "bar" 42)))))
    (is (= [[:expr temp0]
            [[:assign (c/globals "bar") [:expr "42"]]
             [:expr "bar"]
             [:assign (c/globals "baz") [:expr "43"]]
             [:assign temp0 [:expr "baz"]]]
            (mock-compile-context
              :ns-registry {"foo" #{"bar" "baz"}}
              :current-ns "foo"
              :counter 1)]
           (c/transform ctx
             (do$ (def$ "bar" 42)
                  (def$ "baz" 43)))))))
