(ns mal.python.compiler-test
  (:require [clojure.test :refer [deftest is]]
            [mal.core :as core]
            [mal.python.compiler :as c]
            [mal.reader :as r]
            [mal.test.utils :refer [def$ do$ if$ kw$ fn$ let$ qq$ quote$
                                    spunq$ sym$ unq$]]))

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
  (is (= (c/emit [:call [:value "foo"]
                   [:value "a"] [:value "b"]
                   {"c" [:value "42"]}])
         ["foo(a, b, c=42)"]))
  (is (= (c/emit [:block
                   [:assign "a" [:value "b"]]
                   [:call [:value "foo"]
                     [:value "a"] [:value "b"]
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
                     [:call [:value "foo"] [:value "a"] [:value "b"]]]
                   [[[:value "cond2"]
                       [:block
                         [:assign "f" [:value "g"]]
                         [:assign "h" [:value "i"]]]]
                    [[:value "cond3"]
                       [:block
                         [:call [:value "baz"] [:value "j"] [:value "k"]]
                         [:assign "l" [:value "m"]]]]]
                   [:block
                     [:call [:value "bar"]
                       [:value "x"] [:value "y"]
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
                     [:if [:call [:value "foo"]
                            [:value "a"] [:value "b"]
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
                     [:call [:value "foo"]
                       [:value "a"] [:value "b"]
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

(deftest munge-name
  (is (= "foo_bar" (c/munge-name "foo-bar")))
  (is (= "_PLUS_bar" (c/munge-name "+bar")))
  (is (= "baz_BANG_" (c/munge-name "baz!")))
  (is (= "nil_QMARK_" (c/munge-name "nil?")))
  (is (= "_STAR_ns_STAR_" (c/munge-name "*ns*")))
  (is (= "_DOLLAR_foo" (c/munge-name "$foo")))
  (is (= "___def" (c/munge-name "def")))
  (is (= "___global" (c/munge-name "global")))
  (is (= "___globals" (c/munge-name "globals")))
  (is (= "___list" (c/munge-name "list")))
  (is (= "___map" (c/munge-name "map")))
  (is (= "___set" (c/munge-name "set")))
  (is (= "___str" (c/munge-name "str")))
  (is (re-find #"name '___map' is reserved"
        (try (c/munge-name "___map")
          (catch Exception e (core/object-exception-unwrap e)))))
  (is (re-find #"name '___str' is reserved"
        (try (c/munge-name "___str")
          (catch Exception e (core/object-exception-unwrap e))))))

(deftest munge-symbol
  (is (= "foo" (c/munge-symbol (sym$ "foo"))))
  (is (= "foo.bar.baz" (c/munge-symbol (sym$ "foo.bar/baz"))))
  (is (= "foo_bar._STAR_baz_STAR_._PLUS_qux_fred_BANG_"
         (c/munge-symbol (sym$ "foo-bar.*baz*/+qux-fred!"))))
  (is (re-find #"must be a symbol"
        (try (c/munge-symbol "foo")
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
  (let [ctx (mock-compile-context
              :ns-registry {"foo" #{"list"}}
              :current-ns "foo")]
    (is (= [[:call "___list"] nil ctx] (c/transform ctx ()))))
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
    (is (= [[:call (c/temp-name 0)]
            [[:def (c/temp-name 0) []
               [:block [:return [:value "None"]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx (list (sym$ "let*") []))))
    (is (= [[:call (c/temp-name 0)]
            [[:def (c/temp-name 0) []
               [:block
                 [:assign "a" [:value "42"]]
                 [:return [:value "a"]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx
             (let$ [(sym$ "a") 42]
               (sym$ "a")))))
    (is (= [[:call (c/temp-name 0)]
            [[:def (c/temp-name 0) []
               [:block
                 [:assign (c/globals "b") [:value "42"]]
                 [:assign "a" [:value "b"]]
                 [:assign (c/globals "d") [:value "43"]]
                 [:assign "c" [:value "d"]]
                 [:value "a"]
                 [:return [:value "c"]]]]]
            (mock-compile-context
              :ns-registry {"foo" #{"b" "d"}}
              :current-ns "foo"
              :counter 1)]
           (c/transform ctx
             (let$ [(sym$ "a") (def$ "b" 42)
                    (sym$ "c") (def$ "d" 43)]
               (do$
                 (sym$ "a")
                 (sym$ "c"))))))
    (is (= [[:call (c/temp-name 0)]
            [[:def (c/temp-name 0) []
               [:block
                 [:assign "a" [:value "42"]]
                 [:assign "b" [:value "a"]]
                 [:return [:value "b"]]]]]
            (assoc ctx :counter 1)]
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

(deftest transform-if
  (let [ctx (mock-compile-context
              :ns-registry {"foo" #{"a" "b" "c" "d" "e" "f"}}
              :current-ns "foo")]
    (is (= [[:value (c/temp-name 0)]
            [[:if [:value "True"]
               [:block
                 [:assign (c/temp-name 0) [:value "42"]]]
               nil
               [:block
                 [:assign (c/temp-name 0) [:value "None"]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx (if$ true 42))))
    (is (= [[:value (c/temp-name 0)]
            [[:if [:value "False"]
               [:block
                 [:assign (c/temp-name 0) [:value "43"]]]
               nil
               [:block
                 [:assign (c/temp-name 0) [:value "44"]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx (if$ false 43 44))))
    (is (= [[:value (c/temp-name 0)]
            [[:value "a"]
             [:if [:value "b"]
               [:block
                 [:value "c"]
                 [:assign (c/temp-name 0) [:value "d"]]]
               nil
               [:block
                 [:value "e"]
                 [:assign (c/temp-name 0) [:value "f"]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx
             (if$ (do$ (sym$ "a") (sym$ "b"))
                  (do$ (sym$ "c") (sym$ "d"))
                  (do$ (sym$ "e") (sym$ "f"))))))
    (is (re-find #"if expects at least 2 arguments"
          (try (c/transform ctx
                 (list (sym$ "if") true))
            (catch Error e (.getMessage e)))))
    (is (re-find #"if expects at most 3 arguments"
          (try (c/transform ctx
                 (list (sym$ "if") true 42 43 44))
            (catch Error e (.getMessage e)))))))

(deftest transform-fn
  (let [ctx (mock-compile-context
              :ns-registry {"foo" #{"bar" "baz"}}
              :current-ns "foo"
              :locals #{"qux"})]
    (is (= [[:value (c/temp-name 0)]
            [[:def (c/temp-name 0) []
               [:block
                 [:return [:value "None"]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx (fn$ []))))
    (is (= [[:value (c/temp-name 0)]
            [[:def (c/temp-name 0) []
               [:block
                 [:return [:value "42"]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx (fn$ [] 42))))
    (is (= [[:value (c/temp-name 0)]
            [[:def (c/temp-name 0) []
               [:block
                 [:value "bar"]
                 [:value "baz"]
                 [:return [:value "qux"]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx
             (fn$ []
               (do$ (sym$ "bar")
                    (sym$ "baz")
                    (sym$ "qux"))))))
    (is (= [[:value (c/temp-name 0)]
            [[:def (c/temp-name 0) ["a"]
               [:block
                 [:return [:value "42"]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx (fn$ [(sym$ "a")] 42))))
    (is (= [[:value (c/temp-name 0)]
            [[:def (c/temp-name 0) ["b"]
               [:block
                 [:return [:value "b"]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx (fn$ [(sym$ "b")] (sym$ "b")))))
    (is (= [[:value (c/temp-name 0)]
            [[:def (c/temp-name 0) ["c" "d" "*e"]
               [:block
                 [:value "c"]
                 [:value "d"]
                 [:return [:value "e"]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx
             (fn$ [(sym$ "c") (sym$ "d") (sym$ "&") (sym$ "e")]
               (do$ (sym$ "c")
                    (sym$ "d")
                    (sym$ "e"))))))
    (is (= [[:value (c/temp-name 0)]
            [[:def (c/temp-name 0) ["*args"]
               [:block
                 [:return [:value "args"]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx
             (fn$ [(sym$ "&") (sym$ "args")]
               (sym$ "args")))))
    (is (re-find #"expects at most 2 arguments"
          (try (c/transform ctx
                  (list (sym$ "fn*") [] 42 43))
            (catch Error e (.getMessage e)))))
    (is (re-find #"function parameter must be a simple symbol"
          (try (c/transform ctx
                  (list (sym$ "fn*") ["a"]
                        (sym$ "a")))
            (catch Error e (.getMessage e)))))
    (is (re-find #"function parameter must be a simple symbol"
          (try (c/transform ctx
                  (list (sym$ "fn*") [(sym$ "foo/bar")]
                        (sym$ "foo/bar")))
            (catch Error e (.getMessage e)))))
    (is (re-find #"expected only one parameter after &"
          (try (c/transform ctx
                  (list (sym$ "fn*") [(sym$ "a")
                                      (sym$ "&") (sym$ "b") (sym$ "c")]
                        (sym$ "a")))
            (catch Error e (.getMessage e)))))
    (is (re-find #"variadic parameter must be a simple symbol"
          (try (c/transform ctx
                  (list (sym$ "fn*") [(sym$ "a") (sym$ "&") "b"]
                        (sym$ "a")))
            (catch Error e (.getMessage e)))))
    (is (re-find #"variadic parameter must be a simple symbol"
          (try (c/transform ctx
                  (list (sym$ "fn*") [(sym$ "a") (sym$ "&") (sym$ "foo/bar")]
                        (sym$ "a")))
            (catch Error e (.getMessage e)))))
    (is (re-find #"'c' not found"
          (try (c/transform ctx
                  (list (sym$ "fn*") [(sym$ "a") (sym$ "&") (sym$ "b")]
                        (sym$ "c")))
            (catch Exception e (core/object-exception-unwrap e)))))))

(deftest transform-quote
  (let [ctx (mock-compile-context
              :ns-registry {"foo" #{"hash-map"
                                    "hash-set"
                                    "keyword"
                                    "list"
                                    "symbol"
                                    "vector"}}
              :current-ns "foo")]
    (is (= [[:value "None"] nil ctx] (c/transform ctx (quote$ nil))))
    (is (= [[:value "True"] nil ctx] (c/transform ctx (quote$ true))))
    (is (= [[:value "False"] nil ctx] (c/transform ctx (quote$ false))))
    (is (= [[:value "42"] nil ctx] (c/transform ctx (quote$ 42))))
    (is (= [[:value "3.14"] nil ctx] (c/transform ctx (quote$ 3.14))))
    (is (= [[:value "\"Hello, \\\"World\\\"!\""] nil ctx]
           (c/transform ctx (quote$ "Hello, \"World\"!"))))
    (is (= [[:call [:value "keyword"] [:value "\"foo\""]] nil ctx]
           (c/transform ctx (quote$ (kw$ "foo")))))
    (is (= [[:call [:value "symbol"] [:value "\"foo/bar\""]] nil ctx]
           (c/transform ctx (quote$ (sym$ "foo/bar")))))
    (is (= [[:call [:value "symbol"] [:value "\"baz\""]] nil ctx]
           (c/transform ctx (quote$ (sym$ "baz")))))
    (is (= [[:call [:value "___list"]] nil ctx]
           (c/transform ctx (quote$ (list)))))
    (is (= [[:call [:value "___list"]
              [:value "None"]
              [:value "43"]
              [:value "\"hello\""]
              [:call [:value "symbol"] [:value "\"qux\""]]]
            nil ctx]
           (c/transform ctx (quote$ (list nil 43 "hello" (sym$ "qux"))))))
    (is (= [[:call [:value "vector"]] nil ctx]
           (c/transform ctx (quote$ []))))
    (is (= [[:call [:value "vector"]
              [:value "None"]
              [:value "43"]
              [:value "\"hello\""]
              [:call [:value "symbol"] [:value "\"qux\""]]]
            nil ctx]
           (c/transform ctx (quote$ [nil 43 "hello" (sym$ "qux")]))))
    (is (= [[:call [:value "hash_map"]] nil ctx]
           (c/transform ctx (quote$ {}))))
    (is (= [[:call [:value "hash_map"]
              [:value "\"hello\""]
              [:call [:value "symbol"] [:value "\"qux\""]]
              [:value "None"]
              [:value "43"]]
            nil ctx]
           (c/transform ctx (quote$ {nil 43 "hello" (sym$ "qux")}))))
    (is (= [[:call [:value "hash_set"]] nil ctx]
           (c/transform ctx (quote$ #{}))))
    (is (= [[:call [:value "hash_set"]
              [:value "\"hello\""]
              [:value "43"]
              [:value "None"]
              [:call [:value "symbol"] [:value "\"qux\""]]]
            nil ctx]
           (c/transform ctx (quote$ #{nil 43 "hello" (sym$ "qux")}))))
    (is (re-find #"quote expects one argument"
          (try (c/transform ctx (list (sym$ "quote")))
            (catch Error e (.getMessage e)))))
    (is (re-find #"quote expects one argument"
          (try (c/transform ctx (list (sym$ "quote") 1 2))
            (catch Error e (.getMessage e)))))))

(deftest transform-call
  (let [ctx (mock-compile-context
              :ns-registry {"foo" #{"bar" "baz"}}
              :current-ns "foo"
              :locals #{"qux"})]
    (is (= [[:call [:value "foo.bar"]] [] ctx]
           (c/transform ctx (list (sym$ "foo/bar")))))
    (is (= [[:call [:value "baz"]
              [:value "1"] [:value "2"] [:value "3"]]
            []
            ctx]
           (c/transform ctx (list (sym$ "baz") 1 2 3))))
    (is (= [[:call [:value "qux"]
              [:value "\"hello\""] [:value "a"] [:value "bar"]]
            [[:assign (c/globals "a") [:value "43"]]]
            (update-in ctx [:ns-registry (sym$ "foo") :bindings]
              conj (sym$ "a"))]
           (c/transform ctx
             (list (sym$ "qux") "hello" (def$ "a" 43) (sym$ "bar")))))
    (is (= [[:call (c/temp-name 0) [:value "b"]]
            [[:assign (c/globals "a") [:value "1"]]
             [:assign (c/temp-name 0)
               [:call [:value "bar"] [:value "a"]]]
             [:assign (c/globals "b") [:value "2"]]]
            (-> ctx
                (update-in [:ns-registry (sym$ "foo") :bindings]
                  conj (sym$ "a") (sym$ "b"))
                (assoc :counter 1))]
           (c/transform ctx
             (list (list (sym$ "bar") (def$ "a" 1)) (def$ "b" 2)))))))

(deftest transform-quasiquote
  (let [ctx (mock-compile-context
              :ns-registry {"foo" #{"bar"
                                    "baz"
                                    "concat"
                                    "list"
                                    "symbol"
                                    "vec"
                                    "vector"}}
              :current-ns "foo"
              :locals #{"qux"})]
    (is (= [[:value "None"] nil ctx] (c/transform ctx (qq$ nil))))
    (is (= [[:call [:value "symbol"] [:value "\"a\""]] nil ctx]
           (c/transform ctx (qq$ (sym$ "a")))))
    (is (= [[:value "bar"] nil ctx]
           (c/transform ctx (qq$ (unq$ (sym$ "bar"))))))
    (is (= [[:call [:value "concat"] [:value "baz"]] [] ctx]
           (c/transform ctx (qq$ (list (spunq$ (sym$ "baz")))))))
    (is (= [[:call [:value "concat"]
              [:call [:value "___list"]
                [:value "42"]
                [:call [:value "symbol"] [:value "\"x\""]]]
              [:value "qux"]]
            [] ctx]
           (c/transform ctx
             (qq$ (list 42 (sym$ "x") (spunq$ (sym$ "qux")))))))
    (is (= [[:call [:value "concat"]
              [:value "bar"]
              [:call [:value "___list"]
                [:call [:value "symbol"] [:value "\"fred\""]]
                [:value "42"]]]
            [] ctx]
           (c/transform ctx
             (qq$ (list (spunq$ (sym$ "bar")) (sym$ "fred") 42)))))
    (is (= [[:call [:value "concat"]
              [:call [:value "___list"]
                [:value "1"]
                [:call [:value "concat"]
                  [:value "qux"]
                  [:call [:value "___list"] [:value "2"]]]]]
            [] ctx]
           (c/transform ctx
             (qq$ (list 1 (list (spunq$ (sym$ "qux")) 2))))
           ))
    (is (= [[:call [:value "concat"]
              [:call [:value "___list"] [:value "bar"] [:value "42"]]
              [:value "baz"]]
            [] ctx]
           (c/transform ctx
             (qq$ (list (unq$ (sym$ "bar")) 42 (spunq$ (sym$ "baz")))))
           ))
    (is (= [[:call [:value "concat"]
              [:call [:value "vector"]
                [:value "1"]
                [:value "2"]
                [:call [:value "concat"]
                  [:call [:value "___list"]
                    [:value "3"]
                    [:value "4"]]]]]
            [] ctx]
           (c/transform ctx
             (r/read-string "`(~@(vector 1 2 `(~@(list 3 4))))"))))
    (is (= [[:call [:value "concat"]
              [:call [:value "___list"]
                [:value "42"]
                [:call [:value "concat"]
                  [:call [:value "___list"] [:value "43"]]
                  [:value "bar"]]]
              [:value "baz"]]
            [[:value "1"]
             [:value "2"]]
            ctx]
           (c/transform ctx
             (r/read-string "`(42 (43 ~@(do 1 bar)) ~@(do 2 baz))"))))
    (is (= [[:call [:value "concat"]
              [:call [:value "___list"]
                [:call [:value "concat"]
                  [:call [:value "___list"] [:value "bar"]]
                  [:value "x"]]]
              [:value "x"]]
            [[:value "1"]
             [:assign (c/globals "x") [:value "2"]]
             [:value "3"]]
            (update-in ctx [:ns-registry (sym$ "foo") :bindings]
              conj (sym$ "x"))]
           (c/transform ctx
             (r/read-string "`((~bar ~@(do 1 (def! x 2))) ~@(do 3 x))"))))
    (is (= [[:call [:value "concat"]
              [:call [:value "___list"]
                [:value "1"]
                [:value "qux"]
                [:value "2"]]
              [:value "bar"]
              [:value "baz"]]
            [] ctx]
           (c/transform ctx
             (qq$ (list 1 (unq$ (sym$ "qux")) 2
                        (spunq$ (sym$ "bar"))
                        (spunq$ (sym$ "baz")))))))
    (is (= [[:call [:value "vec"]
              [:call [:value "concat"]
                [:call [:value "___list"]
                  [:value "1"]
                  [:value "qux"]
                  [:value "2"]]
                [:value "bar"]
                [:value "baz"]]]
            [] ctx]
           (c/transform ctx
             (qq$ (vector 1 (unq$ (sym$ "qux")) 2
                          (spunq$ (sym$ "bar"))
                          (spunq$ (sym$ "baz")))))))
    (is (re-find #"unquote expects exactly one argument"
          (try (c/transform ctx
                 (qq$ (list (sym$ "unquote") (sym$ "bar") (sym$ "baz"))))
            (catch Error e (.getMessage e)))))
    (is (re-find #"splice-unquote expects exactly one argument"
          (try (c/transform ctx
                 (qq$ (list (list (sym$ "splice-unquote")
                                  (sym$ "bar")
                                  (sym$ "baz")))))
            (catch Error e (.getMessage e)))))
    (is (re-find #"splice-unquote used outside of list context"
          (try (c/transform ctx
                 (qq$ (spunq$ (sym$ "bar"))))
            (catch Exception e (core/object-exception-unwrap e)))))
    (is (re-find #"quasiquote expects exactly one argument"
          (try (c/transform ctx
                 (list (sym$ "quasiquote") (sym$ "bar") (sym$ "baz")))
            (catch Error e (.getMessage e)))))))
