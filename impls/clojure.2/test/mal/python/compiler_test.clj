(ns mal.python.compiler-test
  (:require [clojure.test :refer [deftest is]]
            [mal.core :as core]
            [mal.python.compiler :as c]
            [mal.reader :as r]
            [mal.test.utils :refer [def$ defmacro$ do$ if$ fn$ let$ qq$
                                    quote$ spunq$ thrown-with-msg*
                                    unq$]]))

(defn munge*
  ([name]
    (c/munge-symbol (core/symbol name)))
  ([ns name]
    (c/munge-symbol (core/symbol ns name))))

(defn mock-ns [ns-name bindings]
  {:bindings (into {}
               (map (fn [bname]
                      [(core/symbol bname)
                       {:python-name (munge* ns-name bname)}])
                    bindings))})

(defn mock-compile-context [& {:keys [ns-registry
                                      current-ns
                                      locals
                                      counter]}]
  (assert (or (nil? locals) (set? locals)))
  (let [registry (into {}
                   (map (fn [[ns-name bindings]]
                          (assert (set? bindings))
                          [(core/symbol ns-name)
                           (mock-ns ns-name bindings)])
                        ns-registry))]
    (c/map->CompileContext
      {:ns-registry registry
       :current-ns (when (some? current-ns)
                     (assert (contains? ns-registry current-ns))
                     (core/symbol current-ns))
       :locals (into {}
                 (map (fn [name]
                        [(core/symbol name)
                         {:python-name (munge* name)}])
                      locals))
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
  (is (= (c/emit [:assign [:value "a"] [:value "b"]]) ["a = b"]))
  (is (= (c/emit [:call [:value "foo"]
                   [:value "a"] [:value "b"]
                   {"c" [:value "42"]}])
         ["foo(a, b, c=42)"]))
  (is (= (c/emit [:block
                   [:assign [:value "a"] [:value "b"]]
                   [:call [:value "foo"]
                     [:value "a"] [:value "b"]
                     {"c" [:value "42"]}]])
         ["a = b"
          "foo(a, b, c=42)"]))
  (is (= (c/emit [:block
                   [:block
                     [:assign [:value "a"] [:value "b"]]
                     [:assign [:value "c"] [:value "d"]]]
                   [:block
                     [:assign [:value "e"] [:value "f"]]
                     [:assign [:value "g"] [:value "h"]]]])
         ["a = b"
          "c = d"
          "e = f"
          "g = h"]))
  (is (= (c/emit [:if [:value "a"]
                   [:block
                     [:assign [:value "b"] [:value "c"]]] nil nil])
         ["if a:" "  b = c"]))
  (is (= (c/emit [:if [:value "a"]
                   [:block
                     [:assign [:value "b"] [:value "c"]]]
                   nil
                   [:block
                     [:assign [:value "d"] [:value "e"]]]])
         ["if a:"
          "  b = c"
          "else:"
          "  d = e"]))
  (is (= (c/emit [:if [:value "cond1"]
                   [:block
                     [:assign [:value "a"] [:value "b"]]
                     [:call [:value "foo"] [:value "a"] [:value "b"]]]
                   [[[:value "cond2"]
                       [:block
                         [:assign [:value "f"] [:value "g"]]
                         [:assign [:value "h"] [:value "i"]]]]
                    [[:value "cond3"]
                       [:block
                         [:call [:value "baz"] [:value "j"] [:value "k"]]
                         [:assign [:value "l"] [:value "m"]]]]]
                   [:block
                     [:call [:value "bar"]
                       [:value "x"] [:value "y"]
                       {"z" [:value "42"]}]
                     [:assign [:value "p"] [:value "q"]]]])
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
                     [:assign [:value "b"] [:value "c"]]]])
         ["while a:" "  b = c"]))
  (is (= (c/emit [:while [:value "a"]
                   [:block
                     [:assign [:value "b"] [:value "c"]]
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
                     [:assign [:value "c"] [:value "d"]]
                     [:return [:value "e"]]]])
         ["def foo(a, b):"
          "  c = d"
          "  return e"]))
  (is (= (c/emit [:try
                   [:block
                     [:assign [:value "a"] [:value "b"]]]
                   nil
                   [:block
                     [:assign [:value "c"] [:value "d"]]]])
         ["try:"
          "  a = b"
          "finally:"
          "  c = d"]))
  (is (= (c/emit [:try
                   [:block
                     [:assign [:value "a"] [:value "b"]]
                     [:call [:value "foo"]
                       [:value "a"] [:value "b"]
                       {"c" [:value "42"]}]]
                   [["Exception" nil
                      [:block
                        [:assign [:value "d"] [:value "e"]]
                        [:return [:value "f"]]]]
                    ["ValueError" "e"
                      [:block
                        [:assign [:value "g"] [:value "h"]]
                        [:return [:value "i"]]]]
                    ["TypeError" "e"
                      [:block
                        [:assign [:value "j"] [:value "k"]]]]]
                   [:block
                     [:assign [:value "l"] [:value "m"]]
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
  (is (thrown-with-msg* #"name '___map' is reserved"
        (c/munge-name "___map")))
  (is (thrown-with-msg* #"name '___str' is reserved"
        (c/munge-name "___str"))))

(deftest munge-symbol
  (is (= "foo" (c/munge-symbol 'foo)))
  (is (= "foo_DOT_bar_SLASH_baz" (c/munge-symbol 'foo.bar/baz)))
  (is (= "foo_bar_DOT__STAR_baz_STAR__SLASH__PLUS_qux_fred_BANG_"
         (c/munge-symbol 'foo-bar.*baz*/+qux-fred!)))
  (is (thrown-with-msg* #"must be a symbol" (c/munge-symbol "foo"))))

(deftest resolve-symbol-name
  (is (= "foo" (c/resolve-symbol-name
                 (mock-compile-context :locals #{"foo"})
                 'foo)))
  (is (= (munge* "foo" "bar") (c/resolve-symbol-name
                                (mock-compile-context
                                  :ns-registry {"foo" #{"bar"}}
                                  :current-ns "foo")
                                'bar)))
  (is (= (munge* "baz" "qux") (c/resolve-symbol-name
                                (mock-compile-context
                                  :ns-registry {"foo" #{"bar"}
                                                "baz" #{"qux"}}
                                  :current-ns "foo")
                                'baz/qux)))
  (is (re-find #"must be a symbol"
        (try (c/resolve-symbol-name
               (mock-compile-context :locals #{"foo"})
               "foo")
          (catch Error e (.getMessage e)))))
  (is (thrown-with-msg* #"locals must be a map"
        (c/resolve-symbol-name
          (c/map->CompileContext
            {:ns-registry nil
             :current-ns nil
             :locals ["foo"]
             :counter 0})
          'foo)))
  (is (re-find #"'bar' not found"
        (try (c/resolve-symbol-name
               (mock-compile-context
                 :ns-registry {"foo" #{"baz"}}
                 :current-ns "foo")
               'bar)
          (catch Exception e (core/object-exception-unwrap e)))))
  (is (re-find #"'baz/bar' not found"
        (try (c/resolve-symbol-name
               (mock-compile-context
                 :ns-registry {"foo" #{"bar"}
                               "baz" #{"qux"}}
                 :current-ns "foo")
               'baz/bar)
          (catch Exception e (core/object-exception-unwrap e)))))
  (is (re-find #"namespace 'baz' not found"
        (try (c/resolve-symbol-name
               (mock-compile-context
                 :ns-registry {"foo" #{"bar"}}
                 :current-ns "foo")
               'baz/bar)
          (catch Exception e (core/object-exception-unwrap e)))))
  (is (re-find #"'fred' not found"
        (try (c/resolve-symbol-name
               (mock-compile-context
                 :ns-registry {"foo" #{"bar"}}
                 :current-ns nil
                 :locals #{"baz" "qux"})
               'fred)
          (catch Exception e (core/object-exception-unwrap e)))))
  (is (thrown-with-msg* #"namespace bindings must be a map"
        (c/resolve-symbol-name
          (c/map->CompileContext
            {:ns-registry {'foo ['bar]}
             :current-ns 'foo
             :locals {}
             :counter 0})
          'bar)))
  (is (thrown-with-msg* #"namespace bindings must be a map"
        (c/resolve-symbol-name
          (c/map->CompileContext
            {:ns-registry {'foo {}
                           'bar ['baz]}
             :current-ns 'foo
             :locals {}
             :counter 0})
          'bar/baz)))
  (is (thrown-with-msg* #"current namespace not found"
        (c/resolve-symbol-name
          (c/map->CompileContext
            {:ns-registry {'foo {'bar {:python-name "bar"}}}
             :current-ns 'baz
             :locals {}
             :counter 0})
          'bar))))

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
    (is (= [[:call (munge* "foo" "list")] nil ctx] (c/transform ctx ()))))
  (let [ctx (mock-compile-context :locals #{"foo"})]
    (is (= [[:value "foo"] nil ctx] (c/transform ctx 'foo)))))

(deftest transform-def
  (let [ctx (mock-compile-context
              :ns-registry {"foo" #{}}
              :current-ns "foo")]
    (is (= [[:value (munge* "foo" "bar")]
            [[:assign (c/globals (munge* "foo" "bar")) [:value "42"]]]
            (mock-compile-context
              :ns-registry {"foo" #{"bar"}}
              :current-ns "foo")]
           (c/transform ctx (def$ "bar" 42))))
    (is (= [[:value (munge* "foo" "qux")]
            [[:value "1"]
             [:assign (c/globals (munge* "foo" "qux")) [:value "2"]]]
            (mock-compile-context
              :ns-registry {"foo" #{"qux"}}
              :current-ns "foo")]
           (c/transform ctx
             (def$ "qux"
               (do$ 1 2))))))
  (is (thrown-with-msg* #"def! expects a simple symbol as the first argument"
        (c/transform
          (mock-compile-context
            :ns-registry {"foo" #{}}
            :current-ns "foo")
          (list 'def! "bar" 42))))
  (is (thrown-with-msg* #"def! expects a simple symbol as the first argument"
        (c/transform
          (mock-compile-context
            :ns-registry {"foo" #{}}
            :current-ns "foo")
          (list 'def! 'foo/bar 42))))
  (is (re-find #"def! expects 2 arguments"
        (try (c/transform
               (mock-compile-context
                 :ns-registry {"foo" #{}}
                 :current-ns "foo")
               (list 'def! 'bar))
          (catch Error e (.getMessage e)))))
  (is (re-find #"def! expects 2 arguments"
        (try (c/transform
               (mock-compile-context
                 :ns-registry {"foo" #{}}
                 :current-ns "foo")
               (list 'def! 'bar 42 43))
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
    (is (= [[:value (munge* "foo" "bar")]
            [[:assign (c/globals (munge* "foo" "bar")) [:value "42"]]]
            (mock-compile-context
              :ns-registry {"foo" #{"bar"}}
              :current-ns "foo")]
           (c/transform ctx (do$ (def$ "bar" 42)))))
    (is (= [[:value (munge* "foo" "baz")]
            [[:assign (c/globals (munge* "foo" "bar")) [:value "42"]]
             [:value (munge* "foo" "bar")]
             [:assign (c/globals (munge* "foo" "baz")) [:value "43"]]]
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
           (c/transform ctx (list 'let* []))))
    (is (= [[:call (c/temp-name 0)]
            [[:def (c/temp-name 0) []
               [:block
                 [:assign [:value "a"] [:value "42"]]
                 [:return [:value "a"]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx
             (let$ ['a 42]
               'a))))
    (is (= [[:call (c/temp-name 0)]
            [[:def (c/temp-name 0) []
               [:block
                 [:assign (c/globals (munge* "foo" "b")) [:value "42"]]
                 [:assign [:value "a"] [:value (munge* "foo" "b")]]
                 [:assign (c/globals (munge* "foo" "d")) [:value "43"]]
                 [:assign [:value "c"] [:value (munge* "foo" "d")]]
                 [:value "a"]
                 [:return [:value "c"]]]]]
            (mock-compile-context
              :ns-registry {"foo" #{"b" "d"}}
              :current-ns "foo"
              :counter 1)]
           (c/transform ctx
             (let$ ['a (def$ "b" 42)
                    'c (def$ "d" 43)]
               (do$
                 'a
                 'c)))))
    (is (= [[:call (c/temp-name 0)]
            [[:def (c/temp-name 0) []
               [:block
                 [:assign [:value "a"] [:value "42"]]
                 [:assign [:value "b"] [:value "a"]]
                 [:return [:value "b"]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx
             (let$ ['a 42
                    'b 'a]
               'b))))
    (is (re-find #"expects even number of forms"
          (try (c/transform ctx
                 (let$ ['a 42
                        'b]
                   'a))
            (catch Error e (.getMessage e)))))
    (is (re-find #"no bindings provided"
          (try (c/transform ctx
                 (list 'let*))
            (catch Error e (.getMessage e)))))
    (is (re-find #"expects only one form in body"
          (try (c/transform ctx
                 (list 'let* ['a 42
                              'b 43]
                       'a
                       'b))
            (catch Error e (.getMessage e)))))
    (is (re-find #"binding name must be a simple symbol"
          (try (c/transform ctx
                 (let$ ["a" 42]
                   'a))
            (catch Error e (.getMessage e)))))
    (is (re-find #"binding name must be a simple symbol"
          (try (c/transform ctx
                 (let$ ['bar/qux 42]
                   'bar/qux))
            (catch Error e (.getMessage e)))))
    (is (re-find #"'b' not found"
          (try (c/transform ctx
                 (let$ ['a 42]
                   'b))
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
            [[:value (munge* "foo" "a")]
             [:if [:value (munge* "foo" "b")]
               [:block
                 [:value (munge* "foo" "c")]
                 [:assign (c/temp-name 0) [:value (munge* "foo" "d")]]]
               nil
               [:block
                 [:value (munge* "foo" "e")]
                 [:assign (c/temp-name 0) [:value (munge* "foo" "f")]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx
             (if$ (do$ 'a 'b)
                  (do$ 'c 'd)
                  (do$ 'e 'f)))))
    (is (re-find #"if expects at least 2 arguments"
          (try (c/transform ctx
                 (list 'if true))
            (catch Error e (.getMessage e)))))
    (is (re-find #"if expects at most 3 arguments"
          (try (c/transform ctx
                 (list 'if true 42 43 44))
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
                 [:value (munge* "foo" "bar")]
                 [:value (munge* "foo" "baz")]
                 [:return [:value "qux"]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx
             (fn$ []
               (do$ 'bar
                    'baz
                    'qux)))))
    (is (= [[:value (c/temp-name 0)]
            [[:def (c/temp-name 0) ["a"]
               [:block
                 [:return [:value "42"]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx (fn$ ['a] 42))))
    (is (= [[:value (c/temp-name 0)]
            [[:def (c/temp-name 0) ["b"]
               [:block
                 [:return [:value "b"]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx (fn$ ['b] 'b))))
    (is (= [[:value (c/temp-name 0)]
            [[:def (c/temp-name 0) ["c" "d" "*e"]
               [:block
                 [:value "c"]
                 [:value "d"]
                 [:return [:value "e"]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx
             (fn$ ['c 'd '& 'e]
               (do$ 'c
                    'd
                    'e)))))
    (is (= [[:value (c/temp-name 0)]
            [[:def (c/temp-name 0) ["*args"]
               [:block
                 [:return [:value "args"]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx
             (fn$ ['& 'args]
               'args))))
    (is (re-find #"expects at most 2 arguments"
          (try (c/transform ctx
                  (list 'fn* [] 42 43))
            (catch Error e (.getMessage e)))))
    (is (re-find #"function parameter must be a simple symbol"
          (try (c/transform ctx
                  (list 'fn* ["a"]
                        'a))
            (catch Error e (.getMessage e)))))
    (is (re-find #"function parameter must be a simple symbol"
          (try (c/transform ctx
                  (list 'fn* ['foo/bar]
                        'foo/bar))
            (catch Error e (.getMessage e)))))
    (is (re-find #"expected only one parameter after &"
          (try (c/transform ctx
                  (list 'fn* ['a '& 'b 'c]
                        'a))
            (catch Error e (.getMessage e)))))
    (is (re-find #"variadic parameter must be a simple symbol"
          (try (c/transform ctx
                  (list 'fn* ['a '& "b"]
                        'a))
            (catch Error e (.getMessage e)))))
    (is (re-find #"variadic parameter must be a simple symbol"
          (try (c/transform ctx
                  (list 'fn* ['a '& 'foo/bar]
                        'a))
            (catch Error e (.getMessage e)))))
    (is (re-find #"'c' not found"
          (try (c/transform ctx
                  (list 'fn* ['a '& 'b]
                        'c))
            (catch Exception e (core/object-exception-unwrap e)))))))

(deftest transform-quote
  (let [ctx (mock-compile-context
              :ns-registry {"mal.core" #{"hash-map"
                                         "hash-set"
                                         "keyword"
                                         "list"
                                         "symbol"
                                         "vector"}
                            "foo" #{}}
              :current-ns "foo")
        keyword* (munge* "mal.core/keyword")
        list* (munge* "mal.core/list")
        symbol* (munge* "mal.core/symbol")
        vector* (munge* "mal.core/vector")
        hash-map* (munge* "mal.core/hash-map")
        hash-set* (munge* "mal.core/hash-set")]
    (is (= [[:value "None"] nil ctx] (c/transform ctx (quote$ nil))))
    (is (= [[:value "True"] nil ctx] (c/transform ctx (quote$ true))))
    (is (= [[:value "False"] nil ctx] (c/transform ctx (quote$ false))))
    (is (= [[:value "42"] nil ctx] (c/transform ctx (quote$ 42))))
    (is (= [[:value "3.14"] nil ctx] (c/transform ctx (quote$ 3.14))))
    (is (= [[:value "\"Hello, \\\"World\\\"!\""] nil ctx]
           (c/transform ctx (quote$ "Hello, \"World\"!"))))
    (is (= [[:call [:value keyword*] [:value "\"foo\""]] nil ctx]
           (c/transform ctx (quote$ :foo))))
    (is (= [[:call [:value symbol*] [:value "\"foo/bar\""]] nil ctx]
           (c/transform ctx (quote$ 'foo/bar))))
    (is (= [[:call [:value symbol*] [:value "\"baz\""]] nil ctx]
           (c/transform ctx (quote$ 'baz))))
    (is (= [[:call [:value list*]] nil ctx]
           (c/transform ctx (quote$ (list)))))
    (is (= [[:call [:value list*]
              [:value "None"]
              [:value "43"]
              [:value "\"hello\""]
              [:call [:value symbol*] [:value "\"qux\""]]]
            nil ctx]
           (c/transform ctx (quote$ (list nil 43 "hello" 'qux)))))
    (is (= [[:call [:value vector*]] nil ctx]
           (c/transform ctx (quote$ []))))
    (is (= [[:call [:value vector*]
              [:value "None"]
              [:value "43"]
              [:value "\"hello\""]
              [:call [:value symbol*] [:value "\"qux\""]]]
            nil ctx]
           (c/transform ctx (quote$ [nil 43 "hello" 'qux]))))
    (is (= [[:call [:value hash-map*]] nil ctx]
           (c/transform ctx (quote$ {}))))
    (is (= [[:call [:value hash-map*]
              [:value "\"hello\""]
              [:call [:value symbol*] [:value "\"qux\""]]
              [:value "None"]
              [:value "43"]]
            nil ctx]
           (c/transform ctx (quote$ {nil 43 "hello" 'qux}))))
    (is (= [[:call [:value hash-set*]] nil ctx]
           (c/transform ctx (quote$ #{}))))
    (is (= [[:call [:value hash-set*]
              [:value "\"hello\""]
              [:value "43"]
              [:value "None"]
              [:call [:value symbol*] [:value "\"qux\""]]]
            nil ctx]
           (c/transform ctx (quote$ #{nil 43 "hello" 'qux}))))
    (is (re-find #"quote expects one argument"
          (try (c/transform ctx (list 'quote))
            (catch Error e (.getMessage e)))))
    (is (re-find #"quote expects one argument"
          (try (c/transform ctx (list 'quote 1 2))
            (catch Error e (.getMessage e)))))))

(deftest transform-call
  (let [ctx (mock-compile-context
              :ns-registry {"foo" #{"bar" "baz"}}
              :current-ns "foo"
              :locals #{"qux"})]
    (is (= [[:call [:value (munge* "foo/bar")]] [] ctx]
           (c/transform ctx (list 'foo/bar))))
    (is (= [[:call [:value (munge* "foo/baz")]
              [:value "1"] [:value "2"] [:value "3"]]
            []
            ctx]
           (c/transform ctx (list 'baz 1 2 3))))
    (is (= [[:call [:value "qux"]
              [:value "\"hello\""]
              [:value (munge* "foo/a")]
              [:value (munge* "foo/bar")]]
            [[:assign (c/globals (munge* "foo/a")) [:value "43"]]]
            (mock-compile-context
              :ns-registry {"foo" #{"bar" "baz" "a"}}
              :current-ns "foo"
              :locals #{"qux"})]
           (c/transform ctx
             (list 'qux "hello" (def$ "a" 43) 'bar))))
    (is (= [[:call (c/temp-name 0) [:value (munge* "foo/b")]]
            [[:assign (c/globals (munge* "foo/a")) [:value "1"]]
             [:assign (c/temp-name 0)
               [:call [:value (munge* "foo/bar")]
                 [:value (munge* "foo/a")]]]
             [:assign (c/globals (munge* "foo/b")) [:value "2"]]]
            (mock-compile-context
              :ns-registry {"foo" #{"bar" "baz" "a" "b"}}
              :current-ns "foo"
              :locals #{"qux"}
              :counter 1)]
           (c/transform ctx
             (list (list 'bar (def$ "a" 1)) (def$ "b" 2)))))))

(deftest transform-quasiquote
  (let [ctx (mock-compile-context
              :ns-registry {"mal.core" #{"concat"
                                         "list"
                                         "symbol"
                                         "vec"
                                         "vector"}
                            "foo" #{"bar" "baz"}}
              :current-ns "foo"
              :locals #{"qux"})
        concat* (munge* "mal.core/concat")
        list* (munge* "mal.core/list")
        symbol* (munge* "mal.core/symbol")
        vec* (munge* "mal.core/vec")
        vector* (munge* "mal.core/vector")]
    (is (= [[:value "None"] nil ctx] (c/transform ctx (qq$ nil))))
    (is (= [[:call [:value symbol*] [:value "\"a\""]] nil ctx]
           (c/transform ctx (qq$ 'a))))
    (is (= [[:value (munge* "foo/bar")] nil ctx]
           (c/transform ctx (qq$ (unq$ 'bar)))))
    (is (= [[:call [:value concat*] [:value (munge* "foo/baz")]] [] ctx]
           (c/transform ctx (qq$ (list (spunq$ 'baz))))))
    (is (= [[:call [:value concat*]
              [:call [:value list*]
                [:value "42"]
                [:call [:value symbol*] [:value "\"x\""]]]
              [:value "qux"]]
            [] ctx]
           (c/transform ctx
             (qq$ (list 42 'x (spunq$ 'qux))))))
    (is (= [[:call [:value concat*]
              [:value (munge* "foo/bar")]
              [:call [:value list*]
                [:call [:value symbol*] [:value "\"fred\""]]
                [:value "42"]]]
            [] ctx]
           (c/transform ctx
             (qq$ (list (spunq$ 'bar) 'fred 42)))))
    (is (= [[:call [:value concat*]
              [:call [:value list*]
                [:value "1"]
                [:call [:value concat*]
                  [:value "qux"]
                  [:call [:value list*] [:value "2"]]]]]
            [] ctx]
           (c/transform ctx
             (qq$ (list 1 (list (spunq$ 'qux) 2))))))
    (is (= [[:call [:value concat*]
              [:call [:value list*]
                [:value (munge* "foo/bar")]
                [:value "42"]]
              [:value (munge* "foo/baz")]]
            [] ctx]
           (c/transform ctx
             (qq$ (list (unq$ 'bar) 42 (spunq$ 'baz))))))
    (is (= [[:call [:value concat*]
              [:call [:value vector*]
                [:value "1"]
                [:value "2"]
                [:call [:value concat*]
                  [:call [:value list*]
                    [:value "3"]
                    [:value "4"]]]]]
            [] ctx]
           (c/transform ctx
             (r/read-string
               "`(~@(mal.core/vector 1 2 `(~@(mal.core/list 3 4))))"))))
    (is (= [[:call [:value concat*]
              [:call [:value list*]
                [:value "42"]
                [:call [:value concat*]
                  [:call [:value list*] [:value "43"]]
                  [:value (munge* "foo/bar")]]]
              [:value (munge* "foo/baz")]]
            [[:value "1"]
             [:value "2"]]
            ctx]
           (c/transform ctx
             (r/read-string "`(42 (43 ~@(do 1 bar)) ~@(do 2 baz))"))))
    (is (= [[:call [:value concat*]
              [:call [:value list*]
                [:call [:value concat*]
                  [:call [:value list*] [:value (munge* "foo/bar")]]
                  [:value (munge* "foo/x")]]]
              [:value (munge* "foo/x")]]
            [[:value "1"]
             [:assign (c/globals (munge* "foo/x")) [:value "2"]]
             [:value "3"]]
            (update-in ctx [:ns-registry 'foo :bindings]
              assoc 'x {:python-name (munge* "foo/x")})]
           (c/transform ctx
             (r/read-string "`((~bar ~@(do 1 (def! x 2))) ~@(do 3 x))"))))
    (is (= [[:call [:value concat*]
              [:call [:value list*]
                [:value "1"]
                [:value "qux"]
                [:value "2"]]
              [:value (munge* "foo" "bar")]
              [:value (munge* "foo" "baz")]]
            [] ctx]
           (c/transform ctx
             (qq$ (list 1 (unq$ 'qux) 2
                        (spunq$ 'bar)
                        (spunq$ 'baz))))))
    (is (= [[:call [:value vec*]
              [:call [:value concat*]
                [:call [:value list*]
                  [:value "1"]
                  [:value "qux"]
                  [:value "2"]]
                [:value (munge* "foo" "bar")]
                [:value (munge* "foo" "baz")]]]
            [] ctx]
           (c/transform ctx
             (qq$ (vector 1 (unq$ 'qux) 2
                          (spunq$ 'bar)
                          (spunq$ 'baz))))))
    (is (re-find #"unquote expects exactly one argument"
          (try (c/transform ctx
                 (qq$ (list 'unquote 'bar 'baz)))
            (catch Error e (.getMessage e)))))
    (is (re-find #"splice-unquote expects exactly one argument"
          (try (c/transform ctx
                 (qq$ (list (list 'splice-unquote
                                  'bar
                                  'baz))))
            (catch Error e (.getMessage e)))))
    (is (re-find #"splice-unquote used outside of list context"
          (try (c/transform ctx
                 (qq$ (spunq$ 'bar)))
            (catch Exception e (core/object-exception-unwrap e)))))
    (is (re-find #"quasiquote expects exactly one argument"
          (try (c/transform ctx
                 (list 'quasiquote 'bar 'baz))
            (catch Error e (.getMessage e)))))))

(deftest transform-defmacro
  (let [ctx (mock-compile-context
              :ns-registry {"foo" #{}}
              :current-ns "foo")
        temp0 (c/temp-name 0)]
    (is (= [[:value (munge* "foo/bar")]
            [[:def temp0 []
               [:block
                 [:return [:value "None"]]]]
             [:assign (c/globals (munge* "foo/bar")) [:value temp0]]
             [:call [:value "setattr"]
               [:value (munge* "foo/bar")]
               [:value "___is_mal_macro"]
               [:value "True"]]]
            (mock-compile-context
              :ns-registry {"foo" #{"bar"}}
              :current-ns "foo"
              :counter 1)]
           (c/transform ctx (defmacro$ "bar" (fn$ [])))))
    (is (re-find #"defmacro! expects exactly 2 arguments"
          (try (c/transform ctx (list 'defmacro! 'baz))
            (catch Error e (.getMessage e)))))
    (is (re-find #"defmacro! expects exactly 2 arguments"
          (try (c/transform ctx
                 (list 'defmacro! 'baz (fn$ []) 42))
            (catch Error e (.getMessage e)))))
    (is (re-find #"defmacro! expects a symbol as the first argument"
          (try (c/transform ctx (list 'defmacro! "bar" (fn$ [])))
            (catch Error e (.getMessage e)))))
    (is (re-find #"defmacro! expects fn\* as the second argument"
          (try (c/transform ctx (list 'defmacro! 'bar 42))
            (catch Error e (.getMessage e))))))
  (is (re-find #"no current namespace"
        (try (c/transform (mock-compile-context)
               (defmacro$ "bar" (fn$ [])))
          (catch Error e (.getMessage e))))))

(deftest transform-try
  (let [ctx (mock-compile-context
              :ns-registry {"foo" #{"bar"}}
              :current-ns "foo")
        temp0 (c/temp-name 0)]
    (is (= [[:value "None"] nil ctx]
           (c/transform ctx (list 'try*))))
    (is (= [[:value (munge* "foo/bar")] nil ctx]
           (c/transform ctx
             (list 'try* 'bar))))
    (is (= [[:value temp0]
            [[:try
               [:block
                 [:assign [:value temp0] [:value "None"]]]
               [["Exception" "ex"
                  [:block
                    [:assign [:value temp0] [:value "ex"]]]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx
             (list 'try*
               (list 'catch* 'ex
                 'ex)))))
    (is (= [[:value temp0]
            [[:try
               [:block [:assign [:value temp0] [:value "None"]]]
               [["Exception" "x"
                  [:block [:assign [:value temp0] [:value "None"]]]]]]]
            (assoc ctx :counter 1)]
           (c/transform ctx
             (list 'try*
               (list 'catch* 'x)))))
    (is (= [[:value temp0]
            [[:try
               [:block
                 [:assign (c/globals (munge* "foo/a")) [:value "1"]]
                 [:value (munge* "foo/a")]
                 [:assign (c/globals (munge* "foo/b")) [:value "2"]]
                 [:assign [:value temp0] [:value (munge* "foo/b")]]]
               [["Exception" "exc"
                  [:block
                    [:assign (c/globals (munge* "foo/c")) [:value "exc"]]
                    [:value (munge* "foo/c")]
                    [:assign [:value temp0] [:value (munge* "foo/bar")]]]]]]]
            (mock-compile-context
              :ns-registry {"foo" #{"bar" "a" "b" "c"}}
              :current-ns "foo"
              :counter 1)]
           (c/transform ctx
             (list 'try*
               (do$ (def$ "a" 1)
                    (def$ "b" 2))
               (list 'catch* 'exc
                 (do$ (def$ "c" 'exc)
                      'bar))))))
    (is (= [[:value temp0]
            [[:try
               [:block
                 [:assign (c/globals (munge* "foo/x")) [:value "42"]]
                 [:assign [:value temp0] [:value (munge* "foo/x")]]]
               [["Exception" "exc"
                  [:block
                    [:assign [:value temp0] [:value (munge* "foo/x")]]]]]]]
            (mock-compile-context
              :ns-registry {"foo" #{"bar" "x"}}
              :current-ns "foo"
              :counter 1)]
           (c/transform ctx
             (list 'try*
               (def$ "x" 42)
               (list 'catch* 'exc
                 'x)))))
    (is (re-find #"try\* expects catch\* as the second argument"
          (try (c/transform ctx
                 (list 'try* 1 2))
            (catch Error e (.getMessage e)))))
    (is (re-find #"try\* expects at most 2 arguments"
          (try (c/transform ctx
                  (list 'try*
                    42
                    (list 'catch* 'ex1 'ex1)
                    (list 'catch* 'ex2 'ex2)))
            (catch Exception e (core/object-exception-unwrap e)))))
    (is (thrown-with-msg* #"exception object must be a simple symbol"
          (c/transform ctx
            (list 'try* 42
              (list 'catch* "ex" 'ex)))))
    (is (thrown-with-msg* #"exception object must be a simple symbol"
          (c/transform ctx
            (list 'try* 42
              (list 'catch* 'foo/ex 'ex)))))
    (is (re-find #"catch\* expects at most 2 arguments"
          (try (c/transform ctx
                 (list 'try* 42
                   (list 'catch* 'ex
                     'ex
                     'ex)))
            (catch Error e (.getMessage e)))))
    (is (re-find #"catch\* used outside of try\*"
          (try (c/transform ctx
                 (list 'catch* 'ex 42))
            (catch Exception e (core/object-exception-unwrap e)))))
    (is (re-find #"catch\* used outside of try\*"
          (try (c/transform ctx
                 (list 'try*
                   (list 'catch* 'ex1 'ex1)
                   (list 'catch* 'ex2 'ex2)))
            (catch Exception e (core/object-exception-unwrap e)))))))

(deftest transform-import
  (let [ctx (mock-compile-context
              :ns-registry {"foo" #{}}
              :current-ns "foo")
        temp0 (c/temp-name 0)
        temp1 (c/temp-name 1)
        temp2 (c/temp-name 2)]
    (is (= [[:value "None"]
            [[:assign [:value temp0]
               [:call [:value "__import__"] [:value "\"operator\""]]]]
            (-> ctx
                (assoc :counter 1)
                (update-in [:ns-registry 'foo :bindings]
                  assoc 'is_ {:python-name (str temp0 ".is_")}))]
           (c/transform ctx
             (list 'import ['operator 'is_]))))
    (is (= [[:value "None"]
            [[:assign [:value temp0]
               [:call [:value "__import__"] [:value "\"funcy\""]]]
             [:assign [:value temp1]
               [:call [:value "__import__"]
                 [:value "\"persistent.typing\""]]]
             [:assign [:value temp2]
               [:call [:value "__import__"] [:value "\"unittest.mock\""]]]]
            (-> ctx
                (assoc :counter 3)
                (update-in [:ns-registry 'foo :bindings] merge
                  {'compose {:python-name (str temp0 ".compose")}
                   'partial {:python-name (str temp0 ".partial")}
                   're_find {:python-name (str temp0 ".re_find")}
                   'PMap {:python-name (str temp1 ".PMap")}
                   'PMapType {:python-name (str temp1 ".PMapType")}
                   'call {:python-name (str temp2 ".call")}
                   'patch {:python-name (str temp2 ".patch")}}))]
           (c/transform ctx
             (list 'import
               ['funcy
                  'compose
                  'partial
                  're_find]
               ['persistent.typing
                  'PMap
                  'PMapType]
               ['unittest.mock
                  'call
                  'patch]))))
    (is (thrown-with-msg* #"module name must be a simple symbol"
          (c/transform ctx
            (list 'import ["foo" 'bar]))))
    (is (thrown-with-msg* #"module name must be a simple symbol"
          (c/transform ctx
            (list 'import ['foo/bar 'baz]))))
    (is (thrown-with-msg* #"imported name must be a simple symbol"
          (c/transform ctx
            (list 'import ['foo "bar"]))))
    (is (thrown-with-msg* #"imported name must be a simple symbol"
          (c/transform ctx
            (list 'import ['foo 'bar/baz])))))
  (is (thrown-with-msg* #"import expects at least one argument"
        (c/transform (mock-compile-context) (list 'import))))
  (is (thrown-with-msg* #"import must be called in a namespace"
        (c/transform (mock-compile-context :current-ns nil)
          (list 'import ['foo 'bar])))))

(deftest transform-inline-python
  (let [ctx (mock-compile-context
              :ns-registry {"mal.python.impl" #{"foo"}}
              :locals #{"bar"}
              :current-ns "mal.python.impl")
        foo* (munge* "mal.python.impl" "foo")]
    (is (= [[:value "foo"] nil ctx]
           (c/transform ctx (list 'inline-python [:value "foo"]))))
    (is (= [[:value "qux"]
            [[:value "foo"]
             [:call [:value "bar"] [:value "baz"]]]
            ctx]
           (c/transform ctx
             (list 'inline-python
               [:value "foo"]
               [:call [:value "bar"] [:value "baz"]]
               [:value "qux"]))))
    (is (= [[:value foo*] nil ctx]
           (c/transform ctx (list 'inline-python 'foo))))
    (is (= [[:call [:value "bar"] {"foo" [:value foo*]}]
            [[:call [:value foo*] {"bar" [:value "bar"]}]]
            ctx]
           (c/transform ctx
             (list 'inline-python
               [:call 'foo {"bar" 'bar}]
               [:call 'bar {"foo" 'foo}]))))
    (is (thrown-with-msg* #"inline-python expects at least one argument"
          (c/transform ctx (list 'inline-python))))
    (is (thrown-with-msg* #"last argument .* must be an expression"
          (c/transform ctx
            (list 'inline-python
              [:value "foo"]
              [:if [:value "bar"]
                [:block
                  [:value "baz"]]]))))
    (is (thrown-with-msg* #"'qux' not found"
          (c/transform ctx (list 'inline-python 'qux)))))
  (is (thrown-with-msg* #"isn't allowed outside of mal.python.impl"
        (c/transform (mock-compile-context
                       :ns-registry {"foo" #{}}
                       :current-ns "foo")
          (list 'inline-python [:value "bar"])))))

(deftest switch-ns
  (is (= (mock-compile-context
           :ns-registry {"foo" #{"bar"}
                         "baz" #{"qux"}}
           :current-ns "baz")
         (c/switch-ns
           (mock-compile-context
             :ns-registry {"foo" #{"bar"}
                           "baz" #{"qux"}}
             :current-ns "foo")
           (core/symbol "baz"))))
  (is (= (mock-compile-context
           :ns-registry {"foo" #{"bar"}
                         "qux" #{}}
           :current-ns "qux")
         (c/switch-ns
           (mock-compile-context
             :ns-registry {"foo" #{"bar"}}
             :current-ns "foo")
           (core/symbol "qux"))))
  (is (thrown-with-msg* #"namespace name must be a simple symbol"
        (c/switch-ns
          (mock-compile-context
            :ns-registry {"foo" #{"bar"}}
            :current-ns "foo")
          (core/symbol "foo/bar")))))
