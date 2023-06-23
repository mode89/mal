(defn -pr-char-readable [ch]
  (case ch
    \"       "\\\""
    \newline "\\n"
    \tab     "\\t"
    \\       "\\\\"
    (native-to-string ch)))

(defn pr-str* [object print-readably]
  (cond
    (nil? object)
      "nil"
    (boolean? object)
      (native-to-string object)
    (number? object)
      (native-to-string object)
    (char? object)
      (native-to-string object)
    (string? object)
      (if print-readably
        (join "" (concat ["\""] (map -pr-char-readable object) ["\""]))
        object)
    (symbol? object)
      (str (when-some [ns (namespace object)]
             (str ns "/"))
           (name object))
    (keyword? object)
      (str \:
           (when-some [ns (namespace object)]
             (str ns "/"))
           (name object))
    (list? object)
      (str \(
           (join " "
             (map (fn [x]
                    (pr-str* x print-readably))
                  object))
           \) )
    (vector? object)
      (str \[
           (join " "
             (map (fn [x]
                    (pr-str* x print-readably))
                  object))
           \] )
    (map? object)
      (str \{
           (join " "
             (map (fn [x]
                    (pr-str* x print-readably))
                  (apply concat (seq object))))
           \} )
    (fn? object)
      (str "#function[" (native-to-string object) "]")
    (macro? object)
      (str "#macro[" (native-to-string object) "]")
    (atom? object)
      (str "(atom " (deref object) ")")
    (instance? Namespace object)
      (str "#namespace[" (-> object :name name) "]")
    :else
      (str "#object["
           (native-to-string (type object))
           " "
           (native-to-string object)
           "]")))

(defn pr-str [& args]
  (join " "
    (map (fn [x]
           (pr-str* x true))
         args)))

(defn str [& args]
  (join ""
    (map (fn [x]
           (if (some? x)
             (pr-str* x false)
             ""))
         args)))

(defn prn [& args]
  (print (apply pr-str args))
  (print \newline))

(defn println [& args]
  (print
    (join " "
      (map (fn [x]
             (pr-str* x false))
           args)))
  (print \newline))
