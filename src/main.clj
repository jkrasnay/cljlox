(ns main
  (:require
    [clojure.string :as string]
    interpreter
    lexer
    parser))


(defn evaluate!
  [s]
  (when-not (string/blank? s)
    (let [{:keys [errors
                  statements]} (parser/parse s)]
      (if (seq errors)
        (doseq [error errors]
          (println error))
        (doseq [statement statements]
          (try
            (interpreter/evaluate statement)
            (catch Exception e
              (println (.getMessage e)))))))))


(defn repl
  []
  (loop []
    (print "cljlox> ")
    (flush)
    (let [s (read-line)]
      (when s
        (evaluate! s)
        (recur)))))


(defn -main
  [& args]
  (if (seq args)
    (doseq [arg args]
      (evaluate! (slurp arg)))
    (repl)))
