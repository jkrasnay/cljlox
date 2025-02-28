(ns main
  (:require
    [clojure.string :as string]
    interpreter
    lexer
    parser))


(defn evaluate
  "Returns a map with the following keys:

  :errors | Seq of error messages
  :result | Result of the evaluation
  "
  [s]
  (let [{:keys [errors
                tokens]} (lexer/tokenize s)]
    (if (seq errors)
      {:errors errors}
      (let [{:keys [errors
                    ast]} (parser/parse tokens)]
        (if (seq errors)
          {:errors errors}
          (try
            {:result (interpreter/evaluate ast)}
            (catch Exception e
              {:errors [(.getMessage e)]})))))))


(defn -main
  [& args]
  (loop []
    (print "cljlox> ")
    (flush)
    (let [s (read-line)]
      (when s
        (when-not (string/blank? s)
          (let [{:keys [errors
                        result]} (evaluate s)]
            (if (seq errors)
              (doseq [error errors]
                (println error))
              (println result))))
        (recur))))
  (println))
