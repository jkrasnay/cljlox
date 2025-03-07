(ns main
  (:require
    [clojure.string :as string]
    interpreter
    lexer
    parser))


(defn evaluate!
  [s env]
  (when-not (string/blank? s)
    (let [{:keys [errors
                  statements]} (parser/parse s)]
      (if (seq errors)
        (doseq [error errors]
          (println error))
        (reduce (fn [env statement]
                  (try
                    (interpreter/execute statement env)
                    (catch Exception e
                      (println (.getMessage e))
                      env)))
                env
                statements)))))


(defn repl
  []
  (loop [env {}]
    (print "cljlox> ")
    (flush)
    (let [s (read-line)]
      (when s
        (recur (evaluate! s env))))))


(defn -main
  [& args]
  (if (seq args)
    (reduce (fn [env arg]
              (evaluate! (slurp arg) env))
            {}
            args)
    (repl)))
