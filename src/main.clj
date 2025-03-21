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
        (do (doseq [error errors]
              (println error))
            env)
        (reduce (fn [env statement]
                  (try
                    (let [[_ env] (interpreter/evaluate statement env)]
                      env)
                    (catch Exception e
                      (println (.getMessage e))
                      env)))
                env
                statements)))))


(defn repl
  []
  (let [prompt (if (System/getProperty "babashka.version")
                 "bblox> "
                 "cljlox> ")]
    (loop [env {}]
      (print prompt)
      (flush)
      (let [s (read-line)]
        (when s
          (recur (evaluate! s env)))))))


(defn -main
  [& args]
  (if (seq args)
    (reduce (fn [env arg]
              (evaluate! (slurp arg) env))
            {}
            args)
    (repl)))
