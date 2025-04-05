(ns cljlox.main
  (:require
    [clojure.string :as string]
    [cljlox.interpreter :as interpreter]
    [cljlox.parser :as parser]))


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
            (interpreter/evaluate statement interpreter/globals)
            (catch Exception e
              (println (.getMessage e)))))))))


(defn repl
  []
  (let [prompt (if (System/getProperty "babashka.version")
                 "bblox> "
                 "cljlox> ")]
    (loop []
      (print prompt)
      (flush)
      (let [s (read-line)]
        (when s
          (evaluate! s)
          (recur))))))


(defn -main
  [& args]
  (if (seq args)
    (doseq [file args]
      (evaluate! (slurp file)))
    (repl)))
