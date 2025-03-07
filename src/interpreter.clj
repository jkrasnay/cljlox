(ns interpreter)

;;-- Evaluate ----------------------------------------------------------


(defmulti evaluate
  "Evaluate an expression node, returning the resulting value.
  "
  (fn [{:keys [node-type]} _env]
    node-type))


(defmethod evaluate :literal
  [{:keys [value]} _env]
  value)


(defmethod evaluate :variable
  [node env]
  (get env (:name node)))

(defmethod evaluate :grouping
  [{:keys [expression]} env]
  (evaluate expression env))


(defmethod evaluate :unary
  [{:keys [operator right]} env]
  (let [right-value (evaluate right env)]
    (cond
      (= :bang (:token-type operator)) (not right-value) ; Lox matches Clojure's concept of truthiness
      (= :minus (:token-type operator)) (- right-value)
      )))


(defmethod evaluate :binary
  [{:keys [left operator right]} env]
  (let [left-value (evaluate left env)
        right-value (evaluate right env)]
    (condp = (:token-type operator)
      :greater       (> left-value right-value)
      :greater-equal (>= left-value right-value)
      :less          (< left-value right-value)
      :less-equal    (<= left-value right-value)
      :minus         (- left-value right-value)
      :slash         (/ left-value right-value)
      :star          (* left-value right-value)
      :plus          (if (and (number? left-value)
                              (number? right-value))
                       (+ left-value right-value)
                       (str left-value right-value))
      :bang-equal    (not= left-value right-value)
      :equal-equal   (= left-value right-value)
      )))


;;-- Execute ----------------------------------------------------------


(defmulti execute
  "Execute a statement node, returning an updated environment.
  "
  (fn [{:keys [node-type]} _env]
    node-type))


(defmethod execute :expression
  [{:keys [expression]} env]
  (println (evaluate expression env))
  env)


(defmethod execute :print
  [{:keys [expression]} env]
  (println (evaluate expression env))
  env)


(defmethod execute :var
  [{:keys [name initializer]} env]
  (assoc env (:lexeme name) (when initializer (evaluate initializer env))))
