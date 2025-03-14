(ns interpreter)


(defmulti evaluate
  "Evaluate an expression node.

  Returns a tuple of the resulting value and possibly updated env.
  "
  (fn [{:keys [node-type]} _env]
    node-type))


;;-- Expressions ----------------------------------------------------------


(defmethod evaluate :literal
  [{:keys [value]} env]
  [value env])


(defmethod evaluate :variable
  [node env]
  [(get env (:name node)) env])

(defmethod evaluate :grouping
  [{:keys [expression]} env]
  (evaluate expression env))


(defmethod evaluate :unary
  [{:keys [operator right]} env]
  (let [[right-value env] (evaluate right env)]
    [(cond
       (= :bang (:token-type operator)) (not right-value) ; Lox matches Clojure's concept of truthiness
       (= :minus (:token-type operator)) (- right-value))
     env]))


(defmethod evaluate :binary
  [{:keys [left operator right]} env]
  (let [[left-value env] (evaluate left env)
        [right-value env] (evaluate right env)]
    [(condp = (:token-type operator)
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
       :equal-equal   (= left-value right-value))
     env]))


;;-- Statements ----------------------------------------------------------


(defmethod evaluate :expression
  [{:keys [expression]} env]
  (let [[value env] (evaluate expression env)]
    (println value)
    [nil env]))


(defmethod evaluate :print
  [{:keys [expression]} env]
  (let [[value env] (evaluate expression env)]
    (println value)
    [nil env]))


(defmethod evaluate :var
  [{:keys [name initializer]} env]
  (let [[value env] (if initializer
                      (evaluate initializer env)
                      [nil env])]
    [nil
     (assoc env (:lexeme name) value)]))
