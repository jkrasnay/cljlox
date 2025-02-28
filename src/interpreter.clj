(ns interpreter)

(defmulti evaluate
  (fn [{:keys [node-type]}]
    node-type))


(defmethod evaluate :literal
  [{:keys [value]}]
  value)


(defmethod evaluate :grouping
  [{:keys [expression]}]
  (evaluate expression))


(defmethod evaluate :unary
  [{:keys [operator right]}]
  (let [right-value (evaluate right)]
    (cond
      (= :bang (:token-type operator)) (not right-value) ; Lox matches Clojure's concept of truthiness
      (= :minus (:token-type operator)) (- right-value)
      )))


(defmethod evaluate :binary
  [{:keys [left operator right]}]
  (let [left-value (evaluate left)
        right-value (evaluate right)]
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


(defmethod evaluate :print
  [{:keys [expression]}]
  (println (evaluate expression)))
