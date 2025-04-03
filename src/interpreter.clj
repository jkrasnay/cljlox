(ns interpreter)


(defmulti evaluate
  "Evaluate an expression node.

  Returns a tuple of the resulting value and possibly updated env.
  "
  (fn [{:keys [node-type]} _env]
    node-type))


;;-- Environments ----------------------------------------------------------


(defrecord Environment [symbols parent])


(defn environment
  "Creates a new environment object.
  "
  [parent]
  (Environment. (java.util.HashMap.) parent))


(def globals (environment nil))


(defn env-assign
  [env name value]
  (.put (:symbols env) name value))


(defn env-get
  [env name]
  (cond

    (contains? (:symbols env) name)
    (get (:symbols env) name)

    (some? (:parent env))
    (env-get (:parent env) name)

    :else
    nil))


;;-- Expressions ----------------------------------------------------------


(defmethod evaluate :literal
  [{:keys [value]} _env]
  value)


(defmethod evaluate :variable
  [node env]
  (env-get env (:name node)))


(defmethod evaluate :grouping
  [{:keys [expression]} env]
  (evaluate expression env))


(defmethod evaluate :unary
  [{:keys [operator right]} env]
  (let [right-value (evaluate right env)]
    (cond
      (= :bang (:token-type operator)) (not right-value) ; Lox matches Clojure's concept of truthiness
      (= :minus (:token-type operator)) (- right-value))))


(defmethod evaluate :binary
  [{:keys [left operator right]} env]
  (if (#{:and :or} (:token-type operator))

    ; Short circuit evaluation for logical operators
    (let [left-value (evaluate left env)]
      (cond

        (and (= :or (:token-type operator))
             left-value)
        left-value

        (and (= :and (:token-type operator))
             (not left-value))
        left-value

        :else
        (evaluate right env)))

    ; Regular (non-logical) binary operators
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
        :equal-equal   (= left-value right-value)))))


(defmethod evaluate :assign
  [{:keys [name value]} env]
  (let [value (evaluate value env)]
    (env-assign env name value)
    value))


;;-- Statements ----------------------------------------------------------


(defmethod evaluate :expression
  [{:keys [expression]} env]
  (evaluate expression env))


(defmethod evaluate :if
  [{:keys [condition
           then-branch
           else-branch]} env]
  (let [value (evaluate condition env)]
    (if value
      (evaluate then-branch env)
      (when else-branch
        (evaluate else-branch env)))))


(defmethod evaluate :print
  [{:keys [expression]} env]
  (let [value (evaluate expression env)]
    (println value)
    nil))


(defmethod evaluate :while
  [{:keys [condition
           body]} env]
  (loop []
    (if (evaluate condition env)
      (do (evaluate body env)
          (recur))
      nil)))


(defmethod evaluate :block
  [{:keys [statements]} env]
  (doseq [statement statements]
    (evaluate statement env))
  nil)


(defmethod evaluate :var
  [{:keys [name initializer]} env]
  (let [value (when initializer
                (evaluate initializer env))]
    (env-assign env (:value name) value)
    nil))
