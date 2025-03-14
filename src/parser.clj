(ns parser
  (:require
    [clojure.pprint :refer [pprint]]
    [clojure.string :as string]
    lexer))


;; Like the lexer, the parser needs a parse state to thread through our parsing functions.
;;
;; The parse state is a map with the following keys:
;;
;; :tokens | Seq of the remaining tokens to be parsed.
;; :ast    | Head of the AST parsed so far
;;
;; The AST is a tree of nodes.  Each node is a map with the key `:node-type` corresponding
;; to the AST sub-class in the book, along with any other keys required by that node type.
;;


;;-- AST Printer ----------------------------------------------------------


(defmulti ast->string
  (fn [{:keys [node-type]}]
    node-type))


(defn parenthesize
  [name & exprs]
  (str "("
       (->> exprs
            (map ast->string)
            (cons name)
            (string/join " "))
       ")"))


(defmethod ast->string :binary
  [{:keys [left operator right]}]
  (parenthesize (:lexeme operator) left right))


(defmethod ast->string :grouping
  [{:keys [expression]}]
  (parenthesize "group" expression))


(defmethod ast->string :literal
  [{:keys [value]}]
  value)


(defmethod ast->string :unary
  [{:keys [operator right]}]
  (parenthesize (:lexeme operator) right))


;;-- Error Handling ----------------------------------------------------------


(defn throw-error
  [token message]
  (throw (ex-info (format "Line %d: %s at %s" (:line token) message (:lexeme token)) {})))


;;-- Parsing ----------------------------------------------------------


(defn next-token
  [parse-state]
  (first (:tokens parse-state)))


(defn drop-token
  [parse-state]
  (update parse-state :tokens rest))


(defn assert-next-token
  [parse-state token-type message]
  (if (= token-type (:token-type (next-token parse-state)))
    parse-state
    (throw-error (next-token parse-state) message)))


(defn assert-and-drop-semicolon
  [parse-state]
  (-> parse-state
      (assert-next-token :semicolon "Expected ';'")
      drop-token))


(defn match
  [parse-state & token-types]
  (let [next-type (:token-type (next-token parse-state))]
    (some #(= % next-type) token-types)))


(defn binary-expr
  "Parses a binary expression.

  `sub-expr` is the parse function for the higher-precedence sub-expression.
  `token-types` are one or more token types at this precendence level.
  "
  [parse-state sub-expr & token-types]
  (loop [parse-state (sub-expr parse-state)]
    (if (apply match parse-state token-types)
      (let [left (:ast parse-state)
            operator (next-token parse-state)
            parse-state (sub-expr (drop-token parse-state))
            right (:ast parse-state)]
        (recur (assoc parse-state :ast {:node-type :binary
                                        :left left
                                        :operator operator
                                        :right right})))
      parse-state)))


(defn literal
  ([parse-state value]
   (-> parse-state drop-token (assoc :ast {:node-type :literal
                                           :value value})))
  ([parse-state]
   (-> parse-state drop-token (assoc :ast {:node-type :literal
                                           :value (-> parse-state next-token :value)}))))


(declare expression)


(defn primary
  [parse-state]
  (cond
    (match parse-state :false)          (literal parse-state false)
    (match parse-state :true)           (literal parse-state true)
    (match parse-state :nil)            (literal parse-state nil)
    (match parse-state :number :string) (literal parse-state)
    (match parse-state :left-paren)     (let [parse-state (-> parse-state
                                                              drop-token
                                                              expression
                                                              (assert-next-token :right-paren "Expected ')' after expression")
                                                              drop-token)]
                                          (assoc parse-state :ast {:node-type :grouping
                                                                   :expression (:ast parse-state)}))
    (match parse-state :identifier)     (-> parse-state
                                            drop-token
                                            (assoc :ast {:node-type :variable
                                                         :name (:lexeme (next-token parse-state))}))
    :else (throw-error (next-token parse-state) "Expected expression")))


(defn unary
  [parse-state]
  (if (match parse-state :bang :minus)
    (let [operator (next-token parse-state)
          parse-state (unary (drop-token parse-state))]
      (assoc parse-state :ast {:node-type :unary
                               :operator operator
                               :right (:ast parse-state)}))
    (primary parse-state)))


(defn factor
  [parse-state]
  (binary-expr parse-state unary :slash :star))


(defn term
  [parse-state]
  (binary-expr parse-state factor :minus :plus))


(defn comparison
  [parse-state]
  (binary-expr parse-state term :greater :greater-equal :less :less-equal))


(defn expression
  [parse-state]
  (binary-expr parse-state comparison :bang-equal :equal-equal))


(defn expression-statement
  [parse-state]
  (let [parse-state (-> parse-state
                        expression
                        assert-and-drop-semicolon)]
    (assoc parse-state :ast {:node-type :expression
                             :expression (:ast parse-state)})))


(defn print-statement
  [parse-state]
  (let [parse-state (-> parse-state
                        expression
                        assert-and-drop-semicolon)]
    (assoc parse-state :ast {:node-type :print
                             :expression (:ast parse-state)})))


(defn statement
  [parse-state]
  (cond
    (match parse-state :print) (-> parse-state drop-token print-statement)
    :else                      (-> parse-state expression-statement)))


(defn var-declaration
  [parse-state]
  (assert-next-token parse-state :identifier "Expected variable name")
  (let [name (next-token parse-state)
        parse-state (drop-token parse-state)]
    (if (match parse-state :equal)
      (let [parse-state (-> parse-state
                            drop-token
                            expression
                            assert-and-drop-semicolon)]
        (assoc parse-state :ast {:node-type :var
                                 :name name
                                 :initializer (:ast parse-state)}))
      (-> parse-state
          assert-and-drop-semicolon
          (assoc :ast {:node-type :var
                       :name name})))))


;; Section 6.3.3
(defn synchronize
  [parse-state]
  (loop [parse-state (drop-token parse-state)]
    (cond
      (nil? (next-token parse-state))
      parse-state

      (#{:class :fun :var :for :if :while :print :return} (:token-type (next-token parse-state)))
      parse-state

      :else
      (recur (drop-token parse-state)))))


(defn declaration
  [parse-state]
  (try
    (cond
      (match parse-state :var) (-> parse-state drop-token var-declaration)
      :else                    (-> parse-state statement))
    (catch Exception e
      (-> parse-state
          (update :errors conj (.getMessage e))
          synchronize))))


(defn parse-tokens
  [tokens]
  (loop [tokens tokens
         statements []
         all-errors []]
    (if (seq tokens)
      (let [{:keys [tokens ast errors]} (declaration {:tokens tokens})]
        (if (seq errors)
          (recur tokens
                 statements
                 (into all-errors errors))
          (recur tokens
                 (conj statements ast)
                 errors)))
      {:statements statements
       :errors all-errors})))


(defn parse
  [s]
  (let [{:keys [errors
                tokens]} (lexer/tokenize s)]
    (if (seq errors)
      {:errors errors}
      (parse-tokens tokens))))
