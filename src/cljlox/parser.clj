(ns cljlox.parser
  (:require
    [clojure.pprint :refer [pprint]]
    [clojure.string :as string]
    [cljlox.ast :as ast]
    [cljlox.lexer :as lexer]))


;; Like the lexer, the parser needs a parse state to thread through our parsing functions.
;;
;; The parse state is a map with the following keys:
;;
;; :tokens | Seq of the remaining tokens to be parsed.
;; :ast    | Head of the AST parsed so far
;;
;; The AST is a tree of nodes, represented by records in the cljlox.ast namespace.
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


(defn eof?
  [parse-state]
  (nil? (next-token parse-state)))


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
        (recur (assoc parse-state :ast (ast/->Binary left operator right))))
      parse-state)))


(defn literal
  ([parse-state value]
   (-> parse-state drop-token (assoc :ast (ast/->Literal value))))
  ([parse-state]
   (-> parse-state drop-token (assoc :ast (ast/->Literal (-> parse-state next-token :value))))))


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
                                          (assoc parse-state :ast (ast/->Grouping (:ast parse-state))))
    (match parse-state :identifier)     (-> parse-state
                                            drop-token
                                            (assoc :ast (ast/->Variable (:lexeme (next-token parse-state)))))
    :else (do (pprint parse-state)
              (throw-error (next-token parse-state) "Expected expression"))))



(defn finish-call
  "Called when we match a left-paren after a primary expression.
  "
  [parse-state]
  (let [parse-state (drop-token parse-state)
        callee (:ast parse-state)]
    (if (match parse-state :right-paren)
      (-> parse-state
          drop-token
          (assoc :ast (ast/->Call callee (next-token parse-state) [])))
      (let [parse-state (expression parse-state)]
        (loop [parse-state parse-state
               args [(:ast parse-state)]]
          (cond

            (match parse-state :right-paren)
            (-> parse-state
                drop-token
                (assoc :ast (ast/->Call callee (next-token parse-state) args)))

            (match parse-state :comma)
            (let [parse-state (-> parse-state
                                  drop-token
                                  expression)]
              (recur parse-state (conj args (:ast parse-state))))

            :else
            (throw-error (next-token parse-state) "Expected ')' or ','")))))))


(defn call
  [parse-state]
  (let [parse-state (primary parse-state)]
    (loop [parse-state parse-state]
      (if (match parse-state :left-paren)
        (recur (finish-call parse-state))
        parse-state))))


(defn unary
  [parse-state]
  (if (match parse-state :bang :minus)
    (let [operator (next-token parse-state)
          parse-state (unary (drop-token parse-state))]
      (assoc parse-state :ast (ast/->Unary operator (:ast parse-state))))
    (call parse-state)))


(defn factor
  [parse-state]
  (binary-expr parse-state unary :slash :star))


(defn term
  [parse-state]
  (binary-expr parse-state factor :minus :plus))


(defn comparison
  [parse-state]
  (binary-expr parse-state term :greater :greater-equal :less :less-equal))


(defn equality
  [parse-state]
  (binary-expr parse-state comparison :bang-equal :equal-equal))


(defn and-expr
  [parse-state]
  (binary-expr parse-state equality :and))


(defn or-expr
  [parse-state]
  (binary-expr parse-state and-expr :or))


(defn assignment
  [parse-state]
  (let [first-token (next-token parse-state)
        parse-state (or-expr parse-state)
        expr (:ast parse-state)]
    (if (match parse-state :equal)
      (let [;equals (:ast parse-state)
            parse-state (-> parse-state
                            drop-token
                            assignment)
            value (:ast parse-state)]
        (if (instance? cljlox.ast.Variable expr)
          (assoc parse-state :ast (ast/->Assign (:name expr) value))
          (throw-error first-token "Invalid assignment target")))
      parse-state)))


(defn expression
  [parse-state]
  (assignment parse-state))


(defn expression-statement
  [parse-state]
  (let [parse-state (-> parse-state
                        expression
                        assert-and-drop-semicolon)]
    (assoc parse-state :ast (ast/->Expression (:ast parse-state)))))


(declare statement)


(defn if-statement
  [parse-state]
  (let [parse-state (-> parse-state
                        (assert-next-token :left-paren "Expect '(' after 'if'.")
                        drop-token
                        expression)
        condition (:ast parse-state)
        parse-state (-> parse-state
                        (assert-next-token :right-paren "Expect ')' after 'if' condition.")
                        drop-token
                        statement)
        then-branch (:ast parse-state)
        [else-branch parse-state] (if (match parse-state :else)
                                    (let [parse-state (-> parse-state
                                                          drop-token
                                                          statement)]
                                      [(:ast parse-state) parse-state])
                                    [nil parse-state])]
    (assoc parse-state :ast (ast/->If condition then-branch else-branch))))


(defn print-statement
  [parse-state]
  (let [parse-state (-> parse-state
                        expression
                        assert-and-drop-semicolon)]
    (assoc parse-state :ast (ast/->Print (:ast parse-state)))))


(defn return-statement
  [parse-state]
  (let [[parse-state value] (if (match parse-state :semicolon)
                              [parse-state nil]
                              (let [parse-state (expression parse-state)]
                                [parse-state (:ast parse-state)]))]
    (-> parse-state
        assert-and-drop-semicolon
        (assoc :ast (ast/->Return value)))))


(defn while-statement
  [parse-state]
  (let [parse-state (-> parse-state
                        (assert-next-token :left-paren "Expect '(' after 'while'.")
                        drop-token
                        expression)
        condition (:ast parse-state)
        parse-state (-> parse-state
                        (assert-next-token :right-paren "Expect ')' after 'while' condition.")
                        drop-token
                        statement)
        body (:ast parse-state)]
    (assoc parse-state :ast (ast/->While condition body))))


(declare declaration)


(defn block
  [parse-state]
  (loop [parse-state parse-state
         statements []]
    (cond

      (match parse-state :right-brace)
      (-> parse-state
          drop-token
          (assoc :ast (ast/->Block statements)))

      (eof? parse-state)
      (throw-error (next-token parse-state) "Unterminated block")

      :else
      (let [parse-state (declaration parse-state)]
        (recur parse-state (conj statements (:ast parse-state)))))))


(defn statement
  [parse-state]
  (cond
    (match parse-state :if)         (-> parse-state drop-token if-statement)
    (match parse-state :print)      (-> parse-state drop-token print-statement)
    (match parse-state :return)     (-> parse-state drop-token return-statement)
    (match parse-state :while)      (-> parse-state drop-token while-statement)
    (match parse-state :left-brace) (-> parse-state drop-token block)
    :else                           (-> parse-state expression-statement)))


(defn function-definition
  [parse-state]
  (assert-next-token parse-state :identifier "Expected function name")
  (let [name (next-token parse-state)
        parse-state (drop-token parse-state)
        _ (assert-next-token parse-state :left-paren "Expected '(' after function name")
        parse-state (drop-token parse-state)
        [parse-state params] (if (match parse-state :right-paren)
                               [(drop-token parse-state) []]
                               (let [first-arg (next-token parse-state)]
                                 (loop [parse-state (drop-token parse-state)
                                        params [first-arg]]
                                   (cond

                                     (match parse-state :right-paren)
                                     [(drop-token parse-state) params]

                                     (match parse-state :comma)
                                     (let [parse-state (drop-token parse-state)]
                                       (recur (drop-token parse-state) (conj params (next-token parse-state))))

                                     :else
                                     (throw-error (next-token parse-state) "Expected ')' or ','")))))
        parse-state (do
                      (assert-next-token parse-state :left-brace "Expected '{' before function")
                      (block (drop-token parse-state)))
        block (:ast parse-state)]
    (assoc parse-state :ast (ast/->Function name params block))))


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
        (assoc parse-state :ast (ast/->Var name (:ast parse-state))))
      (-> parse-state
          assert-and-drop-semicolon
          (assoc :ast (ast/->Var name nil))))))


;; Section 6.3.3
(defn synchronize
  [parse-state]
  (loop [parse-state (drop-token parse-state)]
    (cond

      (eof? parse-state)
      parse-state

      (#{:class :fun :var :for :if :while :print :return} (:token-type (next-token parse-state)))
      parse-state

      :else
      (recur (drop-token parse-state)))))


(defn declaration
  [parse-state]
  (try
    (cond
      (match parse-state :fun) (-> parse-state drop-token function-definition)
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
                 all-errors)))
      {:statements statements
       :errors all-errors})))


(defn parse
  [s]
  (let [{:keys [errors
                tokens]} (lexer/tokenize s)]
    (if (seq errors)
      {:errors errors}
      (parse-tokens tokens))))


(comment
  (parse "a = 3;")
  (parse "1 + (3 = 3);")
  (lexer/tokenize "if (3 = x) print y; else print z;")
  (parse "if (3 = x) print y; else print z;")
  (parse "true and false;")
  (parse "foo(x, 3);")
  (parse "{ a = 3; }")
  (parse "fun foo(x, y, z) { print 1; }")
  (parse "return;")
  (parse "return 37;")
  (try
    (throw (ex-info "foo" {:value 37}))
    (catch Exception e
      (ex-data e)))
  )
