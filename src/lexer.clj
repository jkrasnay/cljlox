(ns lexer)

;; Here we use a state machine fed by each character in the source,
;; one at a time.
;;
;; The `lex` map tracks the internal state of the lexer:
;;
;; :state  | Keyword representing the current lexer state.
;; :chars  | Vector of chars representing the current lexeme.
;; :line   | Current line number.
;; :tokens | Vector of token maps.  This is ultimately the lexer's output.
;; :errors | Vector of lexer errors.
;;
;; Each token is a map:
;;
;; :type    | Keyword representing the token type.
;; :value   | Value of the token, where applicable.
;; :line    | Line on which the token occurred.
;;
;; Each lexer error is a map:
;;
;; :line    | Line on which the error occurred.
;; :message | Error message



;; See section 4.2.1
(def single-tokens
  {\( :left-paren
   \) :right-paren
   \{ :left-brace
   \} :right-brace
   \, :comma
   \. :dot
   \- :minus
   \+ :plus
   \; :semicolon
   \* :star})


(def keywords
  #{"and"
    "class"
    "else"
    "false"
    "for"
    "fun"
    "if"
    "nil"
    "or"
    "print"
    "return"
    "super"
    "this"
    "true"
    "var"
    "while"})


(defn reset
  [lex]
  (assoc lex :chars []
         :state :start))


(defn append-char
  [lex c]
  (update lex :chars conj c))


(defn add-token
  ([lex token-type]
   (add-token lex token-type nil))
  ([lex token-type value-fn]
   (let [{:keys [chars
                 line]} lex
         lexeme (apply str chars)]
     (-> lex
         (update :tokens conj (cond-> {:token-type token-type
                                       :lexeme lexeme
                                       :line line}
                                value-fn
                                (assoc :value (value-fn lexeme))))
         reset))))


(defn set-state
  [lex state]
  (assoc lex :state state))


(defn report-error
  [lex message]
  (-> lex
      (update :errors conj (format "Line %d: %s" (:line lex) message))
      reset))


(defn next-line
  [lex]
  (update lex :line inc))


(defn digit?
  [c]
  (when (char? c)
    (<= (byte \0) (byte c) (byte \9))))


(defn alpha?
  [c]
  (when (char? c)
    (or (<= (byte \a) (byte c) (byte \z))
        (<= (byte \A) (byte c) (byte \Z))
        (= \_ c))))


(defn alpha-numeric?
  [c]
  (or (alpha? c)
      (digit? c)))


(defmulti consume-char
  "Consumes the next character, returning an updated `lex` structure.
  Dispatches on `:state`.
  "
  (fn [lex c]
    (:state lex)))


(defmethod consume-char :start
  [lex c]
  (cond
    (#{\space \tab :eof} c) lex
    (= \newline c)     (-> lex next-line)
    (single-tokens c)  (-> lex (append-char c) (add-token (single-tokens c)))
    (= \! c)           (-> lex (append-char c) (set-state :bang))
    (= \= c)           (-> lex (append-char c) (set-state :equal))
    (= \< c)           (-> lex (append-char c) (set-state :less))
    (= \> c)           (-> lex (append-char c) (set-state :greater))
    (= \/ c)           (-> lex (append-char c) (set-state :slash))
    (= \" c)           (-> lex (append-char c) (set-state :string))
    (digit? c)         (-> lex (append-char c) (set-state :number))
    (alpha? c)         (-> lex (append-char c) (set-state :ident))
    :else              (-> lex (report-error (str "Unrecognized character: " c)))))


(defmethod consume-char :bang
  [lex c]
  (cond
    (= \= c) (-> lex (append-char c) (add-token :bang-equal))
    :else    (-> lex (add-token :bang) (consume-char c))))


(defmethod consume-char :equal
  [lex c]
  (cond
    (= \= c) (-> lex (append-char c) (add-token :equal-equal))
    :else    (-> lex (add-token :equal) (consume-char c))))


(defmethod consume-char :less
  [lex c]
  (cond
    (= \= c) (-> lex (append-char c) (add-token :less-equal))
    :else    (-> lex (add-token :less) (consume-char c))))


(defmethod consume-char :greater
  [lex c]
  (cond
    (= \= c) (-> lex (append-char c) (add-token :greater-equal))
    :else    (-> lex (add-token :greater) (consume-char c))))


(defmethod consume-char :slash
  [lex c]
  (cond
    (= \/ c) (-> lex (set-state :comment))
    :else    (-> lex (add-token :slash) (consume-char c))))


(defmethod consume-char :comment
  [lex c]
  (cond
    (= \newline c) (-> lex next-line (set-state :start))
    :else          lex))


(defn trim-quotes
  [s]
  (subs s 1 (dec (count s))))


(defmethod consume-char :string
  [lex c]
  (cond
    (= \newline c) (-> lex next-line (append-char c))
    (= \" c)       (-> lex (append-char c) (add-token :string trim-quotes))
    (= :eof c)     (-> lex (report-error "Unterminated string") (consume-char c))
    :else          (-> lex (append-char c))))


(defmethod consume-char :number
  [lex c]
  (cond
    (digit? c) (-> lex (append-char c))
    (= \. c)   (-> lex (append-char c) (set-state :frac))
    :else      (-> lex (add-token :number parse-double) (consume-char c))))


;; TODO: small bug here.  The book says that numbers cannot end with a period.
;; To fix this should perhaps have an :frac-first state that only accepts
;; the dot as part of the number if the next char is a digit; else, it
;; adds the integer before the dot and re-consumes both the dot and c.

(defmethod consume-char :frac
  [lex c]
  (cond
    (digit? c) (-> lex (append-char c))
    :else      (-> lex (add-token :number parse-double) (consume-char c))))


(defn add-ident
  [lex]
  (let [ident (apply str (:chars lex))]
    (if (keywords ident)
      (add-token lex (keyword ident))
      (add-token lex :identifier identity))))


(defmethod consume-char :ident
  [lex c]
  (cond
    (alpha-numeric? c) (-> lex (append-char c))
    :else              (-> lex add-ident (consume-char c))))


(defn tokenize
  [s]
  (-> (reduce consume-char
              {:state :start
               :chars []
               :line 1
               :tokens []
               :errors []}
              s)
      (consume-char :eof)
      (select-keys [:tokens :errors])))


(comment

  (defn dump
    [{:keys [tokens errors]}]
    (doseq [{:keys [line message]} errors]
      (println "Error, line" line "-" message))
    tokens)

  (tokenize "%")
  (tokenize "\"sdfj")
  (dump (tokenize " + - !+"))
  (dump (tokenize " ( + = )"))
  (dump (tokenize " ( + /= )"))
  (dump (tokenize "+ // this is the rest
                  ="))
  (dump (tokenize "  \"my
                  string\""))
  (dump (tokenize "12.34 + 4"))
  (dump (tokenize "name_2 = \"Ringo\" or age > 42"))
  (dump (tokenize "name_2=\"Ringo\"or age>42"))
  (dump (tokenize "1.2."))
  (dump (tokenize "person.first_name"))
  (dump (tokenize "\"unterminated"))

  )
