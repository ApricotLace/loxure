(ns scanner)


(def token-types
  #{;; Single-character tokens.
    :LEFT_PAREN :RIGHT_PAREN :LEFT_BRACE :RIGHT_BRACE
    :COMMA :DOT :MINUS :PLUS :SEMICOLON :SLASH :STAR

    ;; One or two character tokens.
    :BANG :BANG_EQUAL
    :EQUAL :EQUAL_EQUAL
    :GREATER :GREATER_EQUAL
    :LESS :LESS_EQUAL

    ;; Literals.
    ::IDENTIFIER :STRING :NUMBER

    ;; Keywords.
    :AND :CLASS :ELSE :FALSE :FUN :FOR :IF :NIL :OR
    :PRINT :RETURN :SUPER :THIS :TRUE :VAR :WHILE

    ;;End of file.
    ::EOF})


(defn is-at-end? [{:keys [current length]}]
  (>=  current length))

;;TODO: Shitty imperative style code, reimplement using parser-combinators or fsm ?
(defn advance [{:as ctx, :keys [current source]}]
  (println current source)
  [(.charAt source current) (update ctx :current inc)])


(defn inc-lines [ctx]
  (update ctx :line inc))


(defn add-token
  ([ctx type]
   (add-token ctx type nil))
  ([{:as ctx, :keys [start current line source]} type literal]
   (update ctx
           :tokens
           conj
           {:type type
            :text (subs source start current)
            :literal literal
            :line line})))


(defn curr-char [{:keys [current source]}]
  (.charAt source current))


(defn match [ctx expected]
  (cond
    (is-at-end? ctx) false
    (not= (curr-char ctx) expected) false
    :else (update ctx :current inc)))


(defn peek [ctx]
  (cond
    (is-at-end? ctx) \0
    :else (curr-char ctx)))


(defn init-scanner-ctx [source]
  {:start 0
   :current 0
   :line 1
   :length (count source)
   :source source
   :tokens []})


(defn set-start=current [{:as ctx, :keys [current]}]
  (assoc ctx :start current))


(defn slurp-comment [ctx]
  (loop [ctx' ctx]
    (if (and (not= (peek ctx') \newline) (not (is-at-end? ctx')))
      (recur (second (advance ctx')))
      ctx')))


(defn scan-tokens [source]
  (loop [ctx' (init-scanner-ctx source)]
    (if-not (is-at-end? ctx')
      (let [[c ctx] ((comp advance set-start=current) ctx')]
        (case c
          \( (recur (add-token ctx :LEFT_PAREN))
          \) (recur (add-token ctx :RIGHT_PAREN))
          \{ (recur (add-token ctx :LEFT_BRACE))
          \} (recur (add-token ctx :RIGHT_BRACE))
          \, (recur (add-token ctx :COMMA))
          \. (recur (add-token ctx :DOT))
          \- (recur (add-token ctx :MINUS))
          \+ (recur (add-token ctx :PLUS))
          \; (recur (add-token ctx :SEMICOLON))
          \* (recur (add-token ctx :STAR))
          \! (recur (add-token ctx (if (match ctx \=) :BANG_EQUAL :BANG)))
          \= (recur (add-token ctx (if (match ctx \=) :EQUAL_EQUAL :EQUAL)))
          \< (recur (add-token ctx (if (match ctx \=) :LESS_EQUAL :LESS)))
          \> (recur (add-token ctx (if (match ctx \=) :GREATER_EQUAL :GREATER)))
          \/ (recur (if (match ctx \/) (slurp-comment ctx) (add-token ctx :SLASH)))
          \space (recur ctx)
          \newline (recur (inc-lines ctx))))
      ctx')))


(comment

  (scan-tokens "// this is a comment
(( )){} // grouping stuff
!*+-/=<> <= == // operators")

  )
