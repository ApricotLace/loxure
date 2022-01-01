(ns scanner
  (:require [clojure.string :as str]))


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
(defn advance
  "Consume curr char from stream, inc curr pointer by 1
  => pair of consumed char & updated ctx"
  [{:as ctx, :keys [current source]}]
  [(.charAt source current) (update ctx :current inc)]) ;;TODO Smells.


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
  (if (is-at-end? ctx)
    \0
    (curr-char ctx)))


(defn peek-next [{:as ctx, :keys [current source length]}]
  (if (>= (inc current) length)
    \0
    (.charAt source (inc current))))


(defn init-scanner-ctx [source]
  {:start 0
   :current 0
   :line 1
   :length (count source)
   :source source
   :tokens []})


(defn set-start=current [{:as ctx, :keys [current]}]
  (assoc ctx :start current))


(defn omit-comment [ctx]
  (loop [ctx' ctx]
    (if (and (not= (peek ctx') \newline) (not (is-at-end? ctx')))
      (recur (second (advance ctx')))
      ctx')))


(defn trim-quotes [{:keys [source start current]}]
  (subs source (inc start) (dec current)))


(defn slurp-string-literal [ctx]
  (let [partially-slurped-str-literal-ctx
        (loop [ctx' ctx]
          (if (and (not= (peek ctx') \") (not (is-at-end? ctx')))
            (recur (cond-> ctx'
                     (= (peek ctx') \newline)
                     (inc-lines)
                     :always ((comp second advance))))
            ctx'))]
    (if (is-at-end? partially-slurped-str-literal-ctx)
      "Throw error :(" #_TODO
      (let [[_ ctx'] (advance partially-slurped-str-literal-ctx)]
        (add-token ctx' :STRING (trim-quotes ctx'))))))


(defn is-digit? [c]
  (Character/isDigit c))


(defn slurp-number-literal [ctx]
  (let [ctx'
        (loop [ctx' ctx]
          (if (is-digit? (peek ctx'))
            (recur (second (advance ctx')))
            ctx'))

        {:as ctx', :keys [start current source]}
        (if (and (= (peek ctx') \.) (is-digit? (peek-next ctx')))
          (loop [ctx' (second (advance ctx'))]
            (if (is-digit? (peek ctx'))
              (recur (second (advance ctx')))
              ctx'))
          ctx')]

    (->> (subs source start current)
         (Double/parseDouble)
         (add-token ctx' :NUMBER ))))


(defn is-alpha? [c]
  (Character/isLetter c))


(defn is-alpha-num? [c]
  (or (Character/isLetterOrDigit c)
      (= c \_)))


(def keywords
  {"and"   :AND
   "class" :CLASS
   "else"  :ELSE
   "false" :FALSE
   "for"   :FOR
   "fun"   :FUN
   "if"    :IF
   "nil"   :NIL
   "or"    :OR
   "print" :PRINT
   "return":RETURN
   "super" :SUPER
   "this"  :THIS
   "true"  :TRUE
   "var"   :VAR
   "while" :WHILE})


(defn slurp-identifier [ctx]
  (let [{:as ctx', :keys [source start current]}
        (loop [ctx' ctx]
          (if (is-alpha-num? (peek ctx'))
            (recur (second (advance ctx')))
            ctx'))
        text (subs source start current)
        token-type (if-let [tt (get keywords text)] tt :IDENTIFIER)]
    (add-token ctx' token-type)))


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
          \! (recur (add-token (if-let [cctx (match ctx \=)]
                                 cctx ctx) (if (match ctx \=) :BANG_EQUAL :BANG)))
          \= (recur (add-token (if-let [cctx (match ctx \=)]
                                 cctx ctx) (if (match ctx \=) :EQUAL_EQUAL :EQUAL))) ;;Uh oh, stinky
          \< (recur (add-token (if-let [cctx (match ctx \=)]
                                 cctx ctx) (if (match ctx \=) :LESS_EQUAL :LESS)))
          \> (recur (add-token (if-let [cctx (match ctx \=)]
                                 cctx ctx) (if (match ctx \=) :GREATER_EQUAL :GREATER)))
          \/ (recur (if (match ctx \/) (omit-comment (second (advance ctx))) (add-token ctx :SLASH)))
          \space (recur ctx)
          \newline (recur (inc-lines ctx))
          \" (recur (slurp-string-literal ctx))
          (cond
            (is-digit? c)
            (recur (slurp-number-literal ctx))

            (is-alpha? c)
            (recur (slurp-identifier ctx))

            :error
            '>?)))
      ctx')))


(comment

  (scan-tokens "// this is a comment
((\"Lemon\" \"Apple\")){} // grouping stuff
!*+-/=<> <= == 5490 12.43 // operators
print \"Hello, World!\"")



  )
