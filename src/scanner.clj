(ns scanner
  (:require [clojure.string :as str]))


(defn print-error [& forms]
  (binding [*out* *err*]
    (apply println forms)))


(defn report-error [{:as ctx, :keys [line]} where message]
  (print-error (format "[Line: %s] Error %s: %s" line where message))
  (assoc ctx :error {:where where :message message}))


(defn is-at-end? [{:keys [current length]}]
  (>=  current length))


(defn advance
  "Consume char at ctx:current position"
  [{:keys [current source]}]
  (.charAt source current))


(defn inc-curr-ptr
  [ctx]
  (update ctx :current inc))


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
    \newline
    (curr-char ctx)))


(defn peek-next [{:as ctx, :keys [current source length]}]
  (if (>= (inc current) length)
    \newline
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
      (recur (inc-curr-ptr ctx'))
      ctx')))


(defn trim-quotes [{:keys [source start current]}]
  (subs source (inc start) (dec current)))


(defn slurp-string-literal [{:as ctx, :keys [current]}]
  (let [partially-slurped-str-literal-ctx
        (loop [ctx' ctx]
          (if (and (not= (peek ctx') \") (not (is-at-end? ctx')))
            (recur (cond-> ctx'
                     (= (peek ctx') \newline) (inc-lines)
                     :always inc-curr-ptr))
            ctx'))]
    (if (is-at-end? partially-slurped-str-literal-ctx)
      (report-error ctx "" (format "Unmatched quote at position %s" current))
      (let [ctx' (inc-curr-ptr partially-slurped-str-literal-ctx)]
        (add-token ctx' :STRING (trim-quotes ctx'))))))


(defn is-digit? [c]
  (Character/isDigit c))


(defn slurp-number-literal [ctx]
  (let [ctx'
        (loop [ctx' ctx]
          (if (is-digit? (peek ctx'))
            (recur (inc-curr-ptr ctx'))
            ctx'))

        {:as ctx', :keys [start current source]}
        (if (and (= (peek ctx') \.) (is-digit? (peek-next ctx')))
          (loop [ctx' (inc-curr-ptr ctx')]
            (if (is-digit? (peek ctx'))
              (recur (inc-curr-ptr ctx'))
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
            (recur (inc-curr-ptr ctx'))
            ctx'))
        text (subs source start current)
        token-type (if-let [tt (get keywords text)] tt :IDENTIFIER)]
    (add-token ctx' token-type)))


(defn scan-tokens [source]
  (loop [ctx' (init-scanner-ctx source)]
    (if-not (or (:error ctx') (is-at-end? ctx'))
      (let [ctx (set-start=current ctx')
            c (advance ctx)
            ctx (inc-curr-ptr ctx)]
        (recur
         (case c
           \( (add-token ctx :LEFT_PAREN)
           \) (add-token ctx :RIGHT_PAREN)
           \{ (add-token ctx :LEFT_BRACE)
           \} (add-token ctx :RIGHT_BRACE)
           \, (add-token ctx :COMMA)
           \. (add-token ctx :DOT)
           \- (add-token ctx :MINUS)
           \+ (add-token ctx :PLUS)
           \; (add-token ctx :SEMICOLON)
           \* (add-token ctx :STAR)
           \! (add-token (or (match ctx \=) ctx) (if (match ctx \=) :BANG_EQUAL :BANG))
           \= (add-token (or (match ctx \=) ctx) (if (match ctx \=) :EQUAL_EQUAL :EQUAL))
           \< (add-token (or (match ctx \=) ctx) (if (match ctx \=) :LESS_EQUAL :LESS))
           \> (add-token (or (match ctx \=) ctx) (if (match ctx \=) :GREATER_EQUAL :GREATER))
           \/ (if (match ctx \/) (omit-comment (inc-curr-ptr ctx)) (add-token ctx :SLASH))
           \space ctx
           \newline (inc-lines ctx)
           \" (slurp-string-literal ctx)
           (cond
             (is-digit? c)
             (slurp-number-literal ctx)

             (is-alpha? c)
             (slurp-identifier ctx)

             :else
             (report-error ctx "" (format "Panic. Ilegal char %s" c))))))
      ctx')))


(comment

  (scan-tokens "\"!\" a")


  (scan-tokens "// this is a comment
  identifier b and c
  ((\"Lemon\" \"Apple\")){} // grouping stuff
  !*+-/=<> <= == 5490 12.43 // operators
  print \"Hello, World!\"
  b and c")



  )
