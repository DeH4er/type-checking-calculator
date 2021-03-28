(ns p6-type-checking-calculator.core)

; parsing grammar
;; expr -> term
;; term -> factor (("+" | "-") factor)*
;; factor -> unary (("/" | "*") unary)*
;; unary -> "-" unary | cast
;; cast -> ("i" | "f") cast | num
;; num -> int | float

; scanner

(defn eat
  "Takes string, returns '(<char> <rest string>) or '(nil <emtpy string>)"
  [s]
  (if (empty? s)
    (list nil "")
    (list (first s) (rest s))))

(defn is-digit?
  [c]
  (Character/isDigit c))

(defn is-op?
  [c]
  (or
   (= c \+)
   (= c \-)
   (= c \*)
   (= c \/)))

(defn is-whitespace?
  [c]
  (or
   (= c \space)
   (= c \tab)
   (= c \newline)
   (= c \return)))

(defn is-cast?
  [c]
  (or
   (= c \f)
   (= c \i)))

(defn is-dot?
  [c]
  (= c \.))

(defn vec-to-str
  [vec]
  (apply str vec))

(defn parse-int
  [vec]
  (Integer/parseInt (vec-to-str vec)))

(defn parse-float
  [vec]
  (Float/parseFloat (vec-to-str vec)))

(defn parse-num
  [type num-str]
  (case type
    :float (parse-float num-str)
    :int (parse-int num-str)))

(defn scan-digit
  [c s]
  (loop [num-str [c]
         [c rest] (eat s)
         type :int]
    (cond
      (nil? c) (list {:type type :val (parse-num type num-str)} rest)
      (is-digit? c) (recur (conj num-str c) (eat rest) type)
      (is-dot? c) (recur (conj num-str c) (eat rest) :float)
      :else (list {:type type :val (parse-num type num-str)} rest))))

(defn scan-tok
  "Takes string, returns '(<token> <rest string>)"
  [s]
  (loop [[c rest] (eat s)]
    (cond
      (nil? c) (list {:type :eof} "")
      (is-whitespace? c) (recur (eat rest))
      (is-op? c) (list {:type :op
                        :val c} rest)
      (is-digit? c) (scan-digit c rest)
      (is-cast? c) (list {:type :cast :val c} rest)
      :else (list {:type :err :c c} rest))))

; parser

(defn int-parser
  "Takes string, returns '(<int ast> <rest string>) or nil"
  [s]
  (let [[tok rest] (scan-tok s)]
    (if (= (tok :type) :int)
      (list tok rest)
      nil)))

(defn float-parser
  "Takes string, returns '(<float ast> <rest string>) or nil"
  [s]
  (let [[tok rest] (scan-tok s)]
    (if (= (tok :type) :float)
      (list tok rest)
      nil)))

(defn num-parser
  "Takes string, returns '(<num ast> <rest string>) or nil"
  [s]
  (let [int-parsed (int-parser s)]
    (if-not (nil? int-parsed)
      (list {:type :num :tok (nth int-parsed 0)} (nth int-parsed 1))
      (let [float-parsed (float-parser s)]
        (if-not (nil? float-parsed)
          (list {:type :num :tok (nth float-parsed 0)} (nth float-parsed 1))
          nil)))))

(defn cast-symbol-parser
  [s]
  (let [[tok rest] (scan-tok s)]
    (if (= (tok :type) :cast)
      (list (tok :val) rest)
      nil)))

(defn cast-parser
  [s]
  (let [cast-symbol-parsed (cast-symbol-parser s)]
    (if-not (nil? cast-symbol-parsed)
      (let [[cast-symbol cast-rest] cast-symbol-parsed
            cast-parsed (cast-parser cast-rest)]
        (if-not (nil? cast-parsed)
          (let [[cast-tok cast-rest] cast-parsed]
            (list {:type :cast :cast-symbol cast-symbol :tok cast-tok} cast-rest))
          nil))
      (let [num-parsed (num-parser s)]
        (if-not (nil? num-parsed)
          (let [[num-tok num-rest] num-parsed]
            (list {:type :cast :tok num-tok} num-rest)))))))

(defn op-parser
  "Takes operators and string, returns '(<operator> <rest>) if matches operator, otherwise nil"
  [ops s]
  (let [[tok rest] (scan-tok s)]
    (if (and
         (= (tok :type) :op)
         (some #(= % (tok :val)) ops))
      (list (tok :val) rest)
      nil)))

(defn unary-parser
  "Takes string, returns '(<unary ast> <rest>) if matches unary, otherwise nil"
  [s]
  (let [op-parsed (op-parser [\-] s)]
    (if-not (nil? op-parsed)
      (let [[op rest-op] op-parsed
            unary-parsed (unary-parser rest-op)]
        (if-not (nil? unary-parsed)
          (let [[tok rest-unary] unary-parsed]
            (list {:type :unary :op op :tok tok} rest-unary))
          nil))
      (let [cast-parsed (cast-parser s)]
        (if-not (nil? cast-parsed)
          (let [[tok rest] cast-parsed]
            (list {:type :unary :tok tok} rest))
          nil)))))

(defn factor-rest-parser
  [s]
  (loop [toks []
         rest s]
    (let [op-parsed (op-parser [\/ \*] rest)]
      (if-not (nil? op-parsed)
        (let [[op op-rest] op-parsed
              unary-parsed (unary-parser op-rest)]
          (if-not (nil? unary-parsed)
            (let [[unary-tok unary-rest] unary-parsed]
              (recur (conj toks {:op op :tok unary-tok}) unary-rest))
            (list toks rest)))
        (list toks rest)))))

(defn factor-parser
  [s]
  (let [unary-parsed (unary-parser s)]
    (if-not (nil? unary-parsed)
      (let [[unary-tok unary-rest] unary-parsed
            [factor-rest-toks factor-rest] (factor-rest-parser unary-rest)]
        (list {:type :factor :first unary-tok :rest factor-rest-toks} factor-rest))
      nil)))

(defn term-rest-parser
  [s]
  (loop [toks []
         rest s]
    (let [op-parsed (op-parser [\+ \-] rest)]
      (if-not (nil? op-parsed)
        (let [[op op-rest] op-parsed
              factor-parsed (factor-parser op-rest)]
          (if-not (nil? factor-parsed)
            (let [[factor-tok factor-rest] factor-parsed]
              (recur (conj toks {:op op :tok factor-tok}) factor-rest))
            (list toks rest)))
        (list toks rest)))))

(defn term-parser
  [s]
  (let [factor-parsed (factor-parser s)]
    (if-not (nil? factor-parsed)
      (let [[factor-tok factor-rest] factor-parsed
            [term-rest-toks term-rest] (term-rest-parser factor-rest)]
        (list {:type :term :first factor-tok :rest term-rest-toks} term-rest))
      nil)))

(defn expr-parser
  [s]
  (let [term-parsed (term-parser s)]
    (if-not (nil? term-parsed)
      (let [[tok rest] term-parsed]
        (list {:type :expr :tok tok} rest))
      nil)))

; type checker

(defn type-check-float
  [float-ast]
  (case (float-ast :type)
    :float {:type :float}
    {:type :err :reason "Token is not float"}))

(defn type-check-int
  [int-ast]
  (case (int-ast :type)
    :int {:type :int}
    {:type :err :reason "Token is not integer"}))

(defn type-check-num
  [num-ast]
  (let [num-tok (num-ast :tok)
        num-type (num-tok :type)]
    (case num-type
      :int (type-check-int num-tok)
      :float (type-check-float num-tok)
      {:type :err :reason "Unsupported number type"})))

(defn type-check-cast
  [cast-ast]
  (let [tok (cast-ast :tok)
        type (tok :type)
        cast-symbol (cast-ast :cast-symbol)]
    (case cast-symbol
      \i {:type :int}
      \f {:type :float}
      (case type
        :num (type-check-num tok)
        :cast (type-check-cast tok)
        {:type :err :reason "Unsupported cast type"}))))

(defn type-check-unary
  [unary-ast]
  (let [tok (unary-ast :tok)
        type (tok :type)]
    (case type
      :cast (type-check-cast tok)
      :unary (type-check-unary tok)
      {:type :err :reason "Unsupported unary type"})))

(defn type-check-factor
  [factor-ast]
  (let [first-tok (factor-ast :first)
        rest-toks (factor-ast :rest)
        first-tok-type (type-check-unary first-tok)
        rest-types (map #(type-check-unary (% :tok)) rest-toks)
        all-have-same-type (every? #(= first-tok-type %) rest-types)]
    (if all-have-same-type
      first-tok-type
      {:type :err :reason "Factor has different types"})))

(defn type-check-term
  [term-ast]
  (let [first-tok (term-ast :first)
        rest-toks (term-ast :rest)
        first-tok-type (type-check-factor first-tok)
        rest-types (map #(type-check-factor (% :tok)) rest-toks)
        all-have-same-type (every? #(= first-tok-type %) rest-types)]
    (if all-have-same-type
      first-tok-type
      {:type :err :reason "Term has different types"})))

(defn type-check-expr
  [expr-ast]
  (let [term-tok (expr-ast :tok)]
    (type-check-term term-tok)))

; interpreter

(defn run-int
  [int-ast]
  (int-ast :val))

(defn run-float
  [float-ast]
  (float-ast :val))

(defn run-num
  [num-ast]
  (let [num-tok (num-ast :tok)
        num-type (num-tok :type)]
    (case num-type
      :int (run-int num-tok)
      :float (run-float num-tok))))

(defn run-cast
  [cast-ast]
  (let [cast-symbol (cast-ast :cast-symbol)
        cast-tok (cast-ast :tok)]
    (case cast-symbol
      \i (int (run-cast cast-tok))
      \f (float (run-cast cast-tok))
      (run-num cast-tok))))

(defn run-unary
  [unary-ast]
  (let [{tok :tok op :op} unary-ast]
    (case op
      \- (- (run-unary tok))
      (run-cast tok))))

(defn run-factor
  [factor-ast]
  (let [{:keys [first rest]} factor-ast
        first-eval (run-unary first)
        reduce-fn (fn [acc {:keys [op tok]}]
                    (let [tok-eval (run-unary tok)]
                      (case op
                        \* (* acc tok-eval)
                        \/ (/ acc tok-eval))))]

    (reduce reduce-fn first-eval rest)))

(defn run-term
  [term-ast]
  (let [{:keys [first rest]} term-ast
        first-eval (run-factor first)
        reduce-fn (fn [acc {:keys [op tok]}]
                    (let [tok-eval (run-factor tok)]
                      (case op
                        \+ (+ acc tok-eval)
                        \- (- acc tok-eval))))]

    (reduce reduce-fn first-eval rest)))

(defn run-expr
  [expr-ast]
  (run-term (expr-ast :tok)))

(defn run
  [s]
  (let [ast (first (expr-parser s))
        type-check (type-check-expr ast)]
    (if (= :err (type-check :type))
      (println "Type error: " (type-check :reason))
      (run-expr ast))))

(run "1") ; 1
(run "-1") ; -1

(run "1.0") ; 1.0
(run "-1.0") ; -1.0

(run "i1.0") ; 1
(run "-i1.0") ; -1

(run "f1") ; 1.0
(run "-f1") ; -1.0

(run "1 + 1") ; 2
(run "2 + 2 * 2") ; 6

(run "2 / 2") ; 1

(run "2.0 / 2") ; Type error
(run "2.0 / f2") ; 1.0