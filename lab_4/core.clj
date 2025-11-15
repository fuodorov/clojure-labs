(ns core)

(defn find-first
  "Возвращает первый элемент коллекции `coll`, для которого предикат `pred` истинен."
  [pred coll]
  (first (filter pred coll)))

(defn constant 
  "Конструктор для константы" 
  [value] 
  {:pre [(or (= value 0) (= value 1))]}
  (list ::const value))

(defn constant? 
  "Проверяет, является ли выражение константой" 
  [expr] 
  (= (first expr) ::const))

(defn constant-value 
  "Получает значение константы" 
  [expr] 
  (second expr))

(defn variable 
  "Конструктор для переменной" 
  [name]
  {:pre [(keyword? name)]}
  (list ::var name))

(defn variable? 
  "Проверяет, является ли выражение переменной" 
  [expr] 
  (= (first expr) ::var))

(defn variable-name 
  "Получает имя переменной" 
  [var]
  (name (second var)))

(defn same-variables? 
  "Проверяет, являются ли две переменные одинаковыми" 
  [var1 var2]
  (and
   (variable? var1)
   (variable? var2)
   (= (variable-name var1)
      (variable-name var2))))

(defn logic-not
  "Конструктор для отрицания"
  [expr]
  (list ::not expr))

(defn logic-not?
  "Проверяет, является ли выражение отрицанием"
  [expr]
  (= (first expr) ::not))

(defn logic-or
  "Конструктор для дизъюнкции"
  [expr & rest]
  (if (empty? rest)
    expr
    (cons ::or (cons expr rest))))

(defn logic-or?
  "Проверяет, является ли выражение дизъюнкцией"
  [expr]
  (= (first expr) ::or))

(defn logic-and
  "Конструктор для конъюнкции"
  [expr & rest]
  (if (empty? rest)
    expr
    (cons ::and (cons expr rest))))

(defn logic-and?
  "Проверяет, является ли выражение конъюнкцией"
  [expr] 
  (= (first expr) ::and))

(defn logic-impl
  "Конструктор для импликации"
  [expr1 expr2] 
  (list ::impl expr1 expr2))

(defn logic-impl?
  "Проверяет, является ли выражение импликацией"
  [expr]
  (= (first expr) ::impl))

(defn logic-xor
  "Конструктор для исключающего ИЛИ (XOR). Поддерживает >=1 аргумента."
  [expr & rest]
  (if (empty? rest)
    expr
    (cons ::xor (cons expr rest))))

(defn logic-xor?
  "Проверяет, является ли выражение XOR."
  [expr]
  (= (first expr) ::xor))

(defn get-type
  "Получает тип выражения"
  [expr]
  (keyword (first expr)))

(defn same-type?
  "Проверяет, что expr1 и expr2 имеют одинаковый тип"
  [expr1 expr2]
  (= (first expr1) (first expr2)))

(defn args
  "Получает аргументы операции"
  [expr]
  (rest expr))

(defn first-arg
  "Получает первый аргумент операции"
  [expr]
  (first (rest expr)))

(defn second-arg
  "Получает второй аргумент операции"
  [expr]
  (second (rest expr)))

(defn unary?
  "Проверяет, что операция имеет ровно один аргумент."
  [expr]
  (= 1 (count (args expr))))

(defn binary?
  "Проверяет, что операция имеет ровно два аргумента."
  [expr]
  (= 2 (count (args expr))))

(defn combine-same-args
  "Объединяет аргументы одного типа в одну операцию"
  [expr]
  (if (or (constant? expr) (variable? expr))
    (list expr)
    (->> (args expr)
         (mapcat #(if (same-type? expr %)
                    (combine-same-args %)
                    (list %))))))

(defn expr-to-str
  "Преобразует выражение в строку (через список правил)."
  [expr]
  (let [-expr->str-rules
        (list
         [constant? (fn [e] (constant-value e))]
         [variable? (fn [e] (variable-name e))]
         [logic-not? (fn [e]
                       (str "(" "-" (expr-to-str (first-arg e)) ")"))]
         [logic-or? (fn [e]
                      (let [ss (map expr-to-str (args e))]
                        (str "(" (reduce #(str %1 " | " %2) (first ss) (rest ss)) ")")))]
         [logic-and? (fn [e]
                       (let [ss (map expr-to-str (args e))]
                         (str "(" (reduce #(str %1 " & " %2) (first ss) (rest ss)) ")")))]
         [logic-impl? (fn [e]
                        (str "(" (expr-to-str (first-arg e)) " -> " (expr-to-str (second-arg e)) ")"))]
         [logic-xor? (fn [e]
                       (let [ss (map expr-to-str (args e))]
                         (str "(" (reduce #(str %1 " ^ " %2) (first ss) (rest ss)) ")")))]
         [(fn [_] true) (fn [e] (str e))])
        rule (find-first #((first %) expr) -expr->str-rules)]
    ((second rule) expr)))

(declare impl-elimination)

(def -impl-elimination-rules
  (list
   ;; Устранение импликации: A -> B ~ -A | B (c рекурсией по подвыражениям)
   [logic-impl?
    (fn [expr]
      (logic-or (logic-not (impl-elimination (first-arg expr)))
                (impl-elimination (second-arg expr))))]

   ;; Рекурсивный проход: отрицание
   [logic-not?
    (fn [expr]
      (logic-not (impl-elimination (first-arg expr))))]

   ;; Рекурсивный проход: дизъюнкция
   [logic-or?
    (fn [expr]
      (apply logic-or (map impl-elimination (args expr))))]

   ;; Рекурсивный проход: конъюнкция
   [logic-and?
    (fn [expr]
      (apply logic-and (map impl-elimination (args expr))))]

   ;; База: оставить как есть
   [(fn [_] true)
    (fn [expr] expr)]))

(defn impl-elimination
  "Устранение импликации: x -> y = -x | y (через список правил)"
  [expr]
  (let [rule (find-first #((first %) expr) -impl-elimination-rules)]
    ((second rule) expr)))

(declare negation-equivalencies)

(def -negation-rules
  (list
   ;; Де Морган для дизъюнкции: -(A v B v ...) ~ (-A) & (-B) & ...
   [(fn [expr] (and (logic-not? expr) (logic-or? (first-arg expr))))
    (fn [expr]
      (let [sub (first-arg expr)]
        (apply logic-and (map #(negation-equivalencies (logic-not %)) (args sub)))))]

   ;; Де Морган для конъюнкции: -(A & B & ...) ~ (-A) v (-B) v ...
   [(fn [expr] (and (logic-not? expr) (logic-and? (first-arg expr))))
    (fn [expr]
      (let [sub (first-arg expr)]
        (apply logic-or (map #(negation-equivalencies (logic-not %)) (args sub)))))]

   ;; Двойное отрицание: -(-A) ~ A
   [(fn [expr] (and (logic-not? expr) (logic-not? (first-arg expr))))
    (fn [expr]
      (negation-equivalencies (first-arg (first-arg expr))))]

   ;; Отрицание константы
   [(fn [expr] (and (logic-not? expr) (constant? (first-arg expr))))
    (fn [expr]
      (if (= (first-arg expr) (constant 0)) (constant 1) (constant 0)))]

   ;; Рекурсивный проход по дизъюнкции
   [logic-or?
    (fn [expr]
      (apply logic-or (map negation-equivalencies (args expr))))]

   ;; Рекурсивный проход по конъюнкции
   [logic-and?
    (fn [expr]
      (apply logic-and (map negation-equivalencies (args expr))))]

   ;; Иначе — без изменений
   [(fn [_] true)
    (fn [expr] expr)]))

(defn negation-equivalencies
  "Эквивалентности отрицания (через список правил)"
  [expr]
  (let [rule (find-first #((first %) expr) -negation-rules)]
    ((second rule) expr)))

(declare absorption-law)

(defn -absorb
  [inner-op-check outer-op sub-expr1 sub-expr2]
  (cond
    (inner-op-check sub-expr1)
    (if (or (= sub-expr2 (first-arg sub-expr1))
            (= sub-expr2 (second-arg sub-expr1)))
      (absorption-law sub-expr2)
      (outer-op (absorption-law sub-expr1) (absorption-law sub-expr2)))

    (inner-op-check sub-expr2)
    (if (or (= sub-expr1 (first-arg sub-expr2))
            (= sub-expr1 (second-arg sub-expr2)))
      (absorption-law sub-expr1)
      (outer-op (absorption-law sub-expr1) (absorption-law sub-expr2)))

    :else (outer-op (absorption-law sub-expr1) (absorption-law sub-expr2))))

(def -absorption-rules
  (list
   ;; x | (x & y) = x
   [logic-or?
    (fn [expr]
      (-absorb logic-and? logic-or (first-arg expr) (second-arg expr)))]

   ;; x & (x | y) = x
   [logic-and?
    (fn [expr]
      (-absorb logic-or? logic-and (first-arg expr) (second-arg expr)))]

   ;; по умолчанию — без изменений
   [(fn [_] true)
    (fn [expr] expr)]))

(defn absorption-law
  "Закон поглощения: x | (x & y) = x; x & (x | y) = x (через список правил)"
  [expr]
  (let [rule (find-first #((first %) expr) -absorption-rules)]
    ((second rule) expr)))

(declare distributive-law)

(def -distributive-rules
  (list
   ;; (A v B v ...) & Z ~ (A & Z) v (B & Z) v ...
   [(fn [expr]
      (and (logic-and? expr) (logic-or? (first-arg expr))))
    (fn [expr]
      (let [a1 (first-arg expr)
            a2 (second-arg expr)
            parts (args a1)]
        (apply logic-or (map #(distributive-law (logic-and % a2)) parts))))]

   ;; Z & (A v B v ...) ~ (Z & A) v (Z & B) v ...
   [(fn [expr]
      (and (logic-and? expr) (logic-or? (second-arg expr))))
    (fn [expr]
      (let [a1 (first-arg expr)
            a2 (second-arg expr)
            parts (args a2)]
        (apply logic-or (map #(distributive-law (logic-and a1 %)) parts))))]

   ;; Рекурсивный проход по дизъюнкции
   [logic-or?
    (fn [expr]
      (apply logic-or (map distributive-law (args expr))))]

   ;; Иначе — без изменений
   [(fn [_] true)
    (fn [expr] expr)]))

(defn distributive-law
  "Закон дистрибутивности (через список правил): (x | y) & z = (x & z) | (y & z)"
  [expr]
  (let [rule (find-first #((first %) expr) -distributive-rules)]
    ((second rule) expr)))

(declare decompose)

(def -decompose-rules
  (list
   ;; Раскрыть одноуровневую вложенность одинаковых операций
   [(fn [expr] (or (logic-or? expr) (logic-and? expr)))
    (fn [expr]
      (cons (get-type expr) (map decompose (combine-same-args expr))))]

   ;; Иначе — вернуть как есть
   [(fn [_] true)
    (fn [expr] expr)]))

(defn decompose
  "Разложение: x & (y & z) = x & y & z (через список правил)"
  [expr]
  (let [rule (find-first #((first %) expr) -decompose-rules)]
    ((second rule) expr)))

(declare idempotent-law)

(def -idempotent-rules
  (list
   ;; Идемпотентность для конъюнкции
   [logic-and?
    (fn [expr]
      (apply logic-and (map idempotent-law (distinct (args (decompose expr))))))]

   ;; Идемпотентность для дизъюнкции
   [logic-or?
    (fn [expr]
      (apply logic-or (map idempotent-law (distinct (args (decompose expr))))))]

   ;; Рекурсивный проход через отрицание
   [logic-not?
    (fn [expr]
      (logic-not (idempotent-law (first-arg expr))))]

   ;; База
   [(fn [_] true)
    (fn [expr] expr)]))

(defn idempotent-law
  "Закон идемпотентности: x & x = x; x | x = x (через список правил)"
  [expr]
  (let [rule (find-first #((first %) expr) -idempotent-rules)]
    ((second rule) expr)))

(declare identity-and-domination-laws)

(defn -has-const?
  [expr const]
  (some #(and (constant? %) (= const %)) (args expr)))

(def -identity-rules
  (list
   ;; Для конъюнкции: x & 0 = 0; x & 1 = x
   [logic-and?
    (fn [expr]
      (let [e (decompose expr)]
        (cond
          (-has-const? e (constant 0)) (constant 0)
          (-has-const? e (constant 1)) (identity-and-domination-laws (apply logic-and (filter #(not (= % (constant 1))) (args e))))
          :else (apply logic-and (map identity-and-domination-laws (args e))))))]

   ;; Для дизъюнкции: x | 1 = 1; x | 0 = x
   [logic-or?
    (fn [expr]
      (let [e (decompose expr)]
        (cond
          (-has-const? e (constant 1)) (constant 1)
          (-has-const? e (constant 0)) (identity-and-domination-laws (apply logic-or (filter #(not (= % (constant 0))) (args e))))
          :else (apply logic-or (map identity-and-domination-laws (args e))))))]

   ;; База
   [(fn [_] true)
    (fn [expr] expr)]))

(defn identity-and-domination-laws
  "Законы тождества и доминирования (через список правил):
   x & 0 = 0; x | 1 = 1; x & 1 = x; x | 0 = x"
  [expr]
  (let [rule (find-first #((first %) expr) -identity-rules)]
    ((second rule) expr)))

(declare xor-elimination)

(defn -xor2
  "Бинарный XOR как (A & ¬B) | (¬A & B)."
  [a b]
  (logic-or (logic-and a (logic-not b))
            (logic-and (logic-not a) b)))

(def -xor-elimination-rules
  "Правила устранения XOR и рекурсивного обхода AST."
  (list
   ;; Устранение XOR: (A ^ B ^ C ...) -> дизъюнкция из конъюнкций
   [logic-xor?
    (fn [expr]
      (let [parts (map xor-elimination (args expr))]
        (reduce -xor2 (first parts) (rest parts))))]

   ;; Рекурсивный проход
   [logic-not?
    (fn [expr] (logic-not (xor-elimination (first-arg expr))))]
   [logic-or?
    (fn [expr] (apply logic-or (map xor-elimination (args expr))))]
   [logic-and?
    (fn [expr] (apply logic-and (map xor-elimination (args expr))))]
   [logic-impl?
    (fn [expr] (logic-impl (xor-elimination (first-arg expr))
                           (xor-elimination (second-arg expr))))]

   ;; База
   [(fn [_] true)
    (fn [expr] expr)]))

(defn xor-elimination
  "Устранение XOR до базовых операций (через список правил)."
  [expr]
  (let [rule (find-first #((first %) expr) -xor-elimination-rules)]
    ((second rule) expr)))

(defn dnf
  "Построение ДНФ (дизъюнктивной нормальной формы)"
  [expr]
  (->> expr
       (xor-elimination)
       (impl-elimination)
       (negation-equivalencies)
       (distributive-law)
       (absorption-law)
       (idempotent-law)
       (identity-and-domination-laws)))

(defn substitution
  "Подстановка переменных в выражение (через список правил)."
  [expr var-map]
  (let [expr (identity-and-domination-laws expr)
        -substitution-rules
        (list
         [variable?
          (fn [e]
            (let [var-name (first-arg e)]
              (if (contains? var-map var-name)
                (constant (get var-map var-name))
                e)))]
         [constant? (fn [e] e)]
         [(fn [_] true)
          (fn [e]
            (cons (get-type e) (map #(substitution % var-map) (args e))))])
        rule (find-first #((first %) expr) -substitution-rules)]
    ((second rule) expr)))

(println "x -> y =" (expr-to-str (dnf (logic-impl (variable :x) (variable :y)))))
(println "1 -> y =" (expr-to-str (dnf (logic-impl (constant 1) (variable :y)))))
(println "1 -> (x -> y) =" (expr-to-str (dnf (logic-impl (constant 1) (logic-impl (variable :x) (variable :y))))))
(println "x | y | z =" (expr-to-str (dnf (logic-or (variable :x) (logic-or (variable :y) (variable :z))))))
(println "-((x -> y) | -(y -> z)) =" (expr-to-str (dnf (logic-not (logic-or
                                                                         (logic-impl (variable :x) (variable :y))
                                                                         (logic-not (logic-impl (variable :y) (variable :z))))))))

(println "x ^ y =" (expr-to-str (dnf (logic-xor (variable :x) (variable :y)))))
