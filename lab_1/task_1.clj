;; Задача 1.1: Решение с помощью элементарных операций и рекурсии
;; Генерирует все строки длины n из заданных символов без двух одинаковых подряд

(defn generate-strings
  "Генерирует все строки длины n из symbols без одинаковых символов подряд.
  Использует только элементарные операции"
  [symbols n]
  (letfn [
    ;; Основная функция
    ;; current - текущая строка, remaining - оставшаяся длина
    (build [current remaining]
      (if (zero? remaining)
        (list current)
        (add-symbols symbols current remaining)))
    
    ;; Перебирает все символы и добавляет подходящие
    (add-symbols [syms current remaining]
      (if (empty? syms)
        '()
        (let [symbol (first syms)
              last-char (get-last-char current)]
          (if (= symbol last-char)
            ;; Символ совпадает с последним - пропускаем
            (add-symbols (rest syms) current remaining)
            ;; Символ не совпадает
            ;; Ветка 1: "Я добавил этот символ, продолжайте строить"
            ;; Ветка 2: "А давайте попробуем следующий символ для той же строки"
            (concat (build (str current symbol) (dec remaining))
                    (add-symbols (rest syms) current remaining))))))
    
    ;; Получает последний символ строки
    (get-last-char [s]
      (if (empty? s)
        nil
        (subs s (dec (count s)))))]
    
    (build "" n)))

;; Тестирование
(println "Тест 1: symbols=[a b c], n=2")
(println (generate-strings ["a" "b" "c"] 2))
(println)

(println "Тест 2: symbols=[a b c], n=3")
(println (generate-strings ["a" "b" "c"] 3))
(println)

(println "Тест 3: symbols=[a b], n=4")
(println (generate-strings ["a" "b"] 4))
