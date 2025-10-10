;; Задача 1.2: Хвостовая рекурсия
;; Переписываем решение задачи 1.1 так, чтобы все рекурсивные вызовы были хвостовыми

(defn generate-strings-tail
  "Генерирует все строки длины n из symbols без одинаковых символов подряд.
  Использует хвостовую рекурсию."
  [symbols n]
  (letfn [
    ;; Основная функция
    ;; current - текущая строка, remaining - оставшаяся длина, acc - аккумулятор результатов 
    (build [current remaining acc]
      (if (zero? remaining)
        (cons current acc)
        (add-symbols symbols current remaining acc)))
    
    ;; Перебирает символы (хвостовая рекурсия)
    (add-symbols [syms current remaining acc]
      (if (empty? syms)
        acc
        (let [symbol (first syms)
              last-char (get-last-char current)]
          (if (= symbol last-char)
            ;; Пропускаем совпадающий символ
            (recur (rest syms) current remaining acc)
            ;; Добавляем новую строку и продолжаем
            (recur (rest syms) 
                   current 
                   remaining 
                   (build (str current symbol) (dec remaining) acc))))))
    
    ;; Получает последний символ строки
    (get-last-char [s]
      (if (empty? s)
        nil
        (subs s (dec (count s)))))]
    
    (build "" n '())))

;; Тестирование
(println "=== Задача 1.2: Хвостовая рекурсия ===")
(println "Тест 1: symbols=[a b c], n=2")
(println (generate-strings-tail ["a" "b" "c"] 2))
(println)

(println "Тест 2: symbols=[a b c], n=3")
(println (generate-strings-tail ["a" "b" "c"] 3))
(println)

(println "Тест 3: symbols=[a b], n=4")
(println (generate-strings-tail ["a" "b"] 4))