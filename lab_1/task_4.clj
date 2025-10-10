;; Задача 1.4: Решение основной задачи через map/reduce/filter
;; Генерация всех строк длины n без одинаковых символов подряд

(defn generate-strings-functional
  "Генерирует все строки длины n из symbols без одинаковых символов подряд.
  Использует функционалы map, reduce, filter."
  [symbols n]
  (letfn [
    ;; Получает последний символ строки как строку
    (last-char [s]
      (when (seq s) (str (last s))))
    
    ;; Добавляет все возможные символы к одной строке, 
    ;; исключая повторы последнего символа
    (append-all-symbols [s]
      (->> symbols
           (remove #(= % (last-char s)))
           (map #(str s %))))
    
    ;; Генерирует все строки на следующем уровне
    (next-level [strings]
      (mapcat append-all-symbols strings))]
    
    ;; Начинаем с одной пустой строки и применяем n раз next-level
    (nth (iterate next-level [""]) n)))

;; Тестирование
(println "=== Задача 1.4: Решение через map/reduce/filter ===")
(println "Тест 1: symbols=[a b c], n=2")
(println (generate-strings-functional ["a" "b" "c"] 2))
(println)

(println "Тест 2: symbols=[a b c], n=3")
(println (generate-strings-functional ["a" "b" "c"] 3))
(println)

(println "Тест 3: symbols=[a b], n=4")
(println (generate-strings-functional ["a" "b"] 4))
(println)
