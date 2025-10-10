;; Задача 1.3: Реализация my-map и my-filter через reduce
;; Определяем аналоги map и filter используя только reduce и базовые операции

(defn my-map
  [f coll]
  (reduce (fn [acc item]
            (concat acc (list (f item))))
          '()
          coll))

(defn my-filter
  [pred coll]
  (reduce (fn [acc item]
            (if (pred item)
              (concat acc (list item))
              acc))
          '()
          coll))

;; Тестирование my-map
(println "=== Задача 1.3: my-map и my-filter через reduce ===")

(println "my-map: удвоение чисел")
(println "Исходный список: [1 2 3 4]")
(println "Результат my-map:" (my-map #(* 2 %) [1 2 3 4]))
(println "Результат map:   " (map #(* 2 %) [1 2 3 4]))
(println)

;; Тестирование my-filter
(println "my-filter: чётные числа")
(println "Исходный список: [1 2 3 4 5 6 7 8]")
(println "Результат my-filter:" (my-filter even? [1 2 3 4 5 6 7 8]))
(println "Результат filter:   " (filter even? [1 2 3 4 5 6 7 8]))
(println)

(println "my-filter: числа больше 5")
(println "Исходный список: [1 3 5 7 9 11]")
(println "Результат my-filter:" (my-filter #(> % 5) [1 3 5 7 9 11]))
(println "Результат filter:   " (filter #(> % 5) [1 3 5 7 9 11]))
(println)
