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

(println "my-map: x2")
(println "[1 2 3 4]")
(println "Result my-map:" (my-map #(* 2 %) [1 2 3 4]))
(println "Result map:   " (map #(* 2 %) [1 2 3 4]))
(println)

;; Тестирование my-filter
(println "my-filter: even")
(println "[1 2 3 4 5 6 7 8]")
(println "Result my-filter:" (my-filter even? [1 2 3 4 5 6 7 8]))
(println "Result filter:   " (filter even? [1 2 3 4 5 6 7 8]))
(println)

(println "my-filter: >5")
(println "[1 3 5 7 9 11]")
(println "Result my-filter:" (my-filter #(> % 5) [1 3 5 7 9 11]))
(println "Result filter:   " (filter #(> % 5) [1 3 5 7 9 11]))
(println)
