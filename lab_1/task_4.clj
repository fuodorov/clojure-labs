(defn generate-strings-functional
  [symbols n]
  (letfn [
    (get-last-char [s]
      (if (empty? s)
        nil
        (subs s (dec (count s)))))
    
    (append-all-symbols [s]
      (map #(str s %)
         (filter #(not= % (get-last-char s)) symbols)))
    
    (next-level [strings]
      (mapcat append-all-symbols strings))]

    (nth (iterate next-level [""]) n)))

(println "Test 1: symbols=[a b c], n=2")
(println (generate-strings-functional ["a" "b" "c"] 2))
(println)

(println "Test 2: symbols=[a b c], n=3")
(println (generate-strings-functional ["a" "b" "c"] 3))
(println)

(println "Test 3: symbols=[a b], n=4")
(println (generate-strings-functional ["a" "b"] 4))
(println)
