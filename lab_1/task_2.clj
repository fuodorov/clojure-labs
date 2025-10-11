(defn generate-strings-tail
  [symbols n]
  (letfn [
    (build [current-string remaining-length accumulator-results]
      (if (zero? remaining-length)
        (cons current-string accumulator-results)
        (add-symbols symbols current-string remaining-length accumulator-results)))
    
    (add-symbols [symbols current-string remaining-length accumulator-results]
      (if (empty? symbols)
        accumulator-results
        (let [symbol (first symbols)
              last-char (get-last-char current-string)]
          (if (= symbol last-char)
            (recur (rest symbols) current-string remaining-length accumulator-results)
            (recur (rest symbols) 
                   current-string 
                   remaining-length 
                   (build (str current-string symbol) (dec remaining-length) accumulator-results))))))
    
    (get-last-char [s]
      (if (empty? s)
        nil
        (subs s (dec (count s)))))]
    
    (build "" n '())))

(println "Test 1: symbols=[a b c], n=2")
(println (generate-strings-tail ["a" "b" "c"] 2))
(println)

(println "Test 2: symbols=[a b c], n=3")
(println (generate-strings-tail ["a" "b" "c"] 3))
(println)

(println "Test 3: symbols=[a b], n=4")
(println (generate-strings-tail ["a" "b"] 4))