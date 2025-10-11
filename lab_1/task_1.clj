(defn generate-strings
  [symbols n]
  (letfn [
    (build [current-string remaining-length]
      (if (zero? remaining-length)
        (list current-string)
        (add-symbols symbols current-string remaining-length)))
    
    (add-symbols [symbols current-string remaining-length]
      (if (empty? symbols)
        nil
        (let [symbol (first symbols)
              last-char (get-last-char current-string)]
          (if (= symbol last-char)
            (add-symbols (rest symbols) current-string remaining-length)
            (concat (build (str current-string symbol) (dec remaining-length))
                    (add-symbols (rest symbols) current-string remaining-length))))))
    
    (get-last-char [s]
      (if (empty? s)
        nil
        (subs s (dec (count s)))))]
    
    (build "" n)))

(println "Test 1: symbols=[a b c], n=2")
(println (generate-strings ["a" "b" "c"] 2))
(println)

(println "Test 2: symbols=[a b c], n=3")
(println (generate-strings ["a" "b" "c"] 3))
(println)

(println "Test 3: symbols=[a b], n=4")
(println (generate-strings ["a" "b"] 4))
