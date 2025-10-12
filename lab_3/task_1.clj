(defn split-into-blocks
  [coll block-size]
  (when (seq coll)
    (cons (take block-size coll)
          (lazy-seq (split-into-blocks (drop block-size coll) block-size)))))


(defn parallel-filter
  [pred coll block-size]
  (let [blocks (doall (split-into-blocks coll block-size))

        futures (doall (map (fn [block]
                              (future
                                (doall (filter pred block))))
                            blocks))

        results (map deref futures)]

    (apply concat results)))

(println "Test 1: Correctness check")
(let [data (range 20)
      result-simple (filter even? data)
      result-parallel (parallel-filter even? data 5)]
  (println "Input data:" (vec data))
  (println "Simple filter:" (vec result-simple))
  (println "Parallel filter:" (vec result-parallel))
  (println "Results match:" (= (vec result-simple) (vec result-parallel))))
(println)

(println "Test 2: Block splitting demonstration")
(let [data (range 15)]
  (println "Input data:" (vec data))
  (println "Blocks of 4 elements:")
  (doseq [block (split-into-blocks data 4)]
    (println "  " (vec block))))
(println)

(println "Test 3: Performance (light predicate)")
(let [data (range 10000)
      block-sizes [1 2 3 4 5 10 20 25 50 100]]
  (print "Simple filter: ")
  (time (doall (filter even? data)))
    (doseq [bs block-sizes]
        (print (format "Parallel filter (blocks of %d): " bs))
        (time (doall (parallel-filter even? data bs)))))

(println)

(println "Test 4: Performance (prime number filtering")
(defn is-prime? [n]
  (if (< n 2)
    false
    (not-any? #(zero? (mod n %)) (range 2 (inc (int (Math/sqrt n)))))))

(let [data (range 1 10000)
      block-sizes [1 2 3 4 5 10 20 25 50 100]]
  (print "Simple filter: ")
  (let [result (time (doall (filter is-prime? data)))]
    (println "Found:" (count result) "prime numbers"))

    (doseq [bs block-sizes]
      (print (format "Parallel filter (blocks of %d): " bs))
      (let [result (time (doall (parallel-filter is-prime? data bs)))]
        (println "Found:" (count result) "prime numbers"))))
(println)