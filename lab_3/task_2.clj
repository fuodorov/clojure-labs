(defn split-into-blocks
  [coll block-size]
  (when (seq coll)
    (cons (take block-size coll)
          (lazy-seq (split-into-blocks (drop block-size coll) block-size)))))

(defn lazy-parallel-filter
  [pred block-size coll]
  (let [blocks (split-into-blocks coll block-size)]
    (mapcat
      (fn [block]
        (let [future-result (future (doall (filter pred block)))]
          (deref future-result)))
      blocks)))

(println "Test 1: Laziness check")
(let [infinite-seq (range)
      filtered (lazy-parallel-filter even? 10 infinite-seq)]
  (println "First 20 even numbers from infinite sequence:")
  (println (take 20 filtered)))
(println)

(println "Test 2: Correctness check")
(let [data (range 20)
      result-simple (filter even? data)
      result-lazy-parallel (lazy-parallel-filter even? 5 data)]
  (println "Input data:" (vec data))
  (println "Regular filter:" (vec result-simple))
  (println "Lazy parallel:" (vec result-lazy-parallel))
  (println "Results match:" (= (vec result-simple) (vec result-lazy-parallel))))
(println)

(println "Test 3: Block splitting demonstration")
(let [data (range 15)]
  (println "Input data:" (vec data))
  (println "Blocks of 4 elements:")
  (doseq [block (split-into-blocks data 4)]
    (println "  " (vec block))))
(println)

(println "Test 4: Performance (light predicate)")
(let [data (range 10000)
      block-sizes [1 2 3 4 5 10 20 25 50 100]]
  (print "Regular filter: ")
  (time (doall (filter even? data)))
  (doseq [bs block-sizes]
    (print (format "Lazy parallel (blocks of %d): " bs))
    (time (doall (lazy-parallel-filter even? bs data)))))
(println)

(println "Test 5: Performance (prime number filtering)")
(defn is-prime? [n]
  (if (< n 2)
    false
    (not-any? #(zero? (mod n %)) (range 2 (inc (int (Math/sqrt n)))))))

(let [data (range 1 10000)
      block-sizes [1 2 3 4 5 10 20 25 50 100]]
  (print "Regular filter: ")
  (let [result (time (doall (filter is-prime? data)))]
    (println "Found:" (count result) "prime numbers"))

  (doseq [bs block-sizes]
    (print (format "Lazy parallel (blocks of %d): " bs))
    (let [result (time (doall (lazy-parallel-filter is-prime? bs data)))]
      (println "Found:" (count result) "prime numbers"))))
(println)

