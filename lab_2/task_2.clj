;; Сравнить каждый вызов 
(defn integrate-lazy
  [f]
  (let [step 0.001
        
        integral-seq
        (letfn [(build-seq [i prev-integral]
                  (lazy-seq
                    (if (zero? i)
                      (cons 0.0 (build-seq 1 0.0))
                      (let [x0 (* (dec i) step)
                            x1 (* i step)
                            y0 (f x0)
                            y1 (f x1)
                            trapezoid-area (* step (/ (+ y0 y1) 2.0))
                            new-integral (+ prev-integral trapezoid-area)]
                        (cons new-integral (build-seq (inc i) new-integral))))))]
          (build-seq 0 0.0))
        
        cache (atom {})]
    
    (fn [x]
      (if (<= x 0)
        0.0
        (let [idx (int (/ x step))]
          (if (contains? @cache idx)
            (@cache idx)
            (let [result (nth integral-seq idx)]
              (swap! cache assoc idx result)
              result)))))))

(println "Test 1: f(x) = x, F(x) ≈ x²/2")
(let [f (fn [x] x)
      F (integrate-lazy f)]
  (println "F(2) =" (F 2.0) ", ≈" (/ 4.0 2))
  (println "F(4) =" (F 4.0) ", ≈" (/ 16.0 2))
  (println "F(10) =" (F 10.0) ", ≈" (/ 100.0 2)))
(println)

(println "Test 2: f(x) = x², F(x) ≈ x³/3")
(let [f (fn [x] (* x x))
      F (integrate-lazy f)]
  (println "F(2) =" (F 2.0) ", ≈" (/ 8.0 3))
  (println "F(3) =" (F 3.0) ", ≈" (/ 27.0 3))
  (println "F(5) =" (F 5.0) ", ≈" (/ 125.0 3)))
(println)

(println "Test 3: f(x) = 1, F(x) ≈ x")
(let [f (fn [x] 1.0)
      F (integrate-lazy f)]
  (println "F(5) =" (F 5.0) ", ≈ 5.0")
  (println "F(10) =" (F 10.0) ", ≈ 10.0"))
(println)

(println "Performance Test: multiple calls in non-decreasing order")
(let [f (fn [x] (* x x x x))
      F (integrate-lazy f)
      points [0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0]]
  (time
    (doseq [x points]
      (println (format "F(%.1f) = %.4f" x (F x))))))
(println)
