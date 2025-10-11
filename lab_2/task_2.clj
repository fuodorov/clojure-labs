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
          (build-seq 0 0.0))]
    
    (fn [x]
      (if (<= x 0)
        0.0
        (let [idx (int (/ x step))]
          (nth integral-seq idx))))))

(println "Test 1: f(x) = x, F(x) ≈ x²/2")
(let [f (fn [x] x)
      F (integrate-lazy f)]
  (println "F(2) =" (F 2.0) ", ≈" (/ 4.0 2))
  (println "F(4) =" (F 4.0) ", ≈" (/ 16.0 2))
  (println "F(10) =" (F 10.0) ", ≈" (/ 100.0 2)))
(println)

(println "Test 2: f(x) = x^2, lazy sequence")
(let [f (fn [x] (* x x))
      F (integrate-lazy f)]
  (println "First F(100)...")
  (time (F 100.0))
  (println "Second F(100)...")
  (time (F 100.0))
  (println "First F(200)...")
  (time (F 200.0))
  (println "Second F(200)...")
  (time (F 200.0)))
(println)

(println "Test 3: f(x) = x^3, lazy sequence, various values")
(let [f (fn [x] (* x x x))
      F (integrate-lazy f)
      values [5.0 10.0 15.0 20.0 25.0 30.0 35.0 40.0 45.0 50.0]]
  (doseq [x values]
    (print (format "F(%.1f): " x))
    (time  (F x))))
(println)
