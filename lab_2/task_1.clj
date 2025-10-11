;; cсравнить с memoize
(defn integrate-memoized
  [f]
  (let [step 0.001

        compute-integral 
        (fn [x]
          (if (<= x 0)
            0.0
            (let [n (int (/ x step))]
              (loop [i 0
                     sum 0.0]
                (if (>= i n)
                  (* sum step)
                  (let [x0 (* i step)
                        x1 (* (inc i) step)
                        y0 (f x0)
                        y1 (f x1)]
                    (recur (inc i)
                           (+ sum (/ (+ y0 y1) 2.0)))))))))
        
        memo-cache (atom {})
        
        memoized-integral
        (fn [x]
          (if (contains? @memo-cache x)
            (@memo-cache x)
            (let [result (compute-integral x)]
              (swap! memo-cache assoc x result)
              result)))]
    
    memoized-integral))

(println "Test 1: f(x) = x, F(x) ≈ x²/2")
(let [f (fn [x] x)
      F (integrate-memoized f)]
  (println "F(2) =" (F 2.0) ", ≈" (/ 4.0 2))
  (println "F(4) =" (F 4.0) ", ≈" (/ 16.0 2))
  (println "F(10) =" (F 10.0) ", ≈" (/ 100.0 2)))
(println)

(println "Test 2: f(x) = x², F(x) ≈ x³/3")
(let [f (fn [x] (* x x))
      F (integrate-memoized f)]
  (println "F(2) =" (F 2.0) ", ≈" (/ 8.0 3))
  (println "F(3) =" (F 3.0) ", ≈" (/ 27.0 3))
  (println "F(5) =" (F 5.0) ", ≈" (/ 125.0 3)))
(println)

(println "Test 3: f(x) = 1, F(x) ≈ x")
(let [f (fn [x] 1.0)
      F (integrate-memoized f)]
  (println "F(5) =" (F 5.0) ", ≈ 5.0")
  (println "F(10) =" (F 10.0) ", ≈ 10.0"))
(println)

(println "Memoization:")
(let [f (fn [x] (* x x x))
      F (integrate-memoized f)]
  (println "First F(100)...")
  (time (F 100.0))
  (println "Second F(100)...")
  (time (F 100.0))
  (println "First F(200)...")
  (time (F 200.0))
  (println "Second F(200)...")
  (time (F 200.0)))
(println)

(println "Performance Test: multiple calls in non-decreasing order")
(let [f (fn [x] (* x x x x))
      F (integrate-memoized f)
      points [0.5 1.0 1.5 2.0 2.5 3.0 3.5 4.0]]
  (time
    (doseq [x points]
      (println (format "F(%.1f) = %.4f" x (F x))))))
(println)
