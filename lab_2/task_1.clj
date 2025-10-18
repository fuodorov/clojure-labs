(defn integrate-memoized
  [f]
  (let [step 0.001
        cache (atom {})

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
        
        memoized-integral
        (fn [x]
          (if (contains? @cache x)
            (do
              ;; (println "  [cache hit for x =" x "]")
              (@cache x))
            (do
              ;; (println "  [computing for x =" x "]")
              (let [result (compute-integral x)]
                (swap! cache assoc x result)
                result))))]
    
    memoized-integral))

(defn integrate-with-memoize
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
                           (+ sum (/ (+ y0 y1) 2.0)))))))))]
    
    (memoize compute-integral)))


(println "Test 1: f(x) = x, F(x) ≈ x²/2")
(let [f (fn [x] x)
      F (integrate-memoized f)]
  (println "F(2) =" (F 2.0) ", ≈" (/ 4.0 2))
  (println "F(4) =" (F 4.0) ", ≈" (/ 16.0 2))
  (println "F(10) =" (F 10.0) ", ≈" (/ 100.0 2)))
(println)

(println "Test 2: f(x) = x^2, custom memoized")
(let [f (fn [x] (* x x))
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

(println "Test 3: f(x) = x^2, built-in memoize")
(let [f (fn [x] (* x x))
      F (integrate-with-memoize f)]
  (println "First F(100)...")
  (time (F 100.0))
  (println "Second F(100)")
  (time (F 100.0))
  (println "First F(200)...")
  (time (F 200.0))
  (println "Second F(200)...")
  (time (F 200.0)))
(println)

(println "Test 3: f(x) = x^3, built-in memoize, various values")
(let [f (fn [x] (* x x x))
      F (integrate-with-memoize f)
      values [0.0 1.0 2.0 3.0 4.0 5.0 6.0 7.0 8.0 9.0]]
  (doseq [x values]
    (print (format "F(%.1f): " x))
    (time  (F x))))
(println)

