(load-file "core.clj")

(refer 'core)

(require '[clojure.test :as test])

(test/deftest defpackage-test
  (test/testing "DNF" 
                ;; (x -> y) = ((-x) | y)
                (test/is (= (dnf (logic-impl (variable :x) (variable :y))) 
                            (logic-or (logic-not (variable :x)) (variable :y)))) 
                
                ;; (1 -> y) = y
                (test/is (= (dnf (logic-impl (constant 1) (variable :y))) 
                            (variable :y)))
                
                ;; (-((x -> y) | (-(y -> z)))) = ((x & (-y)) | (x & (-y) & z))
                (test/is (= (dnf (logic-not (logic-or
                                             (logic-impl (variable :x) (variable :y))
                                             (logic-not (logic-impl (variable :y) (variable :z))))))
                            (logic-or 
                             (logic-and (variable :x) (logic-not (variable :y))) 
                             (logic-and (variable :x) (logic-not (variable :y)) (variable :z)))))
                
                ;; (x | (y -> (y & x))) = (x | (-y) | (y & x))
                (test/is (= (dnf (logic-or (variable :x) (logic-impl (variable :y) (logic-and (variable :y) (variable :x)))))
                            (logic-or (variable :x) (logic-not (variable :y)) (logic-and (variable :y) (variable :x)))))
                
                ;;((x | (y -> (z -> (-x)))) -> z) = (((-x) & y & z & x) | z)
                (test/is (= (dnf (logic-impl (logic-or (variable :x) (logic-impl (variable :y) (logic-impl (variable :z) (logic-not (variable :x))))) (variable :z)))
                            (logic-or (logic-and (logic-not (variable :x)) (variable :y) (variable :z) (variable :x)) (variable :z))))) 
  
  (test/testing "Substitutions"
                ;; (x -> y) = [x=1] = y
                (test/is (= (dnf (substitution (logic-impl (variable :x) (variable :y)) (array-map :x 1)))
                            (variable :y)))
                
                ;; (x -> y) = [x=1, y=0] = 0
                (test/is (= (dnf (substitution (logic-impl (variable :x) (variable :y)) (array-map :x 1 :y 0)))
                            (constant 0)))
                
                ;; (-((x -> y) | (-(y -> z)))) = [x=1, y=0] = 1
                (test/is (= (dnf (substitution (logic-not (logic-or
                                                           (logic-impl (variable :x) (variable :y))
                                                           (logic-not (logic-impl (variable :y) (variable :z))))) (array-map :x 1 :y 0)))
                            (constant 1)))
                
                ;; (-((x -> y) | (-(y -> z)))) = [x=1, y=0, z=1] = 1
                (test/is (= (dnf (substitution (logic-not (logic-or
                                             (logic-impl (variable :x) (variable :y))
                                             (logic-not (logic-impl (variable :y) (variable :z))))) (array-map :x 1 :y 0 :z 1)))
                            (constant 1)))))

(test/run-tests)