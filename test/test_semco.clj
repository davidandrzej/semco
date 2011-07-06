(ns test-semco
  (:use [clojure.contrib.math :only (abs)])
  (:use [clojure.set :only (index)])
  (:use [clojure.test :only (testing deftest is are use-fixtures)])
  (:use semco))

(def t1words '("a" "b" "c"))
(def t2words '("x" "y" "z"))
(def topics [(make-topic 0 t1words)
             (make-topic 1 t2words)])
(def docs ['("a" "a" "a")
           '("b" "c" "x" "z")
           '("x" "x" "y" "z" "a")])

(def tol 1e-6)
(defn approx-equal? [x y] (< (abs (- x y)) tol))

(deftest test-counts
  (testing "Count functionality"
    (let [[t1 t2] (get-counts topics docs)]
      (is (= (:counts t1) {"a" 2 "b" 1 "c" 1}))
      (is (= (:cocounts t1)
             {#{"b" "c"} 1}))
      (is (= (:counts t2) {"x" 2 "y" 1 "z" 2}))
      (is (= (:cocounts t2)
             {#{"x" "y"} 1
              #{"y" "z"} 1
              #{"x" "z"} 2})))))

(deftest test-nested
  (testing "Nested word pairs"
    (is (= (topic-words (first topics))
           [["a" "c"] ["a" "b"] ["b" "c"]]))))

;;
;; Double-checked in Python
;;
;; >>> math.log(2) + math.log(1. / 2) + math.log(1. / 2 )
;; -0.69314718055994529
;; >>> math.log(2. / 2) + math.log(3. / 2) + math.log(2. / 1)
;; 1.0986122886681096
;;
(deftest test-score
  (testing "Score calculation"
    (let [[t1 t2] (get-counts topics docs)]
      (is (approx-equal? -0.69314718 (score-topic t1)))
      (is (approx-equal? 1.09861228 (score-topic t2))))))

(deftest test-full
  (testing "All-in-one function"
    (let [results (index (semantic-coherence [t1words t2words] docs)
                         [:topic])]
      (is (approx-equal? -0.69314718                         
                         (:semco (first (results {:topic 0})))))
      (is (approx-equal? 1.09861228
                         (:semco (first (results {:topic 1}))))))))
    
