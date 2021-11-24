;; Project Euler
;; Problem 001 - Multiples of 3 or 5
;;
;; If we list all the natural numbers below 10 that are multiples of 3 or 5, 
;; we get 3, 5, 6 and 9. The sum of these multiples is 23.
;;
;; Find the sum of all the multiples of 3 or 5 below 1000.
;;
(ns projecteuler)

(defn problem-001a []
  (reduce +
          (distinct
           (flatten
            (list
             (filter #(= 0 (mod % 3)) (range 1 1000))
             (filter #(= 0 (mod % 5)) (range 1 1000)))))))


(defn problem-001b []
  (reduce +
          (for [x (range 1 1000)]
            (if (or
                 (= 0 (mod x 3))
                 (= 0 (mod x 5)))
              x
              0))))

(time (problem-001a))  ;; => 233168 in 1.2 msec
(time (problem-001b))  ;; => 233168 in 0.5 msec
