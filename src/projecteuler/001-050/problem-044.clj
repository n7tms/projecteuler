;; Project Euler
;; Problem 44 - Pentagon Numbers
;;

(ns projecteuler)


(def pentagonal-numbers
  "lazy-sequence of pentagonal numbers"
  (map (fn [x] (/ (* x (- (* 3 x) 1)) 2)) (iterate inc 1)))


(defn abs [n] (max n (- n)))

;; (defn pentagonal? [value]
;;   (< 0 (count (filter #(= value %) pone))) )


;; my pentagonal? function (above) seemed to be extremely inefficient.
;; I implemented this one found at https://roboloco.net/project-euler/problem-44
;; and things really took off!
(defn pentagonal? [value]
  (zero? (rem (+ 1 (Math/sqrt (+ 1 (* 24 value)))) 6)))


(defn problem-044 []
  (first (for [a pentagonal-numbers
               b (take-while #(> a %) pentagonal-numbers)
               :when (pentagonal? (+ a b))
               :when (pentagonal? (abs (- a b)))]
           (abs (- a b)))))

(problem-044)   ;; => 5482660



