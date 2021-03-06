(ns projecteuler.core)

;; Project Euler
;; Problem 12

;; =============== Problem Statement =======================
;;The sequence of triangle numbers is generated by adding the natural numbers. 
;;So the 7th triangle number would be 1 + 2 + 3 + 4 + 5 + 6 + 7 = 28. The first ten terms would be:
;;
;;1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...
;;
;;Let us list the factors of the first seven triangle numbers:
;;
;;  1: 1
;;  3: 1,3
;;  6: 1,2,3,6
;; 10: 1,2,5,10
;; 15: 1,3,5,15
;; 21: 1,3,7,21
;; 28: 1,2,4,7,14,28
;; We can see that 28 is the first triangle number to have over five divisors.
;;
;;What is the value of the first triangle number to have over five hundred divisors?
;; ===========================================================

;; sum of a variable range  => (apply + (range x))
;(apply + (range 1 5))  ;; => (+ 1 2 3 4) => 10

;; a function to find the factors of a number
;;    Loop from 1 to sqrt(x), call it i
;;    If x % i == 0, then add i to the list of factors
;;
(defn factors [x]
  (remove nil? (cons x
                    (for [num (range 1 (+ 1 (/ x 2)))]
                      (when (zero? (mod x num)) num)
                      )))
)

;(factors 6)  ;; => (6 1 2 3)
;(count  (factors 8))  ;; => 4
;(count (factors 28))  ;; => 6


;; Generate a Triangle number
(defn triangle [x]
;  (reduce + (range 1 (inc x)))
  (* x (/ (+ x 1) 2))
  )
;(triangle 7)

(def triangle-seq (map triangle (iterate inc 1)))


(defn lets-do-this []
  (println (first (drop-while #(< (count (factors %)) 500) triangle-seq))))

(lets-do-this) ;; => 76576500
;; I finally got an answer. I don't know how long the program ran; I left it running
;; over night and there was an answer the next morning. How ever long it was, it was
;; too long!
;; Consider some optimized solutions  vvv

; http://mishadoff.com/blog/clojure-euler-problem-012/
; https://www.proctor-it.com/project-euler-in-clojure-problem-12/
; https://www.mathblog.dk/triangle-number-with-more-than-500-divisors/




;; (rest (range x)) skips the 0
(factors (apply + (rest (range 8))))


;(let [rng (range)]  (while (> 10            (->> rng                 (take 5)                 rest                 (apply +)                 factors                 count))))

(defn test [x]
  (->> (rest (range))
       (take x)
       rest       ;; ignore the 0
       (apply +)  ;; add them all up
       factors    ;; find the factors of the sum
       count      ;; return the number of factors
       ))

(test 10)

(defn test2 [x]    ;; receives a list of natural numbers [1 2 3 4 ...]
  (->> x
       (apply +)
       factors
       count)
  )

(factors (apply + [1 2 3 4 5]))  ;; => (15 1 3 5)
(test2 [1 2 3 4 5])   ;; => 4

;; How can I use this to "take" until it gets to 500? and return the triangle that produced the 500 factors?
;; This next line give us the triangle number
;; the sequence on the end can be a (range).
;; The 6 in the middle is the iteration. It needs to be generalized to a x.
(apply + (range (+ 1 (last  (take-while (partial > 6) [1 2 3 4 5 6 8 10 12 13])))))






;; I need a way to loop through all of the natural numbers until I find a triangle with more than 500 factors.

;(defn my-test []   (into ()        (loop [x 1]          (when (< x 10)            x            (recur (+ x 1))))))

;(my-test)


(loop [x 1]
  (when (< x 10)
    x
    (recur (+ x 1))))
