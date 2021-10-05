(ns projecteuler.core
  (:use clojure.math.combinatorics
        clojure.math.numeric-tower)
  )

;; The following iterative sequence is defined for the set of positive integers:
;; 
;; n → n/2 (n is even)
;; n → 3n + 1 (n is odd)
;; 
;; Using the rule above and starting with 13, we generate the following sequence:
;; 
;; 13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
;; It can be seen that this sequence (starting at 13 and finishing at 1) contains 
;; 10 terms. Although it has not been proved yet (Collatz Problem), it is thought 
;; that all starting numbers finish at 1.
;; 
;; Which starting number, under one million, produces the longest chain?
;; 
;; NOTE: Once the chain starts the terms are allowed to go above one million.
;; 

(defn collatz [x]
  (if (even? x) (/ x 2) (+ 1 (* 3 x))))

(defn do-rule [n]
  (if (= 1 n) 1 (conj [] (do-rule (collatz n)))))

(defn do-rule2 [n]
  (while (> n 1) (lazy-seq (cons n (do-rule2 (collatz n))))))

(do-rule2 13)

;; for i = 1 to 1000000
;;   


