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


;; This is the actual Collatz calculator
;; If the provided number is even, divide it by two
;; If the provided number is odd, multiply by 3 and add 1
(defn collatz [x]
  (if (even? x)
    (/ x 2)
    (+ 1 (* 3 x))))


;; This function creates the Collatz chain -- the list of digits 
;; ie. given 13, => (13 40 20 10 5 16 8 4 2 1)
(defn produce-collatz-chain [n]
  (cond
    (= 1 n) 1
    :else (do
            (list n (produce-collatz-chain (collatz n) ) ))))

;; This reduces the chain to a vector that contains the number of digits in the chain
;; and the given number.
;; ie. given 13, => [10 13]
(defn pair-up [n]
  (vector (count (flatten (produce-collatz-chain n))) n))


;; Here is where all the work occurs.
;; Create a pair of [collatz-count given] for all numbers between 1 and 1000000, inclusive.
;; Sort the pairs so the longest chain is at the beginning.
;; Take the first pair -- representing the longest chain.
;; Take the last number in that pair -- that is the given number.
;; Display it.
(defn problem-14 []
  (println (last (first (sort (comp - compare) (map pair-up (range 1 1000001)))))))

;; Note: As written, it takes about 3 minutes 40 seconds to produce the answer.
(problem-14)  ;; => 837799

;; The given number, 837799, produces the longest chain of 525 Collatz numbers.


