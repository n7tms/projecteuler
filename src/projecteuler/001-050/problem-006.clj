(ns user
  (:gen-class)
  (:require [clojure.string :as str])
)

;; Project Euler #6
;; The sum of the squares of the first ten natural numbers is,
;; 1^2 + 2^2 + 3^2 + ... + 10^2 = 385
;;
;; The square of the sum of the first ten natural numbers is,
;; (1 + 2 + 3 + ... + 10)^2 = 55^2 = 3025
;;
;; Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is .
;; 3025 - 385 = 2640
;;
;; Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
;;

(defn sqr [x]
  (* x x)
)

;; Using a range of numbers from 1 to (one less than) the upper bound provided,
;; return a sum of each number squared.
(defn sum-of-squares [upper]
  (apply + (let [x (range 1 upper)]
             (for [a x] (sqr a))
             )))

;; Using a range of numbers from 1 to (one less than) the upper bound provided,
;; sum all the numbers and return the square of the sum.
(defn square-of-sums [upper]
  (let [x (apply + (range 1 upper))]
    (sqr x)))

;; Testing the given input.
(sum-of-squares 11) ;; => 385
(square-of-sums 11) ;; => 3025

;; The answer: the difference between .... with the first 100 natural numbers, 1-100.
(- (square-of-sums 101) (sum-of-squares 101) ) ;; => 25164150
