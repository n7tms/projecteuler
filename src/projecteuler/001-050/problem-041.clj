;; Project Euler
;; Problem 041 - Pandigital Prime
;;
;; We shall say that an n-digit number is pandigital if it makes use of all 
;; the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital
;; and is also prime.
;;
;; What is the largest n-digit pandigital prime that exists?
;;

(ns projecteuler
  (:require [clojure.math.combinatorics :as combo]))


(defn prime? [n]
  (not-any? zero? (map #(rem n %) (range 2 n))))


(def perms (combo/permutations "7654321"))
;; Interestingly, permutations greater than 7654321 (ie. 87654321 and 987654321) did not yield any
;; prime numbers.

;; This is a brute force solution. 
;; Make a list of all of the permutations...
;; find the prime ones...
;; get the maximum one.
(defn problem-041 []
  (apply max
         (remove nil?
                 (for [x perms]
                   (let [y (Integer/parseInt (apply str x))]
                     (if (prime? y)
                       y))))))

(problem-041)   ;; => 7652413

