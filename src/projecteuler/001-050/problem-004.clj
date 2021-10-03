(ns projecteuler.core
  (:gen-class)
  (:require [clojure.string :as str])
)

;;Project Euler #4
;A palindromic number reads the same both ways. 
;The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
;
;Find the largest palindrome made from the product of two 3-digit numbers.


;; When you convert a number to a string and reverse it, it produces a list of characters 
;; (reverse (str 54))   => (/4 /5)
;;
;; palindrome? function reverses the number twice to get it into the character format,
;; and then compares it to the number reversed just once. 
;; If they are the same, then number is a palindrome.
;; (= (/4 /5) (/5 /4))  => false (not a palindrome) 
(defn palindrome? [num]
  (= (reverse (reverse (str num))) (reverse (str num))) 
)

;; testing the palindrome? function
(palindrome? 3)     ;; => true
(palindrome? 1001)  ;; => true
(palindrome? 3411)  ;; => false


;; first test the range for 2-digit numbers
(apply max (let [anum (range 10 100)]   ;; range is first (inclusive) to second (EXCLUSIVE)
        (for [a anum
              b anum
              :when (palindrome? (* a b))]
          (* a b)))) ;; => 9009  (Given in problem statement)


;; Same as above, but range is 3-digit numbers.
(apply max (let [anum (range 100 1000)]
        (for [a anum
              b anum
              :when (palindrome? (* a b))]
          (* a b)))) ;; => 906609



