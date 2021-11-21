;; Project Euler
;; Problem 57 - Square Root Convergents
;;
;; It is possible to show that the square root of two can be expressed as an infinite continued fraction.
;; By expanding this for the first four iterations, we get:
;; 3/2 = 1.5
;; 7/5 = 1.4
;; 17/12 = 1.41666...
;; 41/29 = 1.41379...
;;
;; The next three expansions are 99/70, 239/169, and 577/408, but the eighth expansion, 1393/985
;; is the first example where the number of digits in the numerator exceeds the number of digits
;; in the denominator.
;;
;; In the first one-thousand expansions, how many fractions contain a numerator with more digits 
;; than the denominator?

(ns projecteuler)

;; The square-root of 2 expansion follows the an obvious, easily replicatable pattern:
;;    given a/b, the next term is (a+2b)/(a+b)
(def sqrt2-expansion
  (iterate #(/ (+ (numerator %) (* 2 (denominator %)))
               (+ (numerator %)      (denominator %)))
           (/ 3 2)))

;; Does the numerator have more digits than the denominator?
;; I searched for a numerical way to do this. It would probably be more efficient.
;; However, converting this to a string and counting the "characters" is easier.
(defn longer-numerator? [n]
  (> (count (str (numerator n)))
     (count (str (denominator n)))))


;; The sqrt2-expansion returns all of the expansions, but we only care about the ones
;; where the numerator is longer. We'll filter those out and return the count.
(defn problem-057 []
  (count (filter longer-numerator? (take 1000 sqrt2-expansion))))


(problem-057)   ;; => 153

