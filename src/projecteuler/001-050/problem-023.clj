;; Project Euler
;; Problem 23: Non-abundant sums
;; 

;; A perfect number is a number for which the sum of its proper divisors is exactly 
;; equal to the number. For example, the sum of the proper divisors of 28 would be 
;; 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
;; 
;; A number n is called deficient if the sum of its proper divisors is less than n 
;; and it is called abundant if this sum exceeds n.

;; As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number 
;; that can be written as the sum of two abundant numbers is 24. By mathematical 
;; analysis, it can be shown that all integers greater than 28123 can be written as 
;; the sum of two abundant numbers. However, this upper limit cannot be reduced any 
;; further by analysis even though it is known that the greatest number that cannot 
;; be expressed as the sum of two abundant numbers is less than this limit.
;; 
;; Find the sum of all the positive integers which cannot be written as the sum of 
;; two abundant numbers.
;; 

(ns projecteuler)

;; produce a list of divisors of n, not including n
;; e.g. (divisors 10) => (1 2 5)
(defn divisors
  ""
  [n]
  (filter (comp zero? (partial rem n)) (range 1 n)))



(defn abundant? [num]
  "return true if the num is an abundant number (sum of the divisors is greater than the number"
  (< num (reduce + (divisors num)))
)


(def abundants
  "generate a sequence of abundant numbers, 12-28123"
  (into (sorted-set) (filter abundant? (range 12 28124)))
)

;;(count abundants)   ;; => 6965

(->> (for [i abundants j abundants
           :let [num (+ i j)]
           :when (< num 28124)]
       num)
     (distinct)
     (reduce +)
     (- (reduce + (range 1 28124))) ;; subtract the sum of the abundants from the sum of all the numbers to get the sum of the numbers that are not abundant.
)  ;; => 4179871 (29 seconds)

(defn abundant-sum? [n abundant]
  (some #(abundant (- n %))
        (take-while #(< % n)  abundant)))



;; Optimized
(->> (range 1 28124)
     (remove #(abundant-sum? % abundants))
     (reduce +))   ;; => 4179871 (4 seconds)


;;
;; with a little help from http://mishadoff.com/blog/clojure-euler-problem-023/
;;

