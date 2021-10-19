;; Project Euler
;; Problem 25 - 1000-digit Fibonacci number
;; 
;; .
;; .
;; .
;; F12 = 144
;; The 12th term, F12, is the first term to contain three digits.
;;
;; What is the index of the first term in the Fibonacci sequence to contain 1000 digits?
;;

(ns projecteuler)

(defn fibs []
  ((fn fib [a b]
     (lazy-seq (cons a (fib b (+ (bigint a) (bigint b))))))
   0 1))
;; usage:
;; (fibs) => returns an infinite sequence (list) of fibonacci numbers, from 0 to infinite
;; (take 10 (fibs)) => the first 10 fibonacci numbers (0 1 1 2 3 5 8 13 21 34)
;; (last (take 10 (fibs))) => find the 10'th fibonacci number => 34


(take 10 (fibs))
(->> (fibs)
     (take 10))

(take (inc 12) (fibs))


;; This loop actually prints out the index (x) for each iteration.
;; The solution to problem 25 is 1 greater than the last index printed.
(loop [x 1]
  (when (> 1000 (count (str (nth (fibs) x))))
    (println  x)
    (recur (inc x)))
   )

;; => 4782
