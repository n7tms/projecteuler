;; Project Euler
;; Problem 49 - Prime Permutations
;;
;;

(ns projecteuler
  (:require [clojure.math.combinatorics :as combo]))

;; Let n = prime numbers between 1000 and 9999, inclusive
;; let x = sorted permutations of n that are greater than n
;; if x1 is prime and x2 is prime then
;;    if (- x1 n) = (- x1 x2) then save n

(defn primes []
  (letfn [(enqueue [sieve n step]
            (let [m (+ n step)]
              (if (sieve m)
                (recur sieve m step)
                (assoc sieve m step))))
          (next-sieve [sieve candidate]
            (if-let [step (sieve candidate)]
              (-> sieve
                (dissoc candidate)
                (enqueue candidate step))
              (enqueue sieve candidate (+ candidate candidate))))
          (next-primes [sieve candidate]
            (if (sieve candidate)
              (recur (next-sieve sieve candidate) (+ candidate 2))
              (cons candidate 
                (lazy-seq (next-primes (next-sieve sieve candidate) 
                            (+ candidate 2))))))]
    (cons 2 (lazy-seq (next-primes {} 3)))))



;; usage:
;; (primes) => returns an infinite list of prime numbers from 2 to infinite
;; (take 10 (primes)) => the first 10 prime numbers (2 3 5 7 11 13 17 19 23 29)
;; (last (take 10 (primes))) => find the 10'th prime number => 29


;; Prime solution from http://clj-me.cgrand.net/index.php?s=Primes


;; Is a number a prime number
(defn prime? [n]
  (not-any? zero? (map #(rem n %) (range 2 n))))

(defn problem-049 []
  (for [n (filter #(> % 999) (take-while #(< % 10000) (primes))) ]
    (let [x (filter #(> % n) (sort (combo/permutations n)))]
      (if (> (count x) 1)
        (if (= (- (nth x 0) n) (- (nth x 1) (nth x 0)))
          (if (and (prime? (nth x 0)) (prime? (nth x 1)))
            (str n (nth x 0) (nth x 1)))))
      )
    ))


(sort (combo/permutations '(1 2 3 4)))
