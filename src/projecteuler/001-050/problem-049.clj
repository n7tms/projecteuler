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


(defn prime? [n]
  (not-any? zero? (map #(rem n %) (range 2 n))))


(defn to-digits [num]
  (map #(Character/getNumericValue %) (str num)))


(defn problem-049 []
  (for [n (filter #(> % 999) (take-while #(< % 10000) (primes))) ]
    (let [x (filter #(> % n) (map #(Integer/parseInt %) (map #(apply str %) (sort (combo/permutations (to-digits n))))))]
      (if (> (count x) 1)
        (let [y1 (nth x 0)
              y2 (nth x 1)]
          (if (= (- y1 n) (- y2 y1))
            (if (and (prime? y1) (prime? y2))
              (str n y1 y2)))))
      )
    ))


(def testing (filter #(> % 3000) (map #(Integer/parseInt %) (map #(apply str %) (sort (combo/permutations (to-digits 1487)))))))

(nth testing 1)
;(problem-049)



