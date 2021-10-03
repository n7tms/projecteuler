(ns user
  (:require [clojure.string :as str]
            [clojure.math.numeric-tower :as mth])
)

;; Project Euler #7
;;
;; By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
;; 
;; What is the 10 001st prime number?


;; https://stackoverflow.com/questions/53806246/clojure-is-prime-prime-numbers
(defn divides? [m n]
  (zero? (rem m n)))

(defn prime? [n]
  (and (< 1 n) (not-any? #(divides? n %) (range 2 (mth/sqrt n)))))

(prime? 2)  ;; => true
(prime? 13) ;; => true
(prime? 54) ;; => false



(defn count-primes [upper]
  (count  (for [x (range 1 upper)
                :when (prime? x)]
            x)))

(count-primes 20)  ;; => 8


(for [x (range 1 40)
      :when (= 5 (count-primes x))]
  x)

;; This ran for over 20 minutes and did not yield a solution!
(for [x (range 1 120000)
      :when (= 10001 (count-primes x))]
  x)


;(iterate inc 0)
;; infinite list


;; generates an infinite list of primes.
(defn lazy-primes3 []
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

;; This took less than a second to yeild a prime.
(last  (take 10001 (lazy-primes3))) ;; => 104743

;; Prime solution from http://clj-me.cgrand.net/index.php?s=Primes
