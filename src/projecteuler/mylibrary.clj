(ns projecteuler.core)

;; for information on how to create/use a custom library, see
;; https://www.braveclojure.com/organization/


;; generates an infinite list of primes.
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



;; This fibonacci implementation is more efficient -- only one recursion loop.
(defn fib [a b cnt]
  (if (zero? cnt)
    b
    (recur (+ a b) a (dec cnt)))) ;
;; usage: (map (partial fib 1 0) (range 10))  ; => (0 1 1 2 3 5 8 13 21 34)


(defn fibs []
  ((fn fib [a b]
     (lazy-seq (cons a (fib b (+ a b)))))
   0 1))
;; usage:
;; (fibs) => returns an infinite sequence (list) of fibonacci numbers, from 0 to infinite
;; (take 10 (fibs)) => the first 10 fibonacci numbers (0 1 1 2 3 5 8 13 21 34)
;; (last (take 10 (fibs))) => find the 10'th fibonacci number => 34



(defn factorial [n]
  "returns the factorial of the given positive integer argument"
  (if (= n 0) 1
      (loop [val n i n]
        (if (<= i 1) val
            (recur (*' val (dec i)) (dec i))))))
;;
;;
;;


(defn pascal-next-row [x]
  "returns the next row in a Pascal triangle sequence"
  (let [size (count x)]
    (for [i (range (inc size))]
      (if (or (= 0 i) (= size i)) 1
          (+ (nth x (dec i)) (nth x i))))))
;; usage:
;; (pascal-next-row [1 2 1]) => (1 3 3 1)
;; (take 5 (iterate pascal-next-row [1])) returns the first 5 Pascal triangle rows.
;;


(defn power
  "raise \"base\" to the power of \"exp\""
  [base exp]
  (reduce * (repeat exp base))
  )


