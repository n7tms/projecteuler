;; Project Euler
;; Problem 041 - Pandigital Prime
;;
;; We shall say that an n-digit number is pandigital if it makes use of all 
;; the digits 1 to n exactly once. For example, 2143 is a 4-digit pandigital
;; and is also prime.
;;
;; What is the largest n-digit pandigital prime that exists?
;;

(ns projecteuler)

(defn to-digits [num]
  (map #(Character/getNumericValue %) (str num)))


(defn unique-digits? [digits]
  (let [unique (distinct digits)]
    (= (count unique) (count digits))))

(defn pandigital? [digits]
  "returns true if a seq of digits is pandigital-9"
  (and (nil? (some zero? digits))
       (= 9 (count digits))
       (unique-digits? digits)))

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


;(time (apply max (take 1000000 (primes))))
;(time (nth (primes) 10000000))

(def all-primes (take 200000000 (primes)))

(apply max
          (for [x (range 100000000 200000000)]
            (if (pandigital? (nth all-primes x))
              x)))



