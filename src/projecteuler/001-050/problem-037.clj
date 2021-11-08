;; Project Euler
;; Problem 37 - Truncatable Primes
;;
;; The bulk of the solution is from https://roboloco.net/project-euler/problem-37/
;;

(ns projecteuler)


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

;; (defn prime? [n]
;;   (not-any? zero? (map #(rem n %) (range 2 n))))


(defn prime? [n]
  (if (> 2 n)
    false
    (not-any? #(zero? (rem n %)) (take-while #(<= (* % %) n) (primes)))))

(defn as-digits [num]
  (map #(Character/getNumericValue %) (str num)))

(defn as-int [coll]
  (Integer/parseInt (apply str coll)))

(defn lr-truncatable-prime? [n]
  (let [trunc-prime? (fn [f n]
                (cond
                 (< n 10) (prime? n)
                 (prime? n) (recur f (as-int (f (as-digits n))))
                 true false))]
    (and (trunc-prime? butlast n)
         (trunc-prime? rest n))))

(defn problem-037 []
  (reduce + (take 11 (filter lr-truncatable-prime? (iterate inc 10)))))

(problem-037)  ;; => 748317



