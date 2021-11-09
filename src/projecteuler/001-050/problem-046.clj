;; Project Euler
;; Problem 46 - Goldbach's Other Conjecture
;;
(defn all-primes []
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



;; Is a number a prime number
(defn prime? [n]
  (not-any? zero? (map #(rem n %) (range 2 n))))

(def primes (take 1000 (all-primes)))

(defn goldbach? [x]
  (some true?
        (for [p (take-while #(< % x) primes)]
          (let [r (Math/sqrt (quot (- x p) 2))]
            (not (< (int r) r))
            )
          ))
  )

(defn problem-046 []
  (first
   (remove nil?
           (for [n (range 10001)]
             (if (and
                  (odd? n)
                  (not (prime? n))
                  (not (goldbach? n)))
               n
               ))))
  )

(problem-046)   ;; => 5777


