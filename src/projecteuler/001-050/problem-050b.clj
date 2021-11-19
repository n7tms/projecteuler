;; Project Euler
;; Problem 50 - Consecutive Prime Sum
;;
;; solution courtesy of https://roboloco.net/project-euler/problem-50/
;;
;; My solution (problem-050.clj) ran for 48 hours without reaching a resolution.
;; Obviously, some could have been optimized more.
;; primes and prime? are from my library. I've also added a lot of comments to 
;; the code as I sifted through it trying to understand what roboloco did.
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


;; Is a number a prime number
(defn prime? [n]
  (not-any? zero? (map #(rem n %) (range 2 n))))



;; This is better than euler-19 accum, which wasn't lazy
(defn lazy-accum [seq]
  "given a lazy sequence seq, returns a lazy sequence of the sums of elements up to each element in the sequence"
  (map first (iterate (fn [[sum s]]
                        [(+ sum (first s)) (next s)])
                      [(first seq) (rest seq)])))

;; Example: (lazy-accum (primes))  ;; => (2 5 10 17 28 ...)


(defn accum [s]
  "Returns a sequence of the sums of elements up to each element in seq s."
  (loop [e (first s)
         r (rest s)
         sums [0]]
    (if (nil? e)
      (rest sums)
      (recur (first r) (rest r) (conj sums (+ e (last sums)))))))

;; Example: (accum '(1 2 3 4))  ;; => (1 3 6 10)



(def prime-sums (conj (lazy-accum (primes)) 0))

;; Starts with a lazy-accum of prime numbers where the sum is less than 1,000,000.
;; If that sum is not prime, then subtract the prime from each end and check again.
(defn euler-50 [goal]
  (loop [c 1]
    (let [bots (reverse (take c prime-sums))
          tops (take c (reverse (take-while #(> goal (- % (last bots)))
                                            (rest prime-sums))))]
      (if-let [v (some #(if (prime? %) % nil)
                       (map #(- %1 %2) tops bots))]
        v
        (recur (inc c))))))

(euler-50 1000000)   ;; => 997651

