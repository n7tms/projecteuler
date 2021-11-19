;; Project Euler
;; Problem 50 - Consecutive Prime Sum
;;
;; solution courtesy of https://roboloco.net/project-euler/problem-50/

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
(defn make-seq-accumulator [seq]
  (map first (iterate (fn [[sum s]]
                        [(+ sum (first s)) (next s)])
                      [(first seq) (rest seq)])))

(def prime-sums (conj (make-seq-accumulator (primes)) 0))

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

