;; Project Euler
;; Problem 51 - Prime Digit Replacement
;;

(ns projecteuler)

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


(defn power
  "raise 'base' to the power of 'exp'"
  [base exp]
  (reduce * (repeat (bigint exp) (bigint base)))
  )

(defn n-digit-masks
  "Returns all n-digit binary numbers as strings."
  [n]
  (map #(rest (Integer/toBinaryString (bit-or (bit-shift-left 1 n) %)))
       (range (power 2 n))))

(defn as-int [coll] (Integer/parseInt (apply str coll)))


(defn prime? [n]
  (not-any? zero? (map #(rem n %) (range 2 n))))

(defn apply-mask
  "Replaces digits of p with value n according to nonzero locations in mask m."
  [m p n]
  (map #(if (= \0 %1) %2 n) m p))

(defn euler-51 [family]
  (first
   (for [p (map str (primes))
         d [(count p)]
         m (rest (butlast (n-digit-masks d)))
         f [(filter #(prime? (as-int %))
                    (map #(apply-mask m p %) "0123456789"))]
         g [(filter #(not (= \0 (first %))) f)]
         :when (<= family (count g))]
     (map #(apply str %) g))))

(time (euler-51 8))   ;; => ("121313" "222323" "323333" "424343" "525353" "626363" "828383" "929393")

;; Solution in 3.5 hours.


