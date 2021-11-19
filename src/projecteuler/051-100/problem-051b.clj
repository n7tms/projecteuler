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



(defn prime? [n]
  (not-any? zero? (map #(rem n %) (range 2 n))))

( defn n-digit-primes [d]
  (->> (primes)
       (drop-while #(< % (power 10 (dec d))))
       (take-while #(< % (power 10 d)))
       (map str)))

(defn all-unmasked-same? [m n]
  (let [z (map #(if (= \0 %1) %2 \x) m n)]
    (= 1 (count (distinct (filter #(not (= \x %)) z))))))

(defn mask-off [m a]
  (apply str (map #(if (= \0 %1) \x %2) m a)))

(defn euler-51-alternate [family]
;  (first)
  (for [d (iterate inc 2)
        ps [(n-digit-primes d)]
        m (reverse (rest (butlast (n-digit-masks d))))
        f [(filter #(all-unmasked-same? m %) ps)]
        g [(frequencies (map #(mask-off m %) f))]
        k (sort (keys g))
        :when (<= family (g k))]
    k))

(euler-51-alternate 8)
