;; Project Euler
;; Problem 60 - Prime Pair Sets
;;

;; The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and 
;; concatenating them in any order the result will always be prime. For example, taking 
;; 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents 
;; the lowest sum for a set of four primes with this property.
;;
;; Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.
;;

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

;(def numbers '(1 2 3 4 5))
;(def num4 '(3 7 109 673))

(defn prime-pair-set? [numbers]
  (not (some false?
             (map prime?
                  (map #(Integer/parseInt %)
                       (remove nil?
                               (for [a numbers
                                     b numbers]
                                 (if (not= a b)
                                   (str a b)))))))))


(defn problem-60 []
  (remove nil?
          (for [a (take 200 (primes))
                b (take 200 (primes))
                c (take 200 (primes))
                d (take 200 (primes))]
            (if (= 4 (count (distinct (list a b c d))))
              (if (prime-pair-set? (list a b c d))
                (apply + (list a b c d))))
            ))
  )


(defn problem-60b [max]
  (loop [a 0 b 0 c 0 d 0]
    (if (> 4 (count (distinct (list a b c d))))
      (cond
        (= a max) :nothing
        (= b max) (recur (inc a) 0 c d)
        (= c max) (recur a (inc b) 0 d)
        (= d max) (recur a b (inc c) 0)
        :else (recur a b c (inc d)))

      (let [w (nth (primes) a)
            x (nth (primes) b)
            y (nth (primes) c)
            z (nth (primes) d)]
        (if (prime-pair-set? (list w x y z))
          (list w x y z)
          (cond
            (= a max) :nothing
            (= b max) (recur (inc a) 0 c d)
            (= c max) (recur a (inc b) 0 d)
            (= d max) (recur a b (inc c) 0)
            :else (recur a b c (inc d))))))))

;(problem-60b 200)


;; I tried a couple of different approaches (above). Both ran for about an hour and 
;; neither produced a solution. These were meant to just find 4 primes, not the 
;; requested 5.
;; So I turned to RoboLoco....
;; =========================================================================

(defn expt
  "raise 'base' to the power of 'exp'"
  [base exp]
  (reduce * (repeat (bigint exp) (bigint base)))
  )

(defn x10 [n] (expt 10 (inc (quot (Math/log n) (Math/log 10)))))

(defn all-pairs-prime?
  "Returns true iff p forms only prime numbers when concatenated in any order
  with an element of coll."
  [coll p]
  (every? prime? (interleave (map #(+ % (* p (x10 %))) coll)
                             (map #(+ p (* % (x10 p))) coll))))

(defn euler-60 []
  (let [ps (take-while #(< % 10000) (primes))]
    (first
     (for [a ps
           b (filter #(all-pairs-prime? [a] %) (drop-while #(<= % a) ps))
           c (filter #(all-pairs-prime? [a b] %) (drop-while #(<= % b) ps))
           d (filter #(all-pairs-prime? [a b c] %) (drop-while #(<= % c) ps))
           e (filter #(all-pairs-prime? [a b c d] %) (drop-while #(<= % d) ps))]
       (+ a b c d e)))))

(defn euler-60-4 []
  (let [ps (take-while #(< % 1000) (primes))]
    (first
     (for [a ps
           b (filter #(all-pairs-prime? [a] %) (drop-while #(<= % a) ps))
           c (filter #(all-pairs-prime? [a b] %) (drop-while #(<= % b) ps))
           d (filter #(all-pairs-prime? [a b c] %) (drop-while #(<= % c) ps))]
       (+ a b c d )))))

;(time (euler-60-4))  ;; => 792

(time (euler-60))







