;; Project Euler
;; Problem 50 - Consecutive prime sum
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


;; let x = iterate through the list of prime numbers
;;    let y = iterate through the list of prime numbers up to x
;;        

(def some-primes (take-while #(< % 1000000) (primes)))

(defn candidates [max-num]
  (take-while #(< % (/ max-num 2)) (primes)))


(defn part-stuff [size cans]
  (for [x (partition size 1 cans)] (apply + x)))

(defn cps []
  (last
   (sort-by last
            (partition 2 2
                       (flatten
                        (remove empty?
                                (for [sp some-primes]
                                  (let [cans (candidates sp)]
                                    (remove nil?
                                            (for [size (range (count cans))]
                                              (if (and
                                                   (> size 1)
                                                   (<= size (count cans)))
                                                (if (some #(= sp %) (part-stuff size cans))
                                                  (list sp size))
                                                ))))
                                  ))))))
  )


;(cps)




