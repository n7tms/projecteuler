;; Project Euler
;; Problem 058 (B) - 

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


(def alotta-primes (take 5000000 (primes)))
(defn prime? [n]
  (some #(= n %) alotta-primes)
  )

(time (prime? 32452843))
(last alotta-primes)

;; (defn prime? [n]
;;   (not-any? zero? (map #(rem n %) (range 2 n))))



(not-any? zero? (map #(rem 101 %) (range 2 101)))

(defn prime-corners [ring]
  "give a certain 'ring' in the spiral, return the corners that are prime"
  (let [side-length (+ (* ring 2) 1)
        corner-sqrd (* side-length side-length)]
    (remove nil?
            (for [i (range 1 4)]
              (let [x (- corner-sqrd (* (dec side-length) i))]
                (if (prime? x) x)))))
)

(defn calc-percentage [ring num-primes]
  (float (/ num-primes (+ (* ring 4) 1)))
;  (float (/ (count (filter #(prime? %) (flatten (corners (+ (* 2 sides) 1))))) (+ (* sides 4) 1)))
)



(defn problem-058 []
  (loop [ring 3
         all-prime-corners '(3 5 7 13 17)
         corners 9]
    (if (> ring 1000) ring
        (let [new-prime-corners (flatten (cons all-prime-corners (prime-corners ring)))]
          (if (< (* (count new-prime-corners) 10) (+ (* ring 4) 1))
            (+ (* ring 2) 1)
            (recur (inc ring) new-prime-corners (+ corners 4))))))
)


;(time (problem-058))




;; Resources
;; https://steloflute.tistory.com/entry/Project-Euler-Problem-58
