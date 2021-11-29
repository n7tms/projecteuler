

(ns projecteuler)

;; testing a few different prime number generators.

;; The one I use
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


(defn classic-sieve
  "Returns sequence of primes less than N"
  [n]
  (loop [nums (transient (vec (range n))) i 2]
    (cond
     (> (* i i) n) (remove nil? (nnext (persistent! nums)))
     (nums i) (recur (loop [nums nums j (* i i)]
                       (if (< j n)
                         (recur (assoc! nums j nil) (+ j i))
                         nums))
                     (inc i))
     :else (recur nums (inc i)))))

(defn primes3 [max]
  (let [enqueue (fn [sieve n factor]
                  (let [m (+ n (+ factor factor))]
                    (if (sieve m)
                      (recur sieve m factor)
                      (assoc sieve m factor))))
        next-sieve (fn [sieve candidate]
                     (if-let [factor (sieve candidate)]
                       (-> sieve
                         (dissoc candidate)
                         (enqueue candidate factor))
                       (enqueue sieve candidate candidate)))]
    (cons 2 (vals (reduce next-sieve {} (range 3 max 2))))))



;; This is the fastest one
;; https://thesoftwaresimpleton.com/blog/2015/02/07/primes
(defn primes-ss [n]
  (let [root (-> n (Math/sqrt) inc int)
        sieve (boolean-array n true)]
    (loop [i 2]
      (when (< i (Math/sqrt n))
        (when (aget sieve i)
          (loop [j (* i 2)]
            (when (< j n)
              (aset sieve j false)
              (recur (+ j i)))))
        (recur (inc i))))
    (filter #(aget sieve %) (range 2 n))))

;; (time (nth (primes) 1000000))   ;; time = 20624 msec

;; (time (nth (classic-sieve 15485868) 1000000))   ;; time = 7772 msec

;; (time (last (primes3 15485868)))   ;; time =  msec


(time (count (primes-ss 994858680)))  ;; => 1667 msec

(primes-ss 100)
