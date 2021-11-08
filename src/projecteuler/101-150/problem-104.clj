;; Project Euler
;; Problem 104 - Pandigital Fibonacci Ends
;;

(ns projecteuler)

(def fibs
  ((fn fib [a b]
     (lazy-seq (cons a (fib b (+ (bigint a) (bigint b))))))
   0 1))
;; usage:
;; (fibs) => returns an infinite sequence (list) of fibonacci numbers, from 0 to infinite
;; (take 10 (fibs)) => the first 10 fibonacci numbers (0 1 1 2 3 5 8 13 21 34)
;; (last (take 10 (fibs))) => find the 10'th fibonacci number => 34

(defn to-digits [num]
  (map #(Character/getNumericValue %) (str num)))

(defn unique-digits? [digits]
  (let [unique (distinct digits)]
    (= (count unique) (count digits))))

(defn pandigital? [digits]
  "returns true if a seq of digits is pandigital-9"
  (and (nil? (some zero? digits))
       (= 9 (count digits))
       (unique-digits? digits)))

(defn power
  "raise 'base' to the power of 'exp'"
  [base exp]
  (reduce * (repeat (bigint exp) (bigint base)))
  )



(defn problem-104 []
  (loop [idx 2749]
    (let [a (to-digits (nth (fibs) idx))]
      (if (and
           (pandigital? (take 9 a))
           (pandigital? (take-last 9 a)))
        idx
        (recur (inc idx))))))

;(problem-104)


;(nth fibs 329468)


;; My pandigital? function is expecting a sequence of digits (e.g. (1 2 3 4)).
;; This function is just sending an integer of digits (e.g. 1234).
(defn todd []

  (loop [n 2
         fn1 1
         fn2 1]
    (let [fn (+ fn1 fn2)
          tail (mod fn 1000000000)]
      (if (pandigital? tail)
        (let [digits (+ 1 (Math/log fn))]
          (if (> digits 9)
            (if (pandigital? (/ fn (power 10 (- digits 9))))
              n
              (recur (inc n) fn2 (inc fn2)))))
        )))


  )

(todd)
