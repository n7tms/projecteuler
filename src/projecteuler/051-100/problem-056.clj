;; Project Euler
;; Problem 56 - Powerful Digit Sum
;;

(ns projecteuler)

(defn power
  "raise 'base' to the power of 'exp'"
  [base exp]
  (reduce * (repeat (bigint exp) (bigint base)))
  )

(defn add-digits
  "given a 'string' of digits, add-digits returns the sum of the digits"
  [nums]
  (loop [acc 0 idx 0]
    (if (= idx (count nums))
      acc
      (recur (+ acc (Integer/parseInt (str (nth nums idx)))) (inc idx) )) 
    ))

(defn problem-056 []  
  (last
   (sort
    (for [a (range 100)
          b (range 100)]
      (add-digits (str (power a b)))))))

(problem-056)   ;; => 972
