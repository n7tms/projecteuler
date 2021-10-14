;; Project Euler
;;
;; Problem 20: Factorial digit sum
;; https://projecteuler.net/problem=20
;;

;; n! means n × (n − 1) × ... × 3 × 2 × 1
;; 
;; For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
;; and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.
;; 
;; Find the sum of the digits in the number 100!
;;
;; 
;; A fairly straight forward solution. I already had a function to compute factorial
;; and I used the same function (addem) from problem 16 to sum the digits.

(ns adventofcode)


(defn factorial [n]
  "returns the factorial of the given positive integer argument"
  (if (= n 0) 1
      (loop [val n i n]
        (if (<= i 1) val
            (recur (*' val (dec i)) (dec i))))))

(def number
  (->> (factorial 100)
       str
  )
)


;; fn addem loops through the digits adding the digits to the previous
;; sum in the accumulator (acc).
(defn addem
  "given a 'string' of digits, addem returns the sum of the digits"
  [nums]
  (loop [acc 0 idx 0]
    (if (= idx (count nums))
      acc
      (recur (+ acc (Integer/parseInt (str (nth nums idx)))) (inc idx) )) 
    ))

(addem number)

