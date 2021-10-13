(ns projecteuler)


;; Project Euler
;; Problem 16
;;
;; 2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
;; 
;; What is the sum of the digits of the number 2^1000?
;;


;; I needed to create a pwr function that handled big-integers.
;; I calculate 2^1000, convert it to a string and store it in nums.
(def nums (str (pwr 2 1000)))

;; fn addem loops through the digits adding the digits to the previous
;; sum in the accumulator (acc).
(defn addem [nums]
  (loop [acc 0 idx 0]
    (if (= idx (count nums))
      acc
      (recur (+ acc (Integer/parseInt (str (nth nums idx)))) (inc idx) )) 
    ))

(addem nums)   ;; => 1366





