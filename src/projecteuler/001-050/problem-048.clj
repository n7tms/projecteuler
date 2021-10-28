;; Project Euler
;; Problem 48 - Self Powers 

(ns projecteuler)

(defn power
  "raise 'base' to the power of 'exp'"
  [base exp]
  (reduce * (repeat (bigint exp) (bigint base)))
  )

;; Use a for loop to create a sequence of x^x for numbers from 1 to 1000
;; Add them all up
;; Convert it to a string and extract the last 10 "characters"
(defn problem-048 []
  (let
      [answer
       (str
        (reduce +
                (for [x (range 1 1001)]
                  (power x x))))]
    (subs answer (- (count answer) 10))))

(problem-048)  ;; => 9110846700


