;; Project Euler
;; Problem 28 - Number Spiral Diagonals
;;
;; A couple of solutions from Ivan Thorson (https://roboloco.net/project-euler/problem-28/)
;;

(ns projecteuler)

(defn diagonals [start space]
  (take 4 (drop 1 (take-nth space (range start (+ start (* 5 space)))))))

(defn euler-28 [spiral]
  (loop [sum 1
         start 1
         by 2]
    (let [ds (diagonals start by)]
      (if (>= start (* spiral spiral))
        sum
        (recur (+ sum (reduce + ds)) (last ds) (+ 2 by))))))

(euler-28 5)


(defn euler-28-revised [n]
  "Compute the sum of the diagonals in an n by n clockwise spiral. "
  (assert (and (> n 0) (odd? n))) ;; Must be odd to have diagonals!
  (let [diags (fn [m] (take 4 (iterate #(- % (dec m)) (* m m))))
        squares (take-while #(> % 1) (iterate #(- % 2) n))]
    (reduce + (flatten [(map diags squares) 1]))))

(euler-28-revised 1001)  ;; => 669171001



