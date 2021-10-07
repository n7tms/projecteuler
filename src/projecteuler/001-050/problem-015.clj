(ns projecteuler.core
  (:use clojure.math.combinatorics
        clojure.math.numeric-tower)
  )


(defn pascal-next-row [x]
  (let [size (count x)]
    (for [i (range (inc size))]
      (if (or (= 0 i) (= size i)) 1
          (+ (nth x (dec i)) (nth x i))))))



;(pascal-next-row [1 1])   ;; => (1 2 1)

(take 5 (iterate pascal-next-row [1]))

(defn problem-15 []
  (let [n 20 d (inc (* n 2))]
    (nth (last (take d (iterate pascal-next-row [1]))) n)))

(println (problem-15))


;; Solution from http://mishadoff.com/blog/clojure-euler-problem-015/
;; Note:
;; Prior to seeking a solution, I had found the 2n+1 relationship, but failed
;; to recognize Pascal's triangle. I was thinking there was a factorial
;; relationship. Despite dozens of drawings, I missed the diagonals.
