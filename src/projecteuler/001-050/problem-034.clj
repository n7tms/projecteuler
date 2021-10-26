;; Project Euler
;; Problem 34 - Digit Factorials
;; 


(ns projecteuler
  (:require [clojure.string :as str]))

(defn factorial [n]
  "returns the factorial of the given positive integer argument"
  (if (= n 0) 1
      (loop [val n i n]
        (if (<= i 1) val
            (recur (*' val (dec i)) (dec i))))))

(defn sum [num]
  (let [nums (mapv #(Integer/parseInt %) (re-seq #"\d" (str num)))]
    (->> nums
         (map factorial)
         (reduce +))
    ))


;; I used a range 3 - 100000001 and still only came up with two numbers
;; when I plugged in the sum of those two numbers, the solution was correct.
(defn problem34 []
  (reduce +
          (remove nil?
                  (for [x (range 3 100001)]
                    (if (= x (sum x)) x)))))

(problem34)   ;; => 40730

