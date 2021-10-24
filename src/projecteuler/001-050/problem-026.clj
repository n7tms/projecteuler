;; Project Euler
;; Problem 026 - Reciprocal Cycles
;;
;; solution from https://mishadoff.com/blog/clojure-euler-problem-026
;;

(ns projecteuler)

(defn unit-fraction [denom]
  (loop [numer 1 i 1 known {}]
    (let [r (rem (* 10 numer) denom)]
      (cond (zero? r) 0
            (get known r) (- i (get known r))
            :else (recur r (inc i) (assoc known r i))))))


(defn problem-026 []
  (->> (range 1 1000)
       (map #(vec [% (unit-fraction %)]))
       (apply max-key second)
       (first)))

(problem-026)   ;; => 983
