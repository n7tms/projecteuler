;; Project Euler
;; Problem 27 - Quadratic Primes
;;

(ns projecteuler)



(defn prime? [n]
  "determines if n is a prime number"
  (not-any? zero? (map #(rem n %) (range 2 n))))

(defn quadratic [a b]
  (map (fn [n] (+ (* n n) (* a n) b)) (iterate inc 0)))

(defn problem-027 []
  (let [quads (for [a (range -1000 1000)
                    b (range -1000 1000)]
                [a b (count (take-while #(and (> % 0)
                                              (prime? %))
                                        (quadratic a b)))])
        [a b _] (reduce #(if (> (nth %1 2) (nth %2 2)) %1 %2) quads)]
    (* a b)))

(problem-027)  ;; => -59231



      
