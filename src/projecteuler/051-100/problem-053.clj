;; Project Euler
;; Problem 53 - Combinatoric Selections

(ns projecteuler)

(defn factorial [n]
  "returns the factorial of the given positive integer argument"
  (if (= n 0) 1
      (loop [val n i n]
        (if (<= i 1) val
            (recur (*' val (dec i)) (dec i))))))


(defn combinations [n r]
  (/ (factorial n) (*' (factorial r) (factorial (- n r))))
  )

(defn problem-053 []
  (count
   (remove nil?
           (for [n (range 1 101) r (range 1 101)]
             (if (<= r n)
               (if (> (combinations n r) 1000000)
                 [n r]))))))


(problem-053)  ;; => 4075 
