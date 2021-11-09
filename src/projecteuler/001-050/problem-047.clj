;; Project Euler
;; Problem 47 - Distinct Primes Factors
;;
;; Prime factors algorithm from Uncle Bob (https://gist.github.com/unclebob/632303)
;;

(defn factors-starting-at [f n]
  (cond
    (> f (Math/sqrt n)) (if (= n 1) [] [n])
    (= 0 (mod n f)) (cons f (factors-starting-at f (/ n f)))
    :else (recur (inc f) n)))

(defn prime-factors-of [n]
  (factors-starting-at 2 n))


(defn problem-047 []
  (first
   (remove nil?
           (for [x (range 200000)]
             (if (and
                  (= 4 (count (distinct (prime-factors-of x))))
                  (= 4 (count (distinct (prime-factors-of (+ 1 x)))))
                  (= 4 (count (distinct (prime-factors-of (+ 2 x)))))
                  (= 4 (count (distinct (prime-factors-of (+ 3 x))))))
               x)))))

(problem-047)   ;; => 134043
