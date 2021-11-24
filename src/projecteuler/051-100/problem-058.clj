;; Project Euler
;; Problem 58 - Spiral Primes
;;
;; I figured this out on my own, but my algorithm took about 2 days (!!) to obtain a solution.
;; See blocks below this original code for snippets from other developers for more efficient
;; solutions.
;;

(ns projecteuler)

(defn prime? [n]
  (not-any? zero? (map #(rem n %) (range 2 n))))



(defn prime-corners [ring]
  "give a certain 'ring' in the spiral, return the corners that are prime"
  (let [side-length (+ (* ring 2) 1)
        corner-sqrd (* side-length side-length)]
    (remove nil?
            (for [i (range 1 4)]
              (let [x (- corner-sqrd (* (dec side-length) i))]
                (if (prime? x) x)))))
)

(defn calc-percentage [ring num-primes]
  (float (/ num-primes (+ (* ring 4) 1)))
)


(defn problem-058 []
  (loop [ring 3
         all-prime-corners '(3 5 7 13 17)]
    (let [new-prime-corners (flatten (cons all-prime-corners (prime-corners ring)))]
      (if (< (calc-percentage ring (count new-prime-corners)) 0.1)
        (+ (* ring 2) 1)
        (recur (inc ring) new-prime-corners))))
)


;(time (problem-058))   ;; => 26241 ("Elapsed time: 1.38044139050265E8 msecs")

;; This took WAAAAAAY too long. 
;; I'm committed to finding a faster resolution.

;; ====================================================================================
;; Other's solutions
;;

;; roboloco solution (https://roboloco.net/project-euler/problem-58/)
;; He used the clojure.contrib primes lazy-seq that is now deprecated.
;; So I substituted my fn to determine if a number is prime.
;;
(defn euler-58-loop []
  (let [corners (fn [n] (take 4 (iterate #(- % (dec n)) (* n n))))]
    (loop [n 3
           trues 0
           falses 1]
      (let [c (count (filter prime? (corners n)))
            ts (+ trues c)
            fs (+ falses (- 4 c))]
        (if (> 1/10 (/ ts (+ ts fs)))
          n
          (recur (+ n 2) ts fs))))))

;(time (euler-58-loop))







