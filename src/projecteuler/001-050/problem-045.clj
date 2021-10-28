;; Project Euler
;; Problem 45 - Triangular, Pentagonal, Hexagonal
;;

(ns projecteuler)

;; According to wikipedia, every hexagonal number is a triangular number. 
;; Therefore, we only need to see which hexagonal numbers are also
;; pentagonal numbers.

(def triangle-numbers
  "lazy-sequence of triangle numbers"
  (map (fn [x] (/ (* x (inc x)) 2)) (iterate inc 1))
  )

(defn triangle? [value]
  (if (< value 0)
    false
    (let [c (* -2 value)
          b 1
          a 1
          d (- (* b b) (* 4 a c))]
      (if (< d 0)
        false
        (let [root1 (/ (+ (- b) (Math/sqrt d)) (* 2 a))
              root2 (/ (- (- b) (Math/sqrt d)) (* 2 a))]
          (if (and (> root1 0)
                   (= root1 (Math/floor root1)))
            true
            (if (and (> root2 0)
                     (= root2 (Math/floor root2)))
              true
              false))))))
  )


(def pentagonal-numbers
  "lazy-sequence of pentagonal numbers"
  (map (fn [x] (/ (* x (- (* 3 x) 1)) 2)) (iterate inc 1)))

(defn pentagonal? [value]
  (zero? (rem (+ 1 (Math/sqrt (+ 1 (* 24 value)))) 6)))


(def hexagonal-numbers
  "laxy-sequence of hexagonal numbers"
  (map (fn [x] (* x (- (* 2 x) 1))) (iterate inc 1)))

(defn hexagonal? [value]
  (let [n (/ (+ (Math/sqrt (+ (* 8 value) 1)) 1) 4)]
    (if (= n (Math/floor n))
      true
      false))
  )


(defn problem-045 []
  (last
   (take 3
         (for [a hexagonal-numbers
               :when (pentagonal? a)]
           a))))

(problem-045)  ;; => 1533776805


