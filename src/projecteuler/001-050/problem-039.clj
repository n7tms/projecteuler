;; Project Euler
;; Problem 039 - Integer Right Triangles
;;
;; If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, 
;; there are exactly three solutions for p = 120.
;;
;; {20,48,52}, {24,45,51}, {30,40,50}
;;
;; For which value of p ≤ 1000, is the number of solutions maximised?
;;

(ns projecteuler)


(defn perimeter [a b]
  (+ a b (Math/sqrt (+ (* a a) (* b b)))))


(defn sort-by-val [s] (sort-by val s))

(defn calc-perimeters []
          (for [a (range 1 1001)
                b (range 1 1001)]
            (let [p (perimeter a b)]
              (if (< (int p) p)
                0
                (int p))))
)

(defn problem-039 []
  (->> (calc-perimeters)
       (remove zero?)
       (filter #(< % 1000))
       (frequencies)
       (sort-by-val)
       (last)
       (first)
       ))

(problem-039)   ;; => 840
