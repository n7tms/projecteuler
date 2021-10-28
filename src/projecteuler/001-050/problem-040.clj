;; Project Euler
;; Problem 40 - Champernowne's constant
;;

(ns projecteuler)


;; create fractional-part with at least 1000000 digits.
(def fractional-part
  (apply str (range 1 300000)))

;; d1 × d10 × d100 × d1000 × d10000 × d100000 × d1000000
(defn problem-040 []
  (*
   (Integer/parseInt (str (nth fractional-part 0)))
   (Integer/parseInt (str (nth fractional-part 9)))
   (Integer/parseInt (str (nth fractional-part 99)))
   (Integer/parseInt (str (nth fractional-part 999)))
   (Integer/parseInt (str (nth fractional-part 9999)))
   (Integer/parseInt (str (nth fractional-part 99999)))
   (Integer/parseInt (str (nth fractional-part 999999)))))

(problem-040)   ;; => 210 




