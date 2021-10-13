;; Project Euler
;; Problem 19
;;
;; Counting Sundays
;;


(ns projecteuler)

(def years (range 1901 2001))
(def months [1 2 3 4 5 6 7 8 9 10 11 12])
(def days [31 28 31 30 31 30 31 31 30 31 30 31])

 (for [y years
                 m months
                 d days]
             (for [i 1]
               (while (< i (days m))
                 [y m i]
                 (inc i)))
             )

(days 2)

(days 2)
