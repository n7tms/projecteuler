;; Project Euler
;; Problem 19
;;
;; Counting Sundays
;;

;; day-of-week calculations derived from https://artofmemory.com/blog/how-to-calculate-the-day-of-the-week/
;;

(ns projecteuler)

(def years (vec (range 1901 2001)))
(def months (vec (range 1 13)))

;; formula: (Year code + month code + century code + date number - leap year code) mod 7

(defn year-code
  "accepts a 2 or 4 digit year and returns the year-code"
  [y]
  (let [year (mod y 100)]
    (mod (+ year (quot year 4)) 7))
  )


(defn month-code [m]
  ([0 3 3 6 1 4 6 2 5 0 3 5] (dec m))
  )

(defn century-code
  "accepts a 4 digit year (1700-2399) and returns the century-code"
  [c]
  (let [cc (quot c 100)]
    (case cc
      17 4
      18 2
      19 0
      20 6
      21 4
      22 2
      23 0))
  )

(defn leap-year?
  "given a year, returns true if it is a leap year; otherwise, false"
  [y]
  (cond (zero? (mod y 400)) true
        (zero? (mod y 100)) false
        (zero? (mod y   4)) true
        :default false)
  )

(defn leap-year-code
  "given a month and year, returns -1 if Jan or Feb AND year is a leap year; otherwise, 0"
  [m y]
  (if (and (leap-year? y) (or (= m 1) (= m 2))) -1 0)
  )

(defn day-of-week
  "given a date, returns the day of the week, 0=Sun, 1=Mon, ... 6=Sat"
  [y m d]
  (mod (+ (year-code y) (month-code m) (century-code y) d (leap-year-code m y)) 7))

(def days ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])



;; iterate over the given years and months checking for the first of the month
;; falling on a Sunday. 
;; count them up and return the sum.
(reduce +
        (for [y years
              m months]
          (if (zero? (day-of-week y m 1))
            1
            0))
)







