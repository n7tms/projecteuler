(ns projecteuler.core)
(require '[clojure.string :as str])
;(use '[mylibrary :as ml])

;; Project Euler
;; Problem 013
;; 2021-09-30
;;
;; Work out the first ten digits of the sum of the following one-hundred 50-digit numbers.


;; 37107287533902102798797998220837590246510135740250
;; 46376937677490009712648124896970078050417018260538

;; Read the input data and create a vector of strings
;; ["383838..." "4737389..." ...]
(def data-in
  (str/split (slurp "/home/todd/dev/clojure/projecteuler/001-050/problem-013.txt") #"\n")
)

;(subs (take 1 data-in) 49)
(str/join data-in) ;; => returns one long string of 5000 numbers


(subs (str/join (take 1 data-in)) 0 1)  ;; => "3", the first character
(subs (str/join (take 1 data-in)) 1 2)  ;; => "7", the second character
(subs (str/join (take 1 data-in)) 49 50)  ;; => "0", the last character


data-in


(defn save-last-digit [digits]
  "return the last digit in the sum from sum of the columns"
  (Integer/parseInt (subs (str digits) (- (count (str digits)) 1)))
)

(defn get-remaining-digits [digits]
  "return all but the last digit in the sum from the sum of the columns"
  (Integer/parseInt (subs (str digits) 0 (- (count (str digits)) 1)))
)

(defn sum-of-col [y]
  "Returns the sum of the numbers in column y"
  (reduce +
          (for [x (range 1 101)]
            (Integer/parseInt (subs (str/join (last (take x data-in))) y (inc y))) ))
  )

(defn sums [x]
  (if (= x 49)
    (save-last-digit (sum-of-col x))
    (get-remaining-digits (sums (inc x)))
  )
)

;(sums 0)


(def factorial
  (fn [n]
    (loop [cnt n     acc 1]
      (if (zero? cnt)
        acc
        (recur (dec cnt) (* acc cnt))))))


(def fifty-five
  (loop [sum 0    cnt 10]
    (if (= cnt 0)
      sum
      (recur (+ cnt sum) (dec cnt)))))


; https://clojuredocs.org/clojure.core/recur
; 









(sum-of-col 0)  ;; => 506
(sum-of-col 1)  ;; => 428

(+ (sum-of-col 0)  (get-remaining-digits (sum-of-col 1)))


(let [x 2]
    (+ (sum-of-col x) (if (= x 49) 0 (get-remaining-digits (sum-of-col (inc x)))))
  )

;(for [i (range 49 -1 -1)])

(defn choices [x]
  (case x
    49 (save-last-digit (sum-of-col x))
    0 (+ (sum-of-col x) (get-remaining-digits (sum-of-col (inc x))))
    (save-last-digit (choices (inc x)))
  )
)

(choices 1)

 (reverse  (for [i (range 49 -1 -1)
                   ]
               (choices i)))

; the sum of col(x) = sum of col(x) + remaining digits from sum of col(x+1)
; if x = 49, return last digit




