;; Project Euler
;; Problem 52 - Permuted Multiples
;;

(ns projecteuler
  (:require [clojure.string :as str]))


(defn same-digits?
  "compare the digits in num1 and num2; return true if they both contain the same digits"
  [num1 num2]
  (=
   (sort (mapv #(Integer/parseInt %) (str/split (str num1) #"" )))
   (sort (mapv #(Integer/parseInt %) (str/split (str num2) #"" )))
   )
  )


(defn problem-52 []
  (remove nil?
          (for [x (range 1 1000000)]
            (if (and
                 (same-digits? x (* x 2))
                 (same-digits? x (* x 3))
                 (same-digits? x (* x 4))
                 (same-digits? x (* x 5))
                 (same-digits? x (* x 6))
                 )
              x
              ))))

(problem-52)  ;; => 142857
