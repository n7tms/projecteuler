;; Project Euler
;; Problem 62 - Cubic Permutations
;;
;; The cube, 41063625 (3453), can be permuted to produce two other cubes: 
;; 56623104 (3843) and 66430125 (4053). In fact, 41063625 is the smallest 
;; cube which has exactly three permutations of its digits which are also cube.
;;
;;Find the smallest cube for which exactly five permutations of its digits are cube.

(ns projecteuler
  (:require [clojure.math.combinatorics :as combo]))

(defn to-digits [num]
  (map #(Character/getNumericValue %) (str num)))

(def cubes (map #(* % % %) (range 346 10000)))

;; generate a long list of cubes
;; "for" each cube in the list
;;    generate all the permutations
;;    map - take the cube-root of each permutation
;;    filter out the ones that are not perfect roots
;;    see if there are 5 cube roots remaining
;;    if so, that's our man!


(defn digits-to-int [x]
  "given: '(1 2 3), return: 123"
  (Integer/parseInt (apply str  x))
)


(defn problem-062 []
  (remove nil? 
          (for [x cubes]
            (if (= 5 (count
                      (remove #(not= (float %) %)
                              (map #(Math/cbrt %)
                                   (map #(digits-to-int %)
                                        (combo/permutations (to-digits x)))))))
              x)
            )))

(time (problem-062))








