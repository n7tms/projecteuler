;; Project Euler
;; Problem 62 - Cubic Permutations
;;


(ns projecteuler
  (:require [clojure.math.combinatorics :as combo]))

(defn to-digits [num]
  (map #(Character/getNumericValue %) (str num)))


(combo/permutations (to-digits 123))


(for [x '(344 345 346)]
  x)

(Math/cbrt 8)

(def x 345)
(= (float  x) (Math/cbrt 41063625))
(= (float x) 345.000000000000000001)
