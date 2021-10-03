(ns projecteuler.core
  (:gen-class)
  (:require [clojure.string :as str])
)


;Project Euler #3
(defn factors-starting-at [f n]
  (cond
    (> f (Math/sqrt n)) (if (= n 1) [] [n])
    (= 0 (mod n f)) (cons f (factors-starting-at f (/ n f)))
    :else (recur (inc f) n)))

(defn prime-factors-of [n]
  (factors-starting-at 2 n))

(prime-factors-of 600851475143)
;6857
