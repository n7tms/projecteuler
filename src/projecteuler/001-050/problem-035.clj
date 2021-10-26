;; Project Euler
;; Problem 34 - Circular Primes
;;

(ns projecteuler
  (:require [clojure.string :as str]))

;; Is a number a prime number
(defn prime? [n]
  (not-any? zero? (map #(rem n %) (range 2 n))))

;; I only need to check odd numbers (range 3 1000000 2) because even numbers are not prime
;; I also only need to check numbers that have odd numbers in them, because if it contains
;; an even number then at least of the circulars will not be prime.
;;


(defn circular-prime? [num]
  )

(range 3 20 2)

(contains? [1 2 3 4] 5)

;; does the set contain an even number? yes=true, no=nil
;; zero is considered even, so this works well.
(some #(even? %) [1  3 0 5])

;; create a list of number from 3 to 1000000 that does not contain any even numbers
;; or numbers containing a 5
(def candidates
  (cons 2
        (cons 5
              (remove #(not (prime? %))
                      (remove nil? (for [x (range 3 100 2)]
                                     (if (some #(even? %) (mapv #(Integer/parseInt %) (re-seq #"\d" (str x))))
                                       nil
                                       (if (some #{5} (mapv #(Integer/parseInt %) (re-seq #"\d" (str x))))
                                         nil
                                         x))
                                     ))))))

(def digits [1 2 3])

(defn vectorize [x]
  (for [a x]
    [a]))


(defn rotate [x]
  (vec  (cons (last x) (butlast x))) 
)

(defn rotated [digits]
  (loop [cnt (count digits) x digits result []]
    (if (= cnt 0)
      result
      (recur (dec cnt) (rotate x) (conj result (rotate x)))
      )
    )
)

(for [x candidates]
  (map #(prime? %) (map #(Integer/parseInt (apply str %)) (rotated  (mapv #(Integer/parseInt %) (re-seq #"\d" (str 11))))))
)

(if (some #{false} '((true) (true) (true) (true) (true) (true) (false) (true) (true) (true) (true) (true) (true) (true)))
  false
  true) 

(map #(prime? %) (map #(Integer/parseInt (apply str %)) (rotated  (mapv #(Integer/parseInt %) (re-seq #"\d" (str 11))))))

(rotated (vec (list 135)))

(prime? 91)
(Integer/parseInt (apply str [1 2 3]))




