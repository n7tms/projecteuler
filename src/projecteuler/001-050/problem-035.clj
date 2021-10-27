;; Project Euler
;; Problem 34 - Circular Primes
;;

(ns projecteuler
  (:require [clojure.string :as str]))


(defn prime? [n]
  "returns true if n is prime, false if it is not"
  (not-any? zero? (map #(rem n %) (range 2 n))))

;; I only need to check odd numbers (range 3 1000000 2) because even numbers are not prime
;; I also only need to check numbers that have odd numbers in them, because if it contains
;; an even number (or a 5) then at least one of the circulars will not be prime.
(def candidates
  (cons 2
        (cons 5
              (remove #(not (prime? %))
                      (remove nil? (for [x (range 3 1000000 2)]
                                     (if (some #(even? %) (mapv #(Integer/parseInt %) (re-seq #"\d" (str x))))
                                       nil
                                       (if (some #{5} (mapv #(Integer/parseInt %) (re-seq #"\d" (str x))))
                                         nil
                                         x))
                                     ))))))

(defn rotate [x]
  "remove the last element in the list x and append it to the front"
  (vec  (cons (last x) (butlast x))) 
)

(defn rotated [digits]
  "given a vector of digits, returns a vector of rotated vectors"
  (loop [cnt (count digits) x digits result []]
    (if (= cnt 0)
      result
      (recur (dec cnt) (rotate x) (conj result (rotate x)))
      )
    )
)

(defn circular-prime? [num]
  "returns true if all rotations of num are prime; otherwise, false"
  (if (empty?
       (filter false? 
               (mapv
                #(prime? %)
                (map #(Integer/parseInt (apply str %))
                     (rotated
                      (mapv #(Integer/parseInt %)
                            (re-seq #"\d" (str num))))))))
    true
    false)
)

(defn problem-035 []
  "iterate through the candidates checking for circular prime. 
   return the count"
  (count (remove nil?
                 (for [x candidates]
                   (if (circular-prime? x) x nil)))))


(problem-035)  ;; => 55







