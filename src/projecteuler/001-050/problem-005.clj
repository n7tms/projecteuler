(ns user
  (:gen-class)
  (:require [clojure.string :as str])
)

;; Project Euler #5
;; 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
;;
;; What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?


;; found-it-small? is just to prove that I can solve the sample given in the problem statement.
(defn found-it-small? [x]
  (=
   (mod x 2)   (mod x 3)   (mod x 4)   (mod x 5)   (mod x 6)
   (mod x 7)   (mod x 8)   (mod x 9)   (mod x 10))
)

;; proves that the logic works
(found-it-small? 2520)  ; => true

(defn found-it? [x]
  (=
   (mod x 2)   (mod x 3)   (mod x 4)   (mod x 5)   (mod x 6)
   (mod x 7)   (mod x 8)   (mod x 9)   (mod x 10)   (mod x 11)
   (mod x 12)   (mod x 13)   (mod x 14)   (mod x 15)   (mod x 16)
   (mod x 17)   (mod x 18)   (mod x 19)   (mod x 20))
)

(defn find-it [num]
  (loop [x num]
    (if-not (found-it? x)
      (do
        ;(print x " ")
        (recur (inc x)))
      x))
)

(defn do-it []
  (println (find-it 2))) 

;; => 232792560

;; There is probably a faster, simpler way. I brute-forced it ... checking every number, but I found it!
;; And I found it without any help with the logic!

;; better solutions
;; http://mishadoff.com/blog/clojure-euler-problem-005/
