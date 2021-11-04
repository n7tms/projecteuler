;; Project Euler
;; Problem 24 - Lexicographic permutations
;;
;; A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation 
;; of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, 
;;we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:
;;
;; 012   021   102   120   201   210
;;
;; What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
;;

(ns projecteuler
  (:require [clojure.math.combinatorics :as combo]))

;; (defn permutations [s]
;;   (lazy-seq
;;    (if (next s)
;;      (for [head s
;;            tail (permutations (disj s head))]
;;        (cons head tail))
;;      [s])))


;; (nth (permutations #{0 1 2 3 4 5 6 7 8 9}) 999999)
;; (nth-permutation '(0 1 2) 2)

;; (permutations #{0 1 2})
;; (nth (permutations #{0 1 2}) 2)  ;; => (1 0 2)

;(nth  (permutations #{0 1 2 3 4 5 6 7 8 9}) 999999) ;; => (1 9 5 4 8 7 3 6 2 0)


;; (defn perms [s]
;;   (lazy-seq
;;    (if (seq (rest s))
;;      (apply concat (for [x s]
;;                      (map #(cons x %) (perms (remove #{x} s)))))
;;      [s])))

;; (nth (perms #{0 1 2 3 4 5 6 7 8 9}) 999999)   ;; => (1 9 5 4 8 7 3 6 2 0)


;; several different algorithms later, I always produce the same result, but it is 
;; not accepted by project euler.  :-(
;; However, using the permutations function in math.combinatorics seem to provide an answer rather quickly.
;; (hmmm?)


(defn problem-024 [digits n]
  (apply str (nth (combo/permutations digits) (dec n))))


(problem-024 "0123456789" 1000000) ;; => 2783915460

