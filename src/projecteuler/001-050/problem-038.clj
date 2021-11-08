;; Project Euler
;; Problem 038 - Pandigital Multiples
;;
(ns projecteuler)


(defn to-digits [num]
  (map #(Character/getNumericValue %) (str num)))

(defn unique-digits? [digits]
  (let [unique (distinct digits)]
    (= (count unique) (count digits))))

(defn pandigital? [digits]
  "returns true if a seq of digits is pandigital-9"
  (and (nil? (some zero? digits))
       (= 9 (count digits))
       (unique-digits? digits)))

;; concat nx1 and nx2
;; A: if length > 9, then next n
;; if length < 9, then (do (nx3) (goto A))
;; if length = 9, then pandigital?
;;     if not pandigital, then next n
;;     if is pandigital, save the pandigital number
;;
;; sort the resulting list or take the max
;;    

;; loop n from 2 to 50000 
;;   let result = n
;;   loop m from 2 to 9 (can't be longer than 9 digits)
;;     concat result and n times m
;;     if (length concat) = 9
;;        if concat is pandigital
;;           save concat
;;           loop n
;;        if (length concat) < 9
;;           loop m
;;           loop n

(defn problem-038 []
  (apply max
         (map #(Integer/parseInt %)
              (remove nil?
                      (for [n (range 2 50000)]
                        (loop [result (str n)
                               mult 1]
                          (if (< (count (str result)) 10)
                            (if (pandigital? (to-digits result))
                              result
                              (recur (str result (* n (inc mult))) (inc mult)) ))))))))

(problem-038)   ;; => 932718654



