;; Project Euler
;; Problem 038 - Pandigital Multiples
;;

;; concat nx1 and nx2
;; A: if length > 9, then next n
;; if length < 9, then (do (nx3) (goto A))
;; if length = 9, then pandigital?
;;     if not pandigital, then next n
;;     if is pandigital, save the pandigital number
;;
;; sort the resulting list or take the max
;;    

;; loop n from 2 to (987654321 / 2) + 1
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

 
