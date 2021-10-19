;; Project Euler
;; Problem 768 - Chandelier
;;
;; A certain type of chandelier contains a circular ring of n evenly spaced candleholders.
;; If only one candle is fitted, then the chandelier will be imbalanced. However, if a second
;; identical candle is placed in the opposite candleholder (assuming n is even) then perfect 
;; balance will be achieved and the chandelier will hang level.
;; 
;; Let f(n,m) be the number of ways of arranging m identical candles in distinct sockets of a chandelier
;;  with n candleholders such that the chandelier is perfectly balanced.
;; 
;; For example, f(4,2) = 2: assuming the chandelier's four candleholders are aligned with the compass points,
;; the two valid arrangements are "North & South" and "East & West". Note that these are considered 
;; to be different arrangements even though they are related by rotation.
;;
;; You are given that f(12,4) = 15 and f(36,6) = 876.
;; 
;; Find f(360,20).


(ns projecteuler)

(defn power
  "raise \"base\" to the power of \"exp\""
  [base exp]
  (reduce * (repeat exp (biginteger base)))
  )


;; 2r11
;; (count (clojure.string/replace (Long/toBinaryString 13) #"0" ""))


(count
 (filter
  #(=
    (count (clojure.string/replace (Long/toBinaryString %) #"0" "")) 10)
  (range (inc (- (power 2 180) (power 2 170))))))



