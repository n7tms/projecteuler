(ns projecteuler)

(defn power
  "raise \"base\" to the power of \"exp\""
  [base exp]
  (reduce * (repeat (bigint exp) (bigint base)))
  )


(defn problem29 [max]
  (count
   (distinct
    (for [a (range 2 max) b (range 2 max)]
      (power a b)
      ))))

(problem29 101)    ;; => 9183

