

(ns projecteuler)

(def sqrt2 (iterate #(/ (+ (numerator %) (* 2 (denominator %)))
                        (+ (numerator %) (denominator %)))
                    (/ 3 2)))

(defn bigger-numr? [n]
  (> (count (str (numerator n)))
     (count (str (denominator n)))))

(defn problem-057 []
  (count (filter bigger-numr? (take 1000 sqrt2))))


(time (problem-057))

