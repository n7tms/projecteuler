;; Project Euler
;; Problem 30 - Digit fifth powers
;;
;; A solution from Ivar Thorson (https://roboloco.net/project-euler/problem-30/)
;;



(defn power
  "raise 'base' to the power of 'exp'"
  [base exp]
  (reduce * (repeat (bigint exp) (bigint base)))
  )


(defn as-digits [num]
  (map #(Character/getNumericValue %) (str num)))

(defn sum-of-fifth-powers? [num]
  (= num (reduce + (map #(power % 5) (as-digits num)))))

(defn problem-030 []
  (reduce + (filter sum-of-fifth-powers? (range 10 1000000))))

(problem-030)  ;; => 443839

;; Thanks, rafsoaken!
(defn euler-30-rafsoaken []
  (let [pow5 (apply hash-map (interleave (seq "0123456789") 
                                         (map #(power % 5) (range 10))))
        max  (-> \9 pow5 (* 6))]
    (reduce + (filter #(= % (reduce + (map pow5 (str %)))) 
                      (range 2 max) ))))

(euler-30-rafsoaken)

