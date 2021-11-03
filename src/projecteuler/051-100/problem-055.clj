;; Project Euler
;; Problem 55 - Lychrel Numbers
;;

(ns projecteuler)

(defn palindrome? [num]
  (= (str num) (apply str (reverse (str num)))))

(defn rev [num]
  (bigint (apply str (reverse (str num))))
  )

(defn lychrel? [num]
  (loop [x (+ (bigint num) (rev num))
         cnt 0]
    (if (> cnt 50)
      true
      (if (palindrome? x)
        false
        (recur (+' x (rev x)) (inc cnt)))
      ))
  )

(defn problem-055 []
  (count
   (remove false?
           (for [x (range 10000)]
             (lychrel? x)))))

(problem-055)  ;; => 249

        

