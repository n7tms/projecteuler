;; Project Euler
;; Problem 33 - Digit Cancelling Fractions
;;
;; "... If the product of these four fractions is given in its lowest common terms, find the value of the denominator."
;;
;; I'm not even sure what this problem is asking for ... !!
;; And after reading some of the comments in the forum, I was not alone!
;;
;; The following solution is from "bibi" which I found through Ivar Thorson (https://roboloco.net/project-euler/problem-33/)
;;

(defn non-trivial?[n d]
  (let [x (quot n 10)
        y (rem n 10)
        u (quot d 10)
        v (rem d 10)]
    (cond
     (zero? v) false
     (and (= y u) (= (/ x v) (/ n d))) true
     :else false)))
 
(defn euler-33-bibi []
  (let [fracs (for [n (range 10 99)
                    d (range (inc n) 100)
                    :when (non-trivial? n d)]
                (/ n d))]
    (.denominator (reduce * fracs))))

(euler-33-bibi) ;; => 100
