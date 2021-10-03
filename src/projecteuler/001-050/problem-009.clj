(ns user)

;; Project Euler
;; Problem 008
;; 2021-09-27
;;
;; A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
;; a^2 + b^2 = c^2
;; For example, 3^2 + 4^2 = 9 + 16 = 25 = 5^2.

;; There exists exactly one Pythagorean triplet for which a + b + c = 1000.
;; Find the product abc.

(defn sqr [x]
  (* x x))

(reduce * (apply list (first (for [a (range 1 1000)
                            b (range 1 1000)
                            c (range 1 1000)
                            :when (and
                                   (= (+ a b c) 1000)
                                   (= (+ (sqr a) (sqr b)) (sqr c)))]
                        [a b c]
                        )
                      )))  ;; => 31875000
;; => The numbers are a,b,c respectively, [200 375 425]


