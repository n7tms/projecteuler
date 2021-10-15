;; Project Euler
;;
;; Problem 21: Amicable Numbers
;;
;; Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
;; If d(a) = b and d(b) = a, where a ≠ b, then a and b are an amicable pair and each of a and b are called
;; amicable numbers.
;;
;; For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; 
;;therefore d(220) = 284. The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.
;;
;; Evaluate the sum of all the amicable numbers under 10000.
;; 
;; 

(ns projecteuler)

(defn divisors
  ""
  [n]
  (filter (comp zero? (partial rem n)) (range 1 n)))



;; My first attempt at this algorithm ran for over 9 hours, brute-forcing a solution
;; and still had not completed. 
;; I replaced a couple of defn's with def's and a solution is now produced in about 52 seconds.


;; create a sequence of vector pairs of the divisors of every number
;; e.g. [...[220 284] [221 31] [222 234] ... [284 220]...]
(defn divisor-pairs [start end]
  (for [a (range start end)]
    (let [b (reduce + (divisors a))]
      [a b])))

;; This dp def is the same as the above defn function, except once the list
;; is generated, it does not have be generated again and again.
(def dp 
  (for [a (range 1 10000)]
    (let [b (reduce + (divisors a))]
      [a b])))


;; This def also used to be a defn. Now, in its current state, it generates
;; the pairs that can be reference repeatedly without regenerating the list
;; each time.
(def list-of-divisor-pairs
  (remove empty? (for [a dp
                       b dp ]
                   (if (and
                        (not= a b)
                        (and (= (first a) (last b)) (= (last a) (first b)) ))
                     [(first a) (first b)]
                     []
                     )
                   )
          ))


;; Iterate through the list of pairs generated above, summing the first
;; number from each pair.
(defn problem-21 []
  (reduce + (for [i (range (count list-of-divisor-pairs))]
              (first (nth list-of-divisor-pairs i))
              ))
)

(problem-21)   ;; => 31626







;; I found the following (beautiful) solution at https://zach.se/project-euler-solutions/21
;; This algorithm produce a solution in about 11 seconds on my system.
;;
;; He had provided his own divisor function, but mine works fine with his solution, too.

(defn d [n]
  (reduce + (divisors n)))

(defn amicable? [a b]
  (and (not (= a b)) (= (d a) b) (= (d b) a)))

(defn prob-21 []
  (reduce + (filter #(amicable? % (d %)) (range 10000))))


