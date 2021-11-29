;; Project Euler
;; Problem 61 - Cyclical Figurate Numbers
;;
;; solution from: https://roboloco.net/project-euler/problem-61/
;;

(ns projecteuler)

(def triangle-nums   (map #(/ (* % (+ % 1)) 2) (iterate inc 1)))
(def square-nums     (map #(* % %) (iterate inc 1)))
(def pentagonal-nums (map #(/ (* % (- (* 3 %) 1)) 2) (iterate inc 1)))
(def hexagonal-nums  (map #(* % (- (* 2 %) 1)) (iterate inc 1)))
(def heptagonal-nums (map #(/ (* % (- (* 5 %) 3)) 2)  (iterate inc 1)))
(def octagonal-nums  (map #(* % (- (* 3 %) 2)) (iterate inc 1)))

(defn conj-to-cycle
  "Conjugates an element to the front or back of a list of elements such that
  the list is cyclic (overlap 2). Returns nil if n cannot be added validly. "
  [coll n]
  (cond
   (= (subs (str (first coll)) 0 2) (subs (str n) 2 4)) (concat [n] coll)
   (= (subs (str (last coll)) 2 4) (subs (str n) 0 2)) (concat coll [n])
   :else nil))

(defn cyclic?
  "Returns true iff the collection's last element's last two digits are the same
  as the first element's first two digits. "
  [coll]
  (= (subs (str (first coll)) 0 2)
     (subs (str (last coll)) 2 4)))

(defn take-between [bot top coll]
  (take-while #(< % top) (drop-while #(< % bot) coll)))

(defn conj-valid-nums
  "Attempts to conjugate all valid 4 digit numbers in the lazy seq nums into
  the cyclic collections held in coll."
  [coll nums]
  (remove nil? (map #(conj-to-cycle coll %) (take-between 1000 10000 nums))))

(defn euler-61 []
  (first
   (for [a (map vector (take-between 1000 10000 triangle-nums))
         b (conj-valid-nums a square-nums)
         c (conj-valid-nums b pentagonal-nums)
         d (conj-valid-nums c hexagonal-nums)
         e (conj-valid-nums d heptagonal-nums)
         f (conj-valid-nums e octagonal-nums)
         :when (cyclic? f)]
     (reduce + f))))

(time (euler-61))  ;; => 28584  (in 52 msec)

