(ns projecteuler)

(defn prime? [n]
  (not-any? zero? (map #(rem n %) (range 2 n))))


;; let x = the ring
;; corners we want to look at:
;;    let square corner (sqc) = (+ (* 2 x) 1)
;;    let temp = (dec sqc)     (- sqc 1)
;;    let corner 1 (c1) = (- (* sqc sqc) temp) 
;;    let corner 2 (c2) = (- (* sqc sqc) (* temp 2))
;;    let corner 3 (c3) = (- (* sqc sqc) (* temp 3))
;;
;;  let number-of-diag-numbers = (+ (* x 4) 1)


;; (defn corners [sl]
;;   (for [side-length (range 3 (inc sl) 2)]
;;     (let [sqr-corner  (* side-length side-length)  
;;           corner1     (- sqr-corner (dec side-length))
;;           corner2     (- sqr-corner (* (dec side-length) 2))
;;           corner3     (- sqr-corner (* (dec side-length) 3))]

;;       [corner1 corner2 corner3]

;;       )))


(defn prime-corners [ring]
  "give a certain 'ring' in the spiral, return the corners that are prime"
  (let [side-length (+ (* ring 2) 1)
        corner-sqrd (* side-length side-length)]
    (remove nil?
            (for [i (range 1 4)]
              (let [x (- corner-sqrd (* (dec side-length) i))]
                (if (prime? x) x)))))
)

(defn calc-percentage [ring num-primes]
  (float (/ num-primes (+ (* ring 4) 1)))
;  (float (/ (count (filter #(prime? %) (flatten (corners (+ (* 2 sides) 1))))) (+ (* sides 4) 1)))
)

(calc-percentage 2 5)

(defn problem-058 []
  (loop [ring 3
         all-prime-corners '(3 5 7 13 17)]
    (let [new-prime-corners (flatten (cons all-prime-corners (prime-corners ring)))]
      (if (< (calc-percentage ring (count new-prime-corners)) 0.1)
        (+ (* ring 2) 1)
        (recur (inc ring) new-prime-corners))))
)


;(time (problem-058))






