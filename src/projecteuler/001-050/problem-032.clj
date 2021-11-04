;; Project Euler
;; Problem 32 - Pandigital Products
;; 
;; My spin on Ivar Thorson's solution (https://roboloco.net/project-euler/problem-32/)
;;


(defn to-digits [num]
  (map #(Character/getNumericValue %) (str num)))

(defn product-digits [a b]
  "Returns a list of digits of a, b, and (* a b)."
  (lazy-cat (to-digits a) (to-digits b) (to-digits (* a b))))

(defn unique-digits? [digits]
  (let [unique (distinct digits)]
    (= (count unique) (count digits))))


(defn pandigital? [digits]
  (and (nil? (some zero? digits))
       (= 9 (count digits))
       (unique-digits? digits)))

(defn problem=032 []
  (reduce + (into #{} (concat
                       (for [a (range 1 10)
                             b (range 1000 10000)
                             :when (pandigital? (product-digits a b))]
                         (* a b))
                       (for [a (range 10 100)
                             b (range a 1000)
                             :when (pandigital? (product-digits a b))]
                         (* a b))))))

(problem-032)  ;; => 45228


