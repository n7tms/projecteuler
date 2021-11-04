;; Project Euler
;; Problem 31 - Coin Sums
;;
;; Solution provided by Ivar Thorson (https://roboloco.net/project-euler/problem-31/)
;;

;; Possible currency/coin values
;; e.g. 200 will 2 pounds
(def coins [200 100 50 20 10 5 2 1])

(defn coin-combos [sum goal maxcoin]
  (let [valid (filter #(and (>= maxcoin %)
                            (>= (- goal sum) %)) coins)]
    (if (empty? valid)
      (if (= sum goal) 1 0)
      (reduce + (map #(coin-combos (+ sum %) goal %) valid)))))

(defn problem-031 []
  (coin-combos 0 200 200))

(problem-031)
