;; Project Euler
;; Problem 54 - Poker Hands
;;


;;       High Card: Highest value card.
;;        One Pair: Two cards of the same value.
;;       Two Pairs: Two different pairs.
;; Three of a Kind: Three cards of the same value.
;;        Straight: All cards are consecutive values.
;;           Flush: All cards of the same suit.
;;      Full House: Three of a kind and a pair.
;;  Four of a Kind: Four cards of the same value.
;;  Straight Flush: All cards are consecutive values of same suit.
;;     Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.


(ns projecteuler
  (:require [clojure.string :as string]))

(defn split [re s]
  (string/split s re))

(defn change [match replacement s]
  (string/replace s match replacement))

(defn consecutive? [values]
  (let [vals (sort values)]
    (empty? (remove #{1} (map - (next vals) vals)))))

(def small "8C TS KC 9H 4S 7D 2S 5D 3S AC/n5C AD 5D AC 9C 7C 5H 8D TD KS")
(def large (slurp "src/projecteuler/051-100/problem-054-input.txt"))

;; A function to rank the hand
;; Return: rank & value of highest relevant card
;;
(defn parse-hand
  "determine what kind of hand (the cards=hand) are passed to the fn"
  [hand]
  (sort-by :card 
           (for [x 
                 (->> hand
                      ;; change face cards to values (ie. T=10, J=11, etc)
                      (change #"T" "10")
                      (change #"J" "11")
                      (change #"Q" "12")
                      (change #"K" "13")
                      (change #"A" "14")
                      (split #"\s"))]
             (let [[_ card suit]
                   (re-matches #"(\d+)(\w)" x)]
               {:card (Integer/parseInt card)
                :suit suit})))
)

(defn royal-flush [hand]
  (if (and
       (= (map :card hand) '(10 11 12 13 14))
       (= 1 (count (distinct (map :suit hand)))))
    {:rank 10 :value 14}
    {:rank 0 :value 0})
  )

(defn straight-flush [hand]
  (if (and
       (= 1 (count (distinct (map :suit hand))))
       (consecutive? (map :card hand)))
    {:rank 9 :value (apply max (map :card hand))}
    {:rank 0 :value 0}))

(defn four-of-a-kind [hand]
  (if (some #(= 4 %) (vals (frequencies (map :card hand))))
    {:rank 8 :value (key (apply max-key val (frequencies (map :card hand))))}
    {:rank 0 :value 0}))

(defn full-house [hand]
  (if (and
       (= 2 (count (distinct (map :card hand))))
       (some #(= 2 %) (vals (frequencies (map :card hand))))
       (some #(= 3 %) (vals (frequencies (map :card hand)))))
    {:rank 7 :value (key (apply max-key val (frequencies (map :card hand))))}
    {:rank 0 :value 0}))

(defn flush-suit [hand]
  (if (= 1 (count (distinct (map :suit hand))))
    {:rank 6 :value (apply max (map :card hand))}
    {:rank 0 :value 0}))

(defn straight [hand]
  (if (consecutive? (map :card hand))
    {:rank 5 :value (apply max (map :card hand))}
    {:rank 0 :value 0}))

(defn three-of-a-kind [hand]
  (if (some #(= 3 %) (vals (frequencies (map :card hand))))
    {:rank 4 :value (apply max (map :card hand))}
    {:rank 0 :value 0}))

(defn two-pairs [hand]
  (let [x (sort (comp - compare) (vals (frequencies (map :card hand))))]
    (if (and
         (= (first x) 2)
         (= (second x) 2))
      {:rank 3 :value (key (apply max-key val (frequencies (map :card hand))))}
      {:rank 0 :value 0})))

(defn one-pair [hand]
  (if (some #(= 2 %) (vals (frequencies (map :card hand))))
    {:rank 2 :value (key (apply max-key val (frequencies (map :card hand))))}
    {:rank 0 :value 0}))

(defn high-card [hand]
  {:rank 1 :value (apply max (map :card hand))})



(defn evaluate-hand [hand]
  "given a hand (eg. 4H 8D 9D 9C AS), it applies the hand to each of the above logics
and returns a map of highest rank {:rank 2 :value 9}"
  (let [p-hand  (parse-hand hand)]
    (apply max-key :rank 
           (map #(% p-hand) [royal-flush straight-flush four-of-a-kind full-house flush-suit straight three-of-a-kind two-pairs one-pair high-card]))
    )
  )


(def input
  (->> large
       (string/split-lines)
       flatten
       ))

(defn winner [hands]
  "compares player1's and player2's hands. returns a 1 if player 1 wins."
  (let [p1 (subs  hands 0 14)
        p2 (subs  hands 15 29)]
    (if (> (:rank (evaluate-hand p1)) (:rank (evaluate-hand p2)))
      1
      (if (= (:rank (evaluate-hand p1)) (:rank (evaluate-hand p2)))
        (if (> (:value (evaluate-hand p1)) (:value (evaluate-hand p2)))
          1
          0)
        0)
      )))


(defn problem-054 []
  (apply +
         (for [x input]
           (winner x)
           )))

(problem-054)    ;; => 376

