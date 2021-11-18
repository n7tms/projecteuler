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

(def cards "23456789TJQKA")


(def player1 "8C TS KC 9H 4S")
(def player2 "2C 3S 8S 8D TD")
(def player3 "TD JD KD AD QD")

;; A function to rank the hand
;; Return: rank & value of cards highest to lowest
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


             ;; determine what kind of hand this is
)

(defn royal-flush? [hand]
  (if (and
       (= (map :card hand) '(10 11 12 13 14))
       (= 1 (count (distinct (map :suit (parse-hand player1))))))
    true
    false)
  )

(defn straight-flush? [hand]
  (if (and
       (= 1 (count (distinct (map :suit (hand)))))
       (consecutive? (map :card (hand))))
    true
    false))

(defn four-of-a-kind? [hand]
  (if (= 2 (count (distinct (map :card (hand)))))
    true
    false))

(defn full-house? [hand]
  
  )

(defn flush? [])
(defn straight? [])
(defn three-of-a-kind? [])
(defn two-pairs? [])
(defn one-pair? [])
(defn high-card? [])

(frequencies (map :card (parse-hand player1)))

(defn evaluate-hand [hand]
  (let [p-hand  (parse-hand hand)]
    (royal-flush? p-hand))
  )

(evaluate-hand player3)

  ;; a function to compare two hands
  ;; which hand is winner, based on rank
  ;; if rank is the same, who has the next highest card





(consecutive? [1 2 4 5 3]) ;; => true


