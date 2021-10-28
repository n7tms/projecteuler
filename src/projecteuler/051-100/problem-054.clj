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


(ns projecteuler)



(def cards "23456789TJQKA")



;; A function to rank the hand
;; Return: rank & value of cards highest to lowest
;; Would it be possible to sum the value of the cards. If both players
;; have the same rank, but the sum of player 1's cards is higher, then
;; player 1 wins (?)
;;



;; a function to compare two hands
;; which hand is winner, based on rank
;; if rank is the same, who has the next highest card



(defn consecutive? [values]
  (let [vals (sort values)]
    (empty? (remove #{1} (map - (next vals) vals)))))

(consecutive? [1 2 4 5 3]) ;; => true


