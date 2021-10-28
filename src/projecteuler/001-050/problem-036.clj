;; Project Euler
;; Problem 36 - Double-base palindrome
;;

(ns projecteuler
  (:require [clojure.string :as str]))


;; iterate through numbers < 1000000
;; convert num to string and see if it is a palindrome
;; if num is palindrome, check if binary is palindrome
;; if binary is palindrome, save num
;; remove the nil's
;; add 'em all up

(defn problem-036 []
  (reduce +
          (remove nil?
                  (for [x (range 1 1000000)]
                    (if (= (str x) (apply str (reverse (str x))))
                      (if (= (Integer/toString x 2) (apply str (reverse (Integer/toString x 2))))
                        x
                        nil))))))

(problem-036)   ;; => 872187 in 1.4 seconds
