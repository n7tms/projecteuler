;; Project Euler
;; Problem 22: Names Scores
;;
;; https://projecteuler.net/problem=22
;;
;; Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing
;; over five-thousand first names, begin by sorting it into alphabetical order. Then working 
;; out the alphabetical value for each name, multiply this value by its alphabetical position 
;; in the list to obtain a name score.
;; 
;; For example, when the list is sorted into alphabetical order, COLIN, which is worth 
;; 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score 
;; of 938 × 53 = 49714.
;;
;; What is the total of all the name scores in the file?
;;

(ns projecteuler
  (:require [clojure.string :as str]
))

(defn split [regex s]
  "swaps string/split parameters of idiomatic ->> threading"
  (str/split s regex))

;; I previously massaged the input file to remove quotes and put the names
;; on their own line.
(def input-file "src/projecteuler/001-050/problem-022-input.txt")

(def letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(def small "MARY
PATRICIA
LINDA
BARBARA
ELIZABETH
")

(def large (slurp input-file))

(def names (sort (str/split-lines large)))

(defn worth [word]
  (reduce + (for [x word]
              (inc (str/index-of letters x)))))

(worth "BARBARA")   ;; => 43  * 1  => 43
(worth "ELIZABETH") ;; => 88  * 2  => 176  
(worth "LINDA")     ;; => 40  * 3  => 120
(worth "MARY")      ;; => 57  * 4  => 228
(worth "PATRICIA")  ;; => 77  * 5  => 385    ==>> 952


(defn problem-22 []
  (reduce + (for [i (range (count names))]
              (* (inc i) (worth (nth names i)))
              )))

(problem-22)   ;; => 871198282

