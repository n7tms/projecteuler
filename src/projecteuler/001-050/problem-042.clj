;; Project Euler
;; Problem 042 - Coded Triangle Numbers
;;

(ns projecteuler
  (:require [clojure.string :as string]))

(defn split [regex s]
  (str/split s regex))

(def large (slurp "src/projecteuler/001-050/problem-042-input.txt"))

(def triangle-numbers
  "lazy-sequence of triangle numbers (limited to the first 25)"
  (map (fn [num] (int (* 0.5 num (inc num)))) (range 25))
  )

(def letters "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn word-value [word]
  (reduce +
          (for [idx (range (count word))]
            (inc (string/index-of letters (str (nth word idx))))
            )))

(defn triangle? [value]
  (< 0 (count (filter #(= value %) triangle-numbers))) )


(defn problem-042 []
  (->> large
       (split #"\s")
       (map  word-value)
       (map triangle?)
       (remove false?)
       (count)
       ))

(problem-042)   ;; => 162


