;; Project Euler
;; Problem 18 and Problem 67

;; This solution works equally well on both problems 18 and 67.
;;
;; This code is adopted from https://github.com/mishadoff/project-euler/blob/master/src/project_euler/problem018.clj
;; I wrote my own pyramid parser. His only handled two digit numbers. Mine will handle any size number.



(ns projecteuler
  (:require [clojure.string :as string]))

(def input-file-med "src/projecteuler/001-050/problem-018-input.txt")
(def input-file-small "src/projecteuler/001-050/problem-018-sample.txt")
(def input-file-large "src/projecteuler/051-100/problem-067-input.txt")

;; I need to reverse the order of the split parameters so that it works smoother with thread-last.
(defn split [regex s]
  "swaps string/split parameters of idiomatic ->> threading"
  (string/split s regex))

;; This is mishadoff's version of the triangle parser.
;; (def triangle
;;   (map #(Integer/parseInt %)(map #(reduce str %) (partition 2 2 (remove #(or (= \newline %) (= \space %))
;;     (seq (slurp input-file-med)))))))

;; My version of the triangle parser...
;; Read the file....split the numbers on whitespaces....and convert everything to an integer.
(def triangle
  (->> (slurp input-file-large)
       (split #"\s")
       (map #(Integer/parseInt %))
       ))


(def nested-triangle
  (loop [lst triangle n 1 newlist nil]
    (if (empty? lst) (reverse newlist)
      (recur (drop n lst) (inc n) (cons (take n lst) newlist)))))

(defn max-row [lst]
  (map #(reduce max %) (partition 2 1 lst)))

(defn step-max [lst1 lst2]
  (map + (max-row lst1) lst2))

;; Elapsed time: 0.421325 msecs
(defn euler-018 []
  (reduce step-max (reverse nested-triangle)))

(euler-018)
