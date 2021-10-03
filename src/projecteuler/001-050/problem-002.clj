(ns projecteuler.core
  (:gen-class)
  (:require [clojure.string :as str])
)


;Project Euler #2
(defn fib-seq []
  ((fn fib [a b]
     (lazy-seq (cons a (fib b (+ a b)))))
   0 1))

;(filter even? (take-while (partial > 100) (fib-seq)))

(reduce + (filter even? (take-while (partial > 4000000) (fib-seq))))
;;4613732
