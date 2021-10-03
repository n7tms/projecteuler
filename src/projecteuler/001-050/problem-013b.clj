(ns user)

(def numbers
  (slurp "/home/todd/dev/clojure/projecteuler/001-050/problem-013.txt"))


(read-string numbers)

;(->> numbers)

(filter #"(\d+)" numbers)



(reduce + (map bigint numbers))
(apply str (take 10 (str (reduce + (map bigint numbers)))))
