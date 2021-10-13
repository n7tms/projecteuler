(ns projecteuler)

;; Project Euler
;; Problem 17
;;
;; Number Letter Counts
;;
;; If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

;; If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

;; NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen)
;; contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
;;



(def number-words ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"
                   "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen" "sixteen" "seventeen" "eighteen" "nineteen"
                   "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"
                   "hundred" "hundredand" "thousand"])


(quot 324 1000) ;; => 0  (There is no thousand's digit)
(quot 324 100)  ;; => 3  (Returns the magnitude of the 100's place)
(quot  24  10)  ;; => 2  (Returns the magnitude of the 10's place)

(mod 324 100)   ;; => 24 (Trims off the hundreds place)
(mod 324 10)    ;; =>  4 (Trims off hundreds and tens, leaving the one's)
(mod 324 1)     ;; =>  0
;;(keyword (str 50))  ;; => :50   (Can I use this to reference the number-words...if I keyword-ize the list?)

;; ==================================================
(def x  324)
(defn num-to-word
  "convert a number to a word, e.g. 342 => three hundred and forty-two"
  ([num]
   (num-to-word "" num))

  ([so-far num]
   (if (zero? num) so-far
       (if (not= (quot num 1000) 0)
         (num-to-word (str so-far (number-words (quot num 1000)) (number-words 30)) (mod num 1000))  ;; trim 1000's, call with 100's
         (if (not= (quot num 100) 0)
           (if (zero? (mod num 100))
             (num-to-word (str so-far (number-words (quot num 100)) (number-words 28)) (mod num 100))  ;; 100 exactly ... trim 100's, call 10's
             (num-to-word (str so-far (number-words (quot num 100)) (number-words 29)) (mod num 100))  ;; trim 100's, call 10's
)
           (if (not= (quot num 10) 0)
             (if (<= 10 num 19)
               (str so-far (number-words num)) ;; handle 'teen numbers
               (num-to-word (str so-far  (number-words (+ 20 (- (quot num 10) 2)))) (mod num 10))) ;; trim 10's, call 1's
             (str so-far (number-words num))
             )
         )))))


(loop [x 1 acc 0]
  (if (= x 1001)
    acc
    (recur (inc x) (+ acc (count (num-to-word x)))))
)

(num-to-word 1)

(count (loop [x 1 acc ""]
         (if (= x 1001)
           acc
           (recur (inc x) (str acc (num-to-word x))))
         ))  ;; => 21124

;; There is probably a pattern in there somewhere, but brute-force seemed to work just fine here.

