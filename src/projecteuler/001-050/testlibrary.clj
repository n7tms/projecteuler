(ns user)
(require 'mylibrary)
(refer 'mylibrary)

(last (take 10001 (primes)))
