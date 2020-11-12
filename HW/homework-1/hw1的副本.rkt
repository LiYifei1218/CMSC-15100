#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

;; problem 1
;; Volume of a sphere
;; volume = 4/3*pi*r*r*r

( : volume : Real -> Real)
( define (volume radius)
   ( * (/ 4 3) pi radius radius radius))

(check-within (volume 1) 4.1887 0.0001)
;; problem 2
;; count legs
;; no of legs = 4*cows + 2*humans
( : count-legs : Integer Integer -> Integer)
( define (count-legs humans cows)
   (+ (* 2 humans) (* 4 cows)))

(check-expect (count-legs 1 1) 6)
   
(test)