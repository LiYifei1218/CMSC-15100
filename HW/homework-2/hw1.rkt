;; Yifei Li 2020-06-25
#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

;; problem 1
;; (a)
(sqrt (- 151 (* 7 20)))

;; (b)
(/ (* G m M) (* R R))

;; (c)
(if (>= (- (* b b) (* 4 a c)) 0) 2 0)


;; problem 2
;; (a)
( : average-3 : Real Real Real -> Real)

;; (b)
( : even-numerator? : Exact-Rational -> Boolean)

;; (c)
( : chicago-string : String String -> String)


;; problem 3
( : c->f : Exact-Rational -> Exact-Rational)
( define (c->f celsius)
   (+ (* (/ 9 5) celsius) 32))

( : f->c : Exact-Rational -> Exact-Rational)
( define (f->c fahrenheit)
   (/ (- fahrenheit 32) (/ 9 5)))


;; problem 4
( : not-a-percentage? : Real -> Boolean)
( define (not-a-percentage? input)
   (if (or (< input 0) (> input 100)) #t #f))


;; problem 5
( : enough-money? : Exact-Rational Exact-Rational Exact-Rational -> Boolean)
( define (enough-money? balance price tax)
   (if (>= (- balance (* price (+ 1 tax))) 50) #t #f))
;;(enough-money? 157 100 (/ 7 100))


;; problem 6
( : lemons : Integer Integer -> Integer)
( define (lemons adults children)
   (exact-ceiling (+ (* adults (/ 1 3)) (* children (/ 1 4)))))

( : more-lemons : Integer Integer Integer -> Integer)
( define (more-lemons adults children temperature)
   (if (>= temperature 85) (exact-ceiling (+ (* adults (/ 1 2)) (* children (/ 1 3)))) (exact-ceiling (+ (* adults (/ 1 3)) (* children (/ 1 4))))))
;;(more-lemons 4 3 85)