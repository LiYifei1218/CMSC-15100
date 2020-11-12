;; Homework 2
;;
;; Yifei Li
;; liyifei

#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

;; Problem 2

(: score->letter : Exact-Rational -> Symbol)
(define (score->letter score)
  (cond
    [(>= score 90) 'A]
    [(>= score 80) 'B]
    [(>= score 70) 'C]
    [(>= score 60) 'D]
    [(>= score 0) 'F]
    [else (error "score->letter: input not positive")]))

(check-expect (score->letter 98) 'A)
(check-expect (score->letter 86) 'B)
(check-expect (score->letter 77) 'C)
(check-expect (score->letter 62) 'D)
(check-expect (score->letter 45) 'F)
(check-error (score->letter -10) "score->letter: input not positive")

;; Problem 3

(: cat-string : Integer -> String)
(define (cat-string num)
  (cond
    [(= num 0) ""]
    [(= num 1) "there is 1 cat"]
    [(> num 1) (string-append "there are " (number->string num) " cats")]
    [else (error "how-many-animals: input not positive")]))

(: dog-string : Integer -> String)
(define (dog-string num)
  (cond
    [(= num 0) ""]
    [(= num 1) "there is 1 dog"]
    [(> num 1) (string-append "there are " (number->string num) " dogs")]
    [else (error "how-many-animals: input not positive")]))

(: how-many-animals : Integer Integer -> String)
(define (how-many-animals cats dogs)
  (cond
    [(and (= cats 0) (= dogs 0)) "there are no animals"]
    [(= dogs 0) (cat-string cats)]
    [(= cats 0) (dog-string dogs)]
    [else (string-append (cat-string cats) " and " (dog-string dogs))]))

(check-expect (how-many-animals 2 3) "there are 2 cats and there are 3 dogs")
(check-expect (how-many-animals 1 3) "there is 1 cat and there are 3 dogs")
(check-expect (how-many-animals 0 10) "there are 10 dogs")
(check-expect (how-many-animals 0 0) "there are no animals")
(check-error (how-many-animals -1 -1) "how-many-animals: input not positive")

;; Problem 4

(: my-expt : Real Integer -> Real)
(define (my-expt base power)
  (cond
    [(< power 0) (error "my-expt: input not positive")]
    [(= power 0) 1]
    [(= power 1) base]
    [else (* base (my-expt base (- power 1)))]))

(check-expect (my-expt 2 3) 8)
(check-expect (my-expt 1 3) 1)
(check-expect (my-expt 2 0) 1)
(check-error (my-expt 2 -1) "my-expt: input not positive")

(: sum-of-powers : Real Integer -> Real)
(define (sum-of-powers num power)
  (cond
    [(= num 0) num]
    [else (+ (my-expt num power) (sum-of-powers (- num 1) power))]))

(check-expect (sum-of-powers 4 2) 30)
(check-expect (sum-of-powers 5 1) 15)

;; Problem 5

(: my-length : (Listof Number) -> Integer)
(define (my-length num-list)
  (if (empty? (rest num-list))
      1
      (+ 1 (my-length (rest num-list)))))

(: my-list : (Listof Number))
(define my-list (list 97 -1 5 12 6))

(check-expect (my-length my-list) 5)

;; Problem 6

(: theirs-not-mine : (Listof Boolean) (Listof Boolean) -> Integer)
(define (theirs-not-mine mine theirs)
  (cond
    [(and (empty? (rest mine)) (empty? (rest theirs))) 1]
    
    [(or (empty? (rest mine)) (empty? (rest theirs))) (error "theirs-not-mine: length not same")]
    [(and (not (first mine)) (first theirs)) (+ 1 (theirs-not-mine (rest mine) (rest theirs)))]
    [else (theirs-not-mine (rest mine) (rest theirs))]))

(: mine : (Listof Boolean))
(define mine (list #t #t #f #f #f #t #t #f #f #f))
(: theirs : (Listof Boolean))
(define theirs (list #t #t #f #t #t #t #t #t #t #t))
(: theirs2 : (Listof Boolean))
(define theirs2 (list #t #t #f #t #t #t #t #t #t #t #t #t #t #t))

(check-expect (theirs-not-mine mine theirs) 5)
(check-error (theirs-not-mine mine theirs2) "theirs-not-mine: length not same")

(test)