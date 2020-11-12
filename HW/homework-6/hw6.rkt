;; Homework 6
;;
;; Yifei Li
;; liyifei

#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

;; Problem 1a
(define-type TV (Integer -> String))
(: friday-night : TV)
(define (friday-night channel)
  (match channel
    [5 "NBC Evening News"]
    [7 "Game of Thrones"]
    [33 "ESPN SportsCenter"]
    [_ (error "wrong channel number")]))

(check-expect (friday-night 5) "NBC Evening News")

;; Problem 1b
(: change-program : TV Integer String -> TV)
(define (change-program tv chan program)
  (local
    {
     (: out : TV)
     (define (out new-ch)
       (if (= chan new-ch)
           program
           (tv new-ch)))}
    out))

(check-expect ((change-program friday-night 5 "test") 5) "test")
(check-error ((change-program friday-night 50 "test2") 40) "wrong channel number")

;; Problem 2
(: duplicate-string : String Integer -> String)
(define (duplicate-string str n)
  (foldr string-append "" (make-list n str)))

(check-expect (duplicate-string "ha" 3) "hahaha")

;; Problem 3
(: list-0-to-n : Integer -> (Listof Integer))
(define (list-0-to-n n)
  (build-list (+ n 1) +))

(: my-sqrt : Integer -> Integer)
(define (my-sqrt n)
  (local
    {
     (: judge : Integer -> Boolean)
     (define (judge x)
       (<= (* x x) n))}
    (if (> n 0) (foldr max 0 (filter judge (list-0-to-n n))) (error "input not positive"))))

(: my-expt : Real Integer -> Real)
(define (my-expt base power)
  (cond
    [(< power 0) (error "input not positive")]
    [(= power 0) 1]
    [(= power 1) base]
    [else (* base (my-expt base (- power 1)))]))

(: my-nth-root : Integer Integer -> Integer)
(define (my-nth-root n x)
  (local
    {
     (: judge : Integer -> Boolean)
     (define (judge a)
       (<= (my-expt a x) n))}
    (if (> n 0) (foldr max 0 (filter judge (list-0-to-n n))) (error "input not positive"))))


(check-expect (my-sqrt 4) 2)
(check-error (my-sqrt -4) "input not positive")
(check-expect (my-nth-root 8 3) 2)

;; Problem 4
(define-struct (Tree A)
([value : A]
 [left-child : (U 'none Tree)]
 [right-child : (U 'none Tree)]))

(: valid-bracket? : (Tree String) -> Boolean)
(define (valid-bracket? tree)
  (local
    {
     (: left : (U (Tree String) 'none))
     (define left (Tree-left-child tree))
     (: right : (U (Tree String) 'none))
     (define right (Tree-right-child tree))}
    (cond
      [(symbol? tree) #t]
      [(or (not (Tree? left)) (not (Tree? right)) #f)]
      [(or (string=? (Tree-value tree) (Tree-value left))
           (string=? (Tree-value tree) (Tree-value right))
           (and (valid-bracket? left) (valid-bracket? right)))]
      [else #f])))

(test)