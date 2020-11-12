;; Homework 8ret
  
;;
;; Yifei Li
;; liyifei

#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

;; Problem 1
(define-struct Grocery
([name : String]
 [days-old : Integer]))

(: count-old : (Listof Grocery) Integer -> Integer)
(define (count-old items date)
  (cond
    [(empty? items) 0]
    [else (+
           (count-old (rest items) date)
           (if (> (Grocery-days-old (first items)) date) 1 0))]))


;; T(n) = T(n - 1) + c
;;      = T(n - 1) + c
;; T(n - 1) is for the call of recursion function

;; T(0) = 2
;; T(n) = c * n
;;      = O(n)

(: count-old2 : (Listof Grocery) Integer -> Integer)
(define (count-old2 items date)
  (length (filter (lambda ([g : Grocery]) (> (Grocery-days-old g) date)) items)))

;; T(n) = n * T(1) + c
;;      = n + c
;;      = O(n)

;; Problem 2

(: find-minimum : (Listof Integer) -> Integer)
(define (find-minimum list)
  (cond
    [(empty? list) 0]
    [(foldr min (first list) list)]))

;; T(n) = n * T(c) + c
;;      = O(n)

;; (find-minimum (list 5 2 3 4))

(: remove-elem : (Listof Integer) Integer -> (Listof Integer))
(define (remove-elem lst item)
  (cond
    [(empty? lst) '()]
    [(= item (first lst)) (rest lst)]
    [else (append (list (first lst)) (remove-elem (rest lst) item))]))

;; T(n) = T(n - 1) + c
;; T(n) = n * c
;;      = O(n)


;; (remove-elem (list 2 3 7 3) 5)

(: selection-sort : (Listof Integer) -> (Listof Integer))
(define (selection-sort lst)
  (cond
    [(empty? lst) '()]
    [else (append (list (find-minimum lst)) (selection-sort (remove-elem lst (find-minimum lst))))]))

;; T(n) = T(n - 1) + T(n) + T(n) + c
;; T(n) = n * (2 * T(n) + c)
;;      = O(n ^ 2)

(: n : Integer)
(define n 10000000)

"Building list"

(: big-list : (Listof Integer))
(define big-list
  (build-list n (lambda ([i : Integer]) (random n))))

"Sorting list"

;; (empty? (selection-sort big-list)) sort 15000 elements in 10 seconds

(empty? (sort big-list <)) ;; sort 10000000 elements in 10 seconds

"Finished sorting"