#lang typed/racket

(require "../include/BST.rkt")
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

;;;;;; Problem 1
(: add-one! : (Vectorof Integer) -> Void)
;; modifies the input vector by adding 1 to every element
(define (add-one! vec)
  (local
    {
     (: add-one-helper! : (Vectorof Integer) Integer -> Void)
     (define (add-one-helper! vec i)
       (cond
         [(= i (vector-length vec)) (void)]
         [else (begin (vector-set! vec i (+ (vector-ref vec i) 1))
                      (add-one-helper! vec (+ i 1)))]))}
    (add-one-helper! vec 0)))

