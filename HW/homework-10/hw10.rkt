;; Homework 10
  
;;
;; Yifei Li
;; liyifei

#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)


(: add-one-helper! : (Vectorof Integer) Integer -> Void)
(define (add-one-helper! vec index)
  (cond
    [(= index (vector-length vec)) (void)]
    [else (begin (vector-set! vec index (+ (vector-ref vec index) 1)) (add-one-helper! vec (+ index 1)))]))

;; add-one-helper!: T(n) = T(n - 1) + C
;;                       = O(n)

(: add-one! : (Vectorof Integer) -> Void)
(define (add-one! vec)
  (add-one-helper! vec 0))

;; add-one: T(n) = T(n)(add-one-helper!)
;;               = O(n)

(: vec : (Vectorof Integer))
(define vec (vector 1 2 3 4))

(add-one! vec)

(check-expect vec (vector 2 3 4 5))
(test)