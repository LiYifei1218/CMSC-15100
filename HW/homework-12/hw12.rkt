;; Homework 12
  
;;
;; Yifei Li
;; liyifei

#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

(define-type Vertex Integer)

(define-struct Graph
  ([n : Integer]
   [adj : (Vectorof (Listof Vertex))]))

(: get-nbrs : Graph Vertex -> (Listof Vertex))
(define (get-nbrs g v)
  (vector-ref (Graph-adj g) v))

(: dfs! : Graph Vertex (Vectorof Boolean) -> Void)
(define (dfs! g v visited)
  (begin
    (vector-set! visited v #t)
    (local
      {
       (: explore-list! : (Listof Vertex) -> Void)
       (define (explore-list! nbrs)
         (cond
           [(empty? nbrs) (void)]
           [else (begin (if (vector-ref visited (first nbrs)) (void) (dfs! g (first nbrs) visited))
                        (explore-list! (rest nbrs)))]))}
    (explore-list! (get-nbrs g v)))))

;; Problem 1
(: richest-connection : Graph (Vectorof Exact-Rational) Vertex -> Vertex)
(define (richest-connection g m v)
  (local
    {
     (: visted : (Vectorof Boolean))
     (define visted (make-vector 8 #f))

     (: get-max-index : Integer Integer -> Integer)
     (define (get-max-index richest-person curr)
       (cond
         [(= (Graph-n g) curr) richest-person]
         [else (get-max-index (if (vector-ref visted curr)
                                  (if (> (vector-ref m richest-person) (vector-ref m curr))
                                      richest-person
                                      curr)
                                  richest-person)
                              (+ curr 1))]))}
    (begin
      (dfs! g v visted)
      (get-max-index v 0))))

(define test-graph (Graph 8 (vector '(1 2) '(0 4) '(0 3 4) '(2) '(1 2 7) '(6) '(5) '(4))))
(: test-money : (Vectorof Exact-Rational))
(define test-money (vector 10 50 20 10 60 20000 10000 0))
;; 0-7 coressponds to A to H

(check-expect (richest-connection test-graph test-money 0) 4)
;; In the "group" of A B C D E and H, E is the richest person with $60
(check-expect (richest-connection test-graph test-money 6) 5)
;; F is the richest person, but no one knew him except G

;; Problem 2
(: my-andmap : (All (A) (A -> Boolean) (Listof A) -> Boolean))
(define (my-andmap func lst)
  (cond
    [(empty? lst) #t]
    [(not (func (first lst))) #f]
    [else (my-andmap func (rest lst))]))

(check-expect (my-andmap positive? '(1 2 3 4)) #t)
(check-expect (my-andmap positive? '(1 2 -3 4)) #f)

;; Problem 3
(: cover-floor : Integer -> Integer)
(define (cover-floor n)
  (match n
    [0 0]
    [1 1]
    [_ (+ (+ 1 (cover-floor (- n 1))) (+ 1 (cover-floor (- n 2))) (+ 1 (cover-floor (- n 2))))]))
(cover-floor 10)
(test)

