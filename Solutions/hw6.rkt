#lang typed/racket

(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

;;; Problem 1

(define-type TV (Integer -> String))

;;; 1a
(: friday-night : TV)
(define (friday-night chan)
  (match chan
    [5 "NBC Evening News"]
    [7 "Game of Thrones"]
    [33 "ESPN SportsCenter"]
    [_ (error "friday-night: no program on channel")]))
;; I think it's fine if people don't use the standard error format,
;; (error "function-name: error-description")

;;; 1b

(: change-program : TV Integer String -> TV)
(define (change-program tv chan program)
  (local
    {
     (: new-tv : TV)
     (define (new-tv new-chan)
       (if (= new-chan chan)
           program
           (tv new-chan)))}
    new-tv))

;; Problem 2

(: duplicate-string : String Integer -> String)
(define (duplicate-string s n)
  (local
    {
     (: constant-s : Integer -> String)
     (define (constant-s n) s)}
  (foldr string-append "" (build-list n constant-s))))

;; Problem 3

;;; 3a
(: int-id : Integer -> Integer)
(define (int-id n) n)

(: list-0-to-n : Integer -> (Listof Integer))
(define (list-0-to-n n)
  (build-list (+ n 1) int-id))

;;; 3b
(: my-sqrt : Integer -> Integer)
(define (my-sqrt x)
  (if (< x 0) (error "my-sqrt: negative input")
  (local
    {
     (: lte-sqrt-x : Integer -> Boolean)
     (define (lte-sqrt-x n) (<= (* n n) x))}
    (foldr max 0 (filter lte-sqrt-x (list-0-to-n x))))))

;;; 3c
(: my-expt : Integer Integer -> Integer)
(define (my-expt n k)
  (cond
    [(= k 0) 1]
    [else (* n (my-expt n (- k 1)))]))

(: my-kth-rt : Integer Integer -> Integer)
(define (my-kth-rt x k)
  (if (< x 0) (error "my-higher-rt : negative input")
  (local
    {
     (: lte-kth-rt-x : Integer -> Boolean)
     (define (lte-kth-rt-x n) (<= (expt n k) x))}
    (foldr max 0 (filter lte-kth-rt-x (list-0-to-n x))))))

;;; 3d
;; idea: use my-sqrt on 10^{2m} * x for a large enough m,
;; then divide the output by 10^m

(: neg-log10 : Real -> Integer)
;; compute the necessary m
(define (neg-log10 t)
  (cond
    [(>= t 1) 0]
    [else (+ 1 (neg-log10 (* t 10)))]))

(: approx-rt : Integer Real -> Exact-Rational)
(define (approx-rt x t)
  (local
    {
     (: m : Integer)
     (define m (neg-log10 t))
     (: ten-to-m : Integer)
     (define ten-to-m (my-expt 10 m))}
    (/ (my-sqrt (* x ten-to-m ten-to-m)) ten-to-m)))

;;valid-bracket

(define-struct (Tree A)
  ([value : A]
   [left-child : (U 'none (Tree A))]
   [right-child : (U 'none (Tree A))]))

(: valid-bracket? : (Tree String) -> Boolean)
;; takes a bracket and tests for validity
(define (valid-bracket? node)
  (local
    {
     (: left : (U (Tree String) 'none))
     (define left (Tree-left-child node))
     (: right : (U (Tree String) 'none))
     (define right (Tree-right-child node))}
    (cond
      [(and (symbol? left) (symbol? right)) #t]
      [(or (not (Tree? left)) (not (Tree? right))) #f]
      [ (or
         (string=? (Tree-value node) (Tree-value right))
         (string=? (Tree-value node) (Tree-value left)))
        (and (valid-bracket? left) (valid-bracket? right))]
      [else #f])))

(check-expect (valid-bracket? (Tree "Brazil"
                                          (Tree "Spain"
                                                'none
                                                'none)
                                          (Tree "Peru"
                                                'none
                                                'none))) #f)
(check-expect (valid-bracket? (Tree "Brazil"
                                          (Tree "Brazil"
                                                (Tree "Brazil"
                                                      'none
                                                      'none)
                                                (Tree "Argentina"
                                                      'none
                                                      'none))
                                          (Tree "Peru"
                                                (Tree "Peru"
                                                      'none
                                                      'none)
                                                (Tree "France"
                                                      'none
                                                      'none)))) #t)
(define t (Tree "Brazil"
                                          (Tree "Brazil"
                                                (Tree "Brazil"
                                                      'none
                                                      'none)
                                                (Tree "Argentina"
                                                      'none
                                                      'none))
                                          (Tree "Peru"
                                                (Tree "Peru"
                                                      'none
                                                      'none)
                                                (Tree "France"
                                                      'none
                                                      'none))))
