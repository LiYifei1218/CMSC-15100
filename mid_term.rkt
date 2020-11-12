#lang typed/racket
(define-type Pitch (U 'A 'B 'C 'D 'E 'F 'G))
(define-struct Note
  ([pitch : Pitch]
   [duration : Integer]))
(define-type Slience Integer)
(define-type Component (U Note Slience))
(define-type Melody (Listof Component))

(: my-song : Melody)
(define my-song (list (Note 'C 2) 1 (Note 'D 1) (Note 'E 2)))



(: melody-length : Melody -> Integer)
(define (melody-length melody)
  (local
    {
     (: get-length : Component -> Integer)
     (define (get-length c)
  (cond
    [(Note? c) (Note-duration c)]
    [else c]))}
     (cond
    [(= 0 (length melody)) 0]
    [(+ (get-length (first melody)) (melody-length (rest melody)))])))

(melody-length my-song)


;; ====================


(define-type Arrow (U 'up 'down 'left 'right))
(define-struct Point
  ([x : Real]
   [y : Real]))

(: move-once : Arrow Point -> Point)
(define (move-once a p)
  (match a
    ['up (Point (Point-x p) (+ 1 (Point-y p)))]
    ['down (Point (Point-x p) (- (Point-y p) 1))]
    ['left (Point (- (Point-x p) 1) (Point-y p))]
    ['right (Point (+ (Point-x p) 1) (Point-y p))]))

(: move : (Listof Arrow) -> Point)
(define (move arrows)
  (cond
    [(= 0 (length arrows)) (Point 0 0)]
    [(move-once (first arrows) (move (rest arrows)))]))
(Point-x (move (list 'up 'up 'left 'down)))
(Point-y (move (list 'up 'up 'left 'down)))

;; ====================
(define-type Int-Set (Integer -> Boolean))

(: negatives : Int-Set)
(define (negatives n)
  (cond
    [(< n 0) #t]
    [else #f]))

(: less-than-100 : Int-Set)
(define (less-than-100 n)
  (cond
    [(and (<= n 100) (>= n -100)) #t]
    [else #f]))

(:  complement :  Int-Set -> Int-Set)
(define (complement set)
  (local
    {
     (: neg : Int-Set)
     (define (neg n)
       (not (set n)))}
    neg))

(: find-int : Int-Set -> Integer)
(define (find-int set)
  (local
    {
     (: n : Integer)
     (define n (random -2147483543 2147483544))}
     (if (set n) n (find-int set))))

(: test : Int-Set)
(define (test n)
  (cond
    [(= n -10) #t]
    [else #f]))

;; (find-int test)


;; ====================

;; (: length : (All (A) (Listof A) -> Integer))
;; (: foldl : (All (A B) (A B -> B) B (Listof A) -> B))
;; (: map : (All (A B) (A -> B) (Listof A) -> (Listof B)))

;; ====================
(: func : Integer -> Integer)
(define (func n)
  (match n
    [0 0]
    [1 2]
    [_ (+ (func (- n 1)) (* 2 (func (- n 2))))]))

(func 0)
(func 1)
(func 2)
(func 3)

;; ====================

(: count-even : (Listof Integer) -> Integer)
(define (count-even l)
  (cond
    [(zero? (length l)) 0]
    [(= (modulo (first l) 2) 0) (+ 1 (count-even (rest l)))]
    [else (count-even (rest l))]))

(count-even (list 2 7 8 3))

;; ====================
(: my-foldl : (All (A B) (A B -> B) B (Listof A) -> B))
(define (my-foldl func init list)
  (cond
    [(zero? (length list)) init]
    [else (my-foldl func (func (first list) init) (rest list))]))

(foldl string-append "" (list "h" "e" "l" "l" "o"))
(my-foldl string-append "" (list "h" "e" "l" "l" "o"))

;; ====================
(: stairs : Integer -> Integer)
(define (stairs n)
  (local
    {
     (: part : Integer Integer -> Integer)
     (define (part n m)
       (cond
         [(= n m) (+ 1 (part n (- m 1)))]
         [(or (zero? m) (< n 0)) 0]
         [(zero? n) 1]
         [else (+ (part n (- m 1)) (part (- n m) m))]))}
    (part n 3)))


(stairs 4)
(stairs 5)