;; Homework 4
;;
;; Yifei Li
;; liyifei

#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

;; Problem 1

;; a
;; the description of the event should be a String, for example "Meet with Sam"
(define-type Description String)
;; because there are only seven days in a week and Symbol is easier to compare and recgonize
(define-type Day-of-Week
  (U 'Monday 'Tuesday 'Wednesday 'Thursday 'Friday 'Saturday 'Sunday))
;; the location should be a String too, like "No.8 Maple Street"
(define-type Location String)
;; according to the question description, the type Activity contains description, day-of-week and location
(define-struct Activity
  ([description : Description]
   [day-of-week : Day-of-Week]
   [location : Location]))
;; the Calender is a list of Activity
(define-type Calendar (Listof Activity))

;; b
(: lecture1 : Activity)
(: lecture2 : Activity)
(: lecture3 : Activity)
(: lab : Activity)
(define lecture1 (Activity "CS151 Lecture" 'Monday "Online Zoom Meeting"))
(define lecture2 (Activity "CS151 Lecture" 'Wednesday "Online Zoom Meeting"))
(define lecture3 (Activity "CS151 Lecture" 'Friday "Online Zoom Meeting"))
(define lab (Activity "CS151 Lab" 'Monday "Online Zoom Meeting"))
(: My-Summer : Calendar)
(define My-Summer (list lecture1 lecture2 lecture3 lab))

;; c
(: weekend? : Activity -> Boolean)
(define (weekend? activity)
  (match activity
    [(Activity _ 'Saturday _) #t]
    [(Activity _ 'Sunday _) #t]
    [_ #f]))

;; test start
(define party (Activity "Party" 'Saturday "Game Party on Discord"))
(check-expect (weekend? lecture1) #f)
(check-expect (weekend? party) #t)
;; test end

;; Problem 2
(define-struct Pine-Tree
([seed : (U 'nothing Pine-Cone)]
 [height : Real]))

(define-struct Pine-Cone
([grows-into : (U 'nothing Pine-Tree)]
 [number-of-bristles : Integer]))

;; a
(: seed-bristles : Pine-Tree -> Integer)
(define (seed-bristles tree)
  (match (Pine-Tree-seed tree)
    ['nothing 0]
    [(Pine-Cone _ bristles) bristles]))

;; b
(: germinate : Pine-Tree -> Pine-Tree)
(define (germinate tree)
  (match tree
    [(Pine-Tree 'noting _) (Pine-Tree (Pine-Cone 'noting 40) (Pine-Tree-height tree))]
    [_ tree]))

;; c
(: max-tree-descendant-height : Pine-Tree -> Real)
(define (max-tree-descendant-height tree)
  (cond
    [(symbol? (Pine-Tree-seed tree)) (Pine-Tree-height tree)]
    [else (max (Pine-Tree-height tree) (max-cone-descendant-height (Pine-Tree-seed tree)))]))
(: max-cone-descendant-height : Pine-Cone -> Real)
(define (max-cone-descendant-height cone)
  (cond
    [(symbol? (Pine-Cone-grows-into cone)) 0]
    [else (max-cone-descendant-height cone)]))

;; Problem 3

(define-struct Succ
  ([nat : Nat]))
(define-type Nat
  (U 'zero Succ))

(: pred : Nat -> Nat)
(define (pred nat)
(match nat
  [’zero (error "pred: input is zero")]
  [(Succ m) m]))

(: sub : Nat Nat -> Nat)
(define (sub nat-left nat-right)
  (match nat-right
    [’zero nat-left]
    [(Succ m) (sub (pred nat-left) m)]))

;; a
(: my-gte : Nat Nat -> Boolean)
(define (my-gte n1 n2)
  (cond
    [(symbol? n1) #f]
    [(symbol? n2) #t]
    [else (my-gte (Succ-nat n1) (Succ-nat n2))]))

;; b
(: my-odd : Nat -> Boolean)
(define (my-odd n)
  (cond
    [(symbol? n) #t]
    [(not (symbol? n)) #f]
    [else (my-odd (sub n (Succ (Succ 'zero))))]))

;; c
(: my-quotient : Nat Nat -> Nat)
(define (my-quotient n1 n2)
  (cond
    [(my-gte n1 n2) (my-quotient (sub n1 n2) n2)]
    [else n1]))

(test)