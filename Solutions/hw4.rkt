#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

(define-type Description String)
(define-type Day-of-Week (U 'Monday 'Tuesday 'Wednesday 'Thursday 'Friday 'Saturday 'Sunday))

(define-type Location String)

(define-struct Activity
  ( [desc : Description]
    [day  : Day-of-Week]
    [location : String])
   )

(define-type Calendar (Listof Activity))

(define activity1 (Activity "Class1" 'Monday "Ry251"))
(define activity2 (Activity "Class2" 'Wednesday "Ry251"))
(define activity3 (Activity "Class3" 'Friday "Ry251"))
(define activity4 (Activity "Class4" 'Wednesday "CSIL"))

(define CS151 (list activity1 activity2 activity3 activity4))

(: weekend? : Activity -> String)
(define (weekend? actv)
  (match (Activity-day actv)
    ['Sunday "It is on weekend"]
    ['Saturday "It is on weekend"]
    [_ "It is not on weekend"]))

(check-expect (weekend? (Activity "A" 'Monday "Ry34")) "It is not on weekend")
(check-expect (weekend? (Activity "B" 'Sunday "CSIL")) "It is on weekend")

(define-struct Pine-Tree
([seed : (U 'nothing Pine-Cone)]
[height : Real]))

(define-struct Pine-Cone
([grows-into : (U 'nothing Pine-Tree)]
[number-of-bristles : Integer]))


(: seed-bristles : Pine-Tree -> Integer)
;;Outputs the number of bristles in a Pine tree
(define (seed-bristles pine-t)
  (match (Pine-Tree-seed pine-t)
    ['nothing 0]
    [(Pine-Cone grows-into number-of-bristles)
     (if (Pine-Tree? grows-into)
                   (+ number-of-bristles (seed-bristles grows-into))
                   number-of-bristles
                   )]))

(: seed-bristles1 : Pine-Tree -> Integer)
(define (seed-bristles1 pine-t)
  (match (Pine-Tree-seed pine-t)
    ['nothing 0]
    [(Pine-Cone grows-into number-of-bristles)
     number-of-bristles]))

(define empty-pine-tree (Pine-Tree 'nothing 20))
(define pine-cone1 (Pine-Cone 'nothing 10))
(define pine-tree1 (Pine-Tree pine-cone1 20))
(define pine-cone2 (Pine-Cone pine-tree1 30))
(define pine-tree2 (Pine-Tree pine-cone2 10))

(check-expect (seed-bristles empty-pine-tree) 0)
(check-expect (seed-bristles pine-tree1) 10)
(check-expect (seed-bristles pine-tree2) 40)

(: germinate : Pine-Tree -> Pine-Tree )
(define (germinate pine-tree)
  (cond
    [(Pine-Cone? Pine-Tree-seed) pine-tree]
    [else (Pine-Tree (Pine-Cone 'nothing 10) (Pine-Tree-height pine-tree))]))

(check-expect (germinate empty-pine-tree) pine-tree1)
(check-expect (germinate pine-tree1) pine-tree1)



(: max-tree-descendant-height : Pine-Tree -> Real)
(define (max-tree-descendant-height tree)
  (cond
    [(symbol? (Pine-Tree-seed tree)) (Pine-Tree-height tree)]
    [else (max (Pine-Tree-height tree) (max-cone-descendant-height (Pine-Tree-seed tree)))]))

(: max-cone-descendant-height : Pine-Cone -> Real)
(define (max-cone-descendant-height cone)
  (cond
    [ (symbol? (Pine-Cone-grows-into cone)) 0]
    [else (max-tree-descendant-height (Pine-Cone-grows-into cone))]))


(define-struct Succ
([nat : Nat]))
(define-type Nat (U 'zero Succ))

(: my-gte : Nat Nat -> Boolean)
( define (my-gte number1 number2)
   (match number2
     [ 'zero #t]
     [(Succ n) (if (symbol? number1)
                   #f
                   (my-gte (Succ-nat number1) n))]))

(check-expect (my-gte (Succ (Succ (Succ 'zero))) (Succ (Succ 'zero))) #t)
(check-expect (my-gte (Succ (Succ 'zero)) (Succ (Succ (Succ 'zero)))) #f)

(:  pred :  Nat -> Nat)
(define (pred nat)
  (match nat
    ['zero (error "pred:  input is zero")]
    [(Succ m) m]))


(:  sub :  Nat Nat -> Nat)
(define (sub nat-left nat-right)
      (match nat-right
        ['zero nat-left]
        [(Succ m) (sub (pred nat-left) m)]))


(: my-odd? : Nat -> Boolean)
(define (my-odd? number1)
  (match number1
    ['zero #f]
    [(Succ 'zero) #t]
    [(Succ (Succ m))  (my-odd? m)])) 

(check-expect (my-odd? (Succ (Succ (Succ 'zero)))) #t)
(check-expect (my-odd? (Succ (Succ 'zero))) #f)

(: my-quotient : Nat Nat -> Nat )
( define (my-quotient num1 num2 )
   ( cond
      [(not (my-gte num1 num2)) 'zero]
      [else (Succ (my-quotient (sub num1 num2) num2))]))


(check-within (sqrt 2) 1.414 0.001)

(test)  