#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

(define-type Description String)
(define-type Day-of-Week (U 'Monday 'Tuesday 'Wednesday 'Thursday 'Friday 'Saturday 'Sunday))

(define-type Location String)

(define-struct Activity
  ( [desc : Description]
    [day  : Day-of-Week]
    [location : String]))

(define-type Calendar (Listof Activity))

(define activity1 (Activity "Class1" 'Monday "Ry251"))
(define activity2 (Activity "Class2" 'Wednesday "Ry251"))
(define activity3 (Activity "Class3" 'Friday "Ry251"))
(define activity4 (Activity "Class4" 'Wednesday "CSIL"))

(define back-activity1 (Activity "Class1" 'Sunday "Ry251"))
(define back-activity2 (Activity "Class2" 'Tuesday "Ry251"))
(define back-activity3 (Activity "Class3" 'Thursday "Ry251"))
(define back-activity4 (Activity "Class4" 'Tuesday "CSIL"))

(define cs151 (list activity1 activity2 activity3 activity4))
(define back-cs151 (list back-activity1 back-activity2 back-activity3 back-activity4))


(: weekday? : Activity -> Boolean)
(define (weekday? activity)
  ( not (or (symbol=? (Activity-day activity) 'Sunday) (symbol=? (Activity-day activity) 'Saturday))))

(: weekday-activities : Calendar -> Calendar)
(define (weekday-activities cal)
  (filter weekday? cal))

(check-expect (weekday-activities cs151) cs151)

(: back-a-day : Day-of-Week -> Day-of-Week )
(define (back-a-day day)
  (match day
    ['Sunday 'Saturday]
    ['Monday 'Sunday ]
    ['Tuesday 'Monday ]
    ['Wednesday 'Tuesday]
    ['Thursday 'Friday]
    ['Friday 'Thursday]
    ['Saturday 'Friday]
    ['Sunday 'Saturday]))

(: back-one-activity : Activity -> Activity)
( define (back-one-activity ac)
   (Activity (Activity-desc ac) (back-a-day (Activity-day ac)) (Activity-location ac)))

(: back-one-calendar : Calendar -> Calendar )
( define (back-one-calendar cal)
   (map back-one-activity cal))

(check-expect (back-one-calendar cs151) back-cs151)

(: in-ry251? : Activity -> Boolean)
;; returns whether or not an activity takes place in Ryerson 251
(define (in-ry251? act)
  (string=? "Ryerson 251" (Activity-location act)))

(: ry251 : Calendar -> Integer)
;; counts the number of activities that take place in Ryerson 251
(define (ry251 cal)
  (length (filter in-ry251? cal)))



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


(: valid-bracket1? : (Tree String) -> Boolean)
;; takes a bracket and tests for validity

(define (valid-bracket1? t)
  (match t
    [(Tree name1  (Tree _ _ _) 'none) #f]
    [(Tree name1 (Tree name2 _ _) (Tree name3 _ _)) (string=? name1 name2)]))

(test)