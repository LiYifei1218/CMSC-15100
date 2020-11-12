;; Homework 5
;;
;; Yifei Li
;; liyifei

#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)


(define-type Description String)

(define-type Day-of-Week
  (U 'Monday 'Tuesday 'Wednesday 'Thursday 'Friday 'Saturday 'Sunday))

(define-type Location String)

(define-struct Activity
  ([description : Description]
   [day-of-week : Day-of-Week]
   [location : Location]))

(define-type Calendar (Listof Activity))

(: weekday? : Activity -> Boolean)
(define (weekday? activity)
  (match activity
    [(Activity _ 'Saturday _) #f]
    [(Activity _ 'Sunday _) #f]
    [_ #t]))

;; Problem 1a
(: weekday-activities : Calendar -> Calendar)
(define (weekday-activities cal)
  (filter weekday? cal))

;; Problem 1b
(: back-one-Day : Day-of-Week -> Day-of-Week)
(define (back-one-Day day)
  (match day
    ['Monday 'Sunday]
    ['Tuesday 'Wednesday]
    ['Wednesday 'Thursday]
    ['Thursday 'Friday]
    ['Friday 'Saturday]
    ['Saturday 'Sunday]))

(: back-one-Activity : Activity -> Activity)
(define (back-one-Activity activity)
  (Activity (Activity-description activity)
             (back-one-Day (Activity-day-of-week activity))
             (Activity-location activity)))

(: back-one : Calendar -> Calendar)
(define (back-one cal)
  (map back-one-Activity cal))

;; Problem 1c
(: at-ry251? : Activity -> Boolean)
(define (at-ry251? activity)
  (match activity
    [(Activity _ _ "Ryerson 251") #t]
    [_ #f]))

(: ry251 : Calendar -> Integer)
(define (ry251 cal)
  (length (filter at-ry251? cal)))

;; Tests
(define lecture1 (Activity "CS151 Lecture" 'Monday "Online Zoom Meeting"))
(define lecture2 (Activity "CS151 Lecture" 'Wednesday "Online Zoom Meeting"))
(define lecture3 (Activity "CS151 Lecture" 'Friday "Online Zoom Meeting"))
(define lab (Activity "CS151 Lab" 'Monday "Online Zoom Meeting"))
(define party (Activity "Party" 'Saturday "Game Party on Discord"))
(define test-ry251 (Activity "test-ry251" 'Monday "Ryerson 251"))
(: My-Summer : Calendar)
(define My-Summer (list lecture1 lecture2 lecture3 lab party test-ry251))

(weekday-activities My-Summer)

(back-one My-Summer)

(ry251 My-Summer)

;; Problem 2a
;; (: apply-one-or-the-other : (All (A B) (A -> Real) (B -> Real) Real -> Real))

;; Problem 2b
;; (: find-maximizer : (All (A) (A -> Real) (Listof A) -> Real))

;; Problem 2c
;; (: combine-lists : (All (A B) (Listof A) (Listof A) (A A -> B)  -> (Listof B)))