;; Homework 3
;;
;; Yifei Li
;; liyifei

#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

;; Problem 1
(define-struct Point
  ([x : Real]
   [y : Real]))

;; define Points
(: SR : Point)
(define SR (Point 0.3 -1.7))
(: AA : Point)
(define AA (Point 1.7 2.4))
(: RO : Point)
(define RO (Point 0.2 -0.3))
(: MKL : Point)
(define MKL (Point -0.8 1.2))

(: in-quad? : Point -> Boolean)
;; returns of the given Point is in the area of Quadrangle
(define (in-quad? point)
  (if (and (and (>= (Point-y point) -1) (<= (Point-y point) 1))
           (and (>= (Point-x point) -0.75) (<= (Point-x point) 0.75))) #t #f))

(: dist : Point Point -> Real)
;; returns the distance between given two Points
(define (dist point1 point2)
  (sqrt (+ (expt (- (Point-x point1) (Point-x point2)) 2)
           (expt (- (Point-y point1) (Point-y point2)) 2))))

(: route-dist : (Listof Point) -> Real)
;; returns the total distance of a given Listof Point
(define (route-dist list)
  (cond
    [(= (length list) 1) 0]
    [else (+ (dist (first list) (second list)) (route-dist (rest list)))]))

;; Test Begins
(in-quad? RO)
(dist RO MKL)
(: point-list : (Listof Point))
(define point-list (list SR AA RO MKL))
(route-dist point-list)
;; Test Ends

;; Problem 2

(define-type Color (U 'gray 'red 'white 'blue))
(define-type Size (U 'small 'medium 'large))
(define-struct Shirt ([short-sleeves? : Boolean]
                      [color : Color]
                      [size : Size]))
(define-struct Pants
  ([shorts? : Boolean]
   [color : Color]
   [size : Size]))
(define-type Clothing (U Shirt Pants))

(: clothing-color : Clothing -> Color)
;; returns the color of given cloth
(define (clothing-color cloth)
  (cond
    [(Shirt? cloth) (Shirt-color cloth)]
    [(Pants? cloth) (Pants-color cloth)]))

(: clothing-size : Clothing -> Size)
;; returns the size of given cloth
(define (clothing-size cloth)
  (cond
    [(Shirt? cloth) (Shirt-size cloth)]
    [(Pants? cloth) (Pants-size cloth)]))

(: same-size? : Clothing Clothing -> Boolean)
;; returns is the two given cloths are in same size, uses clothing-size function
(define (same-size? cloth1 cloth2)
  (if (symbol=? (clothing-size cloth1) (clothing-size cloth2)) #t #f))

(: cool? : Clothing -> Boolean)
;; returns if the given cloth is shorts(sleeve)
(define (cool? cloth)
  (cond
    [(Shirt? cloth) (Shirt-short-sleeves? cloth)]
    [(Pants? cloth) (Pants-shorts? cloth)]))

;; Test Begins
(: shirt1 : Shirt)
(define shirt1 (Shirt #t 'gray 'small))
(: shirt2 : Shirt)
(define shirt2 (Shirt #f 'red 'large))
(: pants1 : Pants)
(define pants1 (Pants #t 'white 'small))
(: pants2 : Pants)
(define pants2 (Pants #f 'red 'medium))

(check-expect (clothing-color shirt1) 'gray)
(check-expect (same-size? shirt1 pants1) #t)
(check-expect (cool? pants2) #f)
;; Test Ends

(test)


