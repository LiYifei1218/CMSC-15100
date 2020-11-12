;; Homework 6
;;
;; Yifei Li
;; liyifei

#lang typed/racket

(require "../include/cs151-core.rkt")
(require "../include/cs151-universe.rkt")
(require "../include/cs151-image.rkt")
(require typed/test-engine/racket-tests)


;;;;; data definitions for Lab 2
(define-type Color (U 'red 'black))

(define-struct Loc
  ([row : Integer]
   [col : Integer]))

(define-struct Piece
  ([color : Color]
   [loc : Loc]))

(define-struct Checkers
  ([pieces : (Listof Piece)]
   [turn : Color]
   [clicked-piece : (U 'none Piece)]))

(: pieces-in-row : Integer Color Integer -> (Listof Piece))
;; Output a list of 4 pieces of the given color in the given row
;; This is a helper function to define starting-board
(define (pieces-in-row row c offset)
  (list
   (Piece c (Loc row (+ 1 offset)))
   (Piece c (Loc row (+ 3 offset)))
   (Piece c (Loc row (+ 5 offset)))
   (Piece c (Loc row (+ 7 offset)))))

(: starting-pieces : (Listof Piece))
(define starting-pieces
  (append
   (pieces-in-row 1 'red 1)
   (pieces-in-row 2 'red 0)
   (pieces-in-row 3 'red 1)
   (pieces-in-row 6 'black 0)
   (pieces-in-row 7 'black 1)
   (pieces-in-row 8 'black 0)))

(: starting-board : Checkers)
(define starting-board (Checkers starting-pieces 'red 'none))

(: switch-color : Color -> Color)
(define (switch-color c)
  (match c
    ['red 'black]
    ['black 'red]))
  

(: click-piece : Piece Checkers -> Checkers)
(define (click-piece p c)
  (Checkers (remove p (Checkers-pieces c)) (Piece-color p) p))

(: place-piece : Loc Checkers -> Checkers)
(define (place-piece l c)
  (Checkers (append (list (Piece (Checkers-turn c) l)) (Checkers-pieces c))
            (Checkers-turn c)
            'none))

(: loc=? : Loc Loc -> Boolean)
(define (loc=? l1 l2)
  (cond
    [(and (= (Loc-row l1) (Loc-row l2)) (= (Loc-col l1) (Loc-col l2))) #t]
    [else #f]))

(: get-piece-or-none : Loc Checkers -> (U 'none Piece))
(define (get-piece-or-none l c)
  (local
    {
     (: pieces-from-list : (Listof Piece))
     (define pieces-from-list
       (filter (lambda ([p : Piece]) (loc=? (Piece-loc p) l))
               (Checkers-pieces c)))}
    (if (= (length pieces-from-list) 0) 'none (first pieces-from-list))))

;; (start-game)


;;;;; creating a piece

(: add-piece : Checkers Color Loc -> Checkers)
;; add a piece to the board at the given location
(define (add-piece game-state c loc)
  (match game-state
    [(Checkers pieces turn cp)
     (Checkers (cons (Piece c loc) pieces) turn cp)]))

(: xy->loc : Integer Integer -> Loc)
;; convert (x, y) coordinates to a grid location
(define (xy->loc x y)
  (Loc (+ 1 (exact-floor (/ y 50))) (+ 1 (exact-floor (/ x 50)))))

(: click-board : Checkers Integer Integer Mouse-Event -> Checkers)
;; this function is called whenever the mouse does something
;; Currently, it adds a piece to the board at the clicked location
(define (click-board game-state x y event)
  (match event
    ["button-up" (match (Checkers-clicked-piece game-state)
                   [(Piece _ _) (place-piece (xy->loc x y) game-state)]
                   ['none (match (get-piece-or-none (xy->loc x y) game-state)
                            [(Piece c loc) (click-piece (Piece c loc) game-state)]
                            ['none game-state])])]
                            
    [_ game-state]))

;; (add-piece game-state 'red (xy->loc x y))

;;; ==== Code for interactivity ====
;; Code past this point is used for rendering the game board

;;;;; functions we need from Lab 1
(: draw-square : Integer Image-Color -> Image)
(define (draw-square size color)
  (overlay
   (square size "outline" "black")
   (square size "solid" color)))

(: alt-shaded-row : Integer Integer Image-Color Image-Color -> Image)
(define (alt-shaded-row n size color-one color-two)
  (cond
    [(= 0 n) empty-image]
    [else (beside
           (draw-square size color-one)
           (alt-shaded-row (- n 1) size color-two color-one))]))

(: alt-shaded-rows : Integer Integer Integer Image-Color Image-Color -> Image)
(define (alt-shaded-rows m n size color-one color-two)
  (cond
    [(= 0 m) empty-image]
    [else (above
           (alt-shaded-row n size color-one color-two)
           (alt-shaded-rows (- m 1) n size color-two color-one))]))

(: draw-board : Checkers -> Image)
;; draws the given game state
(define (draw-board game)
  (local
    {(: sq-len : Integer)
     (define sq-len 50)

     (: board : Image)
     (define board (alt-shaded-rows 8 8 sq-len "maroon" "ivory"))
     
     (: draw-piece : Color -> Image)
     (define (draw-piece color)
       (circle (round (/ (- sq-len 10) 2))
               "solid"
               (match color
                 ['red  "red"]
                 ['black "black"])))

     (: draw-pieces : (Listof Piece) Image -> Image)
     (define (draw-pieces pieces background)
       (match pieces
         ['() background]
         [(cons (Piece color (Loc r c)) rest)
          (overlay/xy (draw-piece color)
                      (- (* -1 sq-len (- c 1)) 5)
                      (- (* -1 sq-len (- r 1)) 5)
                      (draw-pieces rest background))]))}
    (draw-pieces (Checkers-pieces game) board)))

(: start-game : -> Checkers)
;; renders an interactive game board in the start state
;; the clauses inside [] below are the handlers for different interactions
;; To draw the game state, we call the function draw-board
;; On a mouse action, we call the function click-board
(define (start-game)
  (big-bang starting-board : Checkers
    [to-draw draw-board]
    [on-mouse click-board]))