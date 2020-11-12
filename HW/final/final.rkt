;; Final
  
;;
;; Yifei Li
;; liyifei

#lang typed/racket
(require "../include/cs151-core.rkt")
(require typed/test-engine/racket-tests)

(: integer-lte? : Integer Integer -> Boolean)
(define (integer-lte? n1 n2)
  (<= n1 n2))
  
(define-struct (BST A)
  ([lte? : (A A -> Boolean)]
   [bst : (U 'none (Node A))]))

(define-struct (Node A)
  ([value : A]
   [left-child : (U 'none (Node A))]
   [right-child : (U 'none (Node A))]))

(: tree : (BST Integer))
(define tree
  (BST
   integer-lte?
   (Node 50
             (Node -5 'none (Node 46 'none 'none))
             (Node 81 (Node 66 'none 'none) 'none))))

(: third-moment : (Listof Real) -> Real)
(define (third-moment lst)
  (local
    {
     (: element : Real -> Real)
     (define (element r)
       (/ (expt r 3) (length lst)))}
    (foldr + 0 (map element lst))))

(: p2-test : (Listof Real))
(define p2-test '(1 2 3 4 5))
(check-expect (third-moment p2-test) (/ (+ 1 8 27 64 125) 5))


(: power-set : (Listof Integer) -> (Listof (Listof Integer)))
(define (power-set lst)
  (local
    {
     (: append-first : (Listof Integer) -> (Listof Integer))
     (define (append-first my-lst)
       (append (list (first lst)) my-lst))}
    (cond
      [(= (length lst) 1) (list (list) lst)]
      [else (append (map append-first (power-set (rest lst)))
                    (power-set (rest lst)))])))

(power-set '(1 2 3))
  



(define-struct Hemisphere
([longitude : (U 'east 'west)]
 [latitude : (U 'south 'north)]))

(: country->hemisphere : String -> Hemisphere)
(define (country->hemisphere str)
  (match str
    ["United States" (Hemisphere 'west 'north)]
    ["China" (Hemisphere 'east 'north)]
    ["Japan" (Hemisphere 'east 'north)]
    ["Australia" (Hemisphere 'east 'south)]))

(: same-hemisphere? : String String -> Boolean)
(define (same-hemisphere? str1 str2)
  (match* ((country->hemisphere str1) (country->hemisphere str2))
    [(n n) #t]
    [(a b) #f]))

(check-expect (same-hemisphere? "China" "Japan") #t)
(check-expect (same-hemisphere? "Australia" "United States") #f)


(define-struct Phil
([days-alive : Integer]
 [past-life : (U 'none Phil)]))

(: rebirth : Phil -> Phil)
(define (rebirth phil)
  (Phil 0 phil))

(: total-days-alive : Phil -> Integer)
(define (total-days-alive phil)
  (match (Phil-past-life phil)
     ['none (Phil-days-alive phil)]
     [(Phil d p) (+ (total-days-alive (Phil d p)) (Phil-days-alive phil))]))

(check-expect (total-days-alive (Phil 3 (Phil 2 (Phil 5 'none)))) 10)


(define-type Base (U 'A 'C 'T 'G))
(define-type DNA (Vectorof Base))

(: complementary-bases? : Base Base -> Boolean)
(define (complementary-bases? b1 b2)
  (match* (b1 b2)
    [('A 'T) #t]
    [('T 'A) #t]
    [('C 'G) #t]
    [('G 'C) #t]
    [(_ _) #f]))

(: complementary? : DNA DNA -> Boolean)
(define (complementary? d1 d2)
  (local
    {
     (: get-complementary : Base -> Base)
     (define (get-complementary b)
       (match b
         ['A 'T]
         ['T 'A]
         ['C 'G]
         ['G 'C]))
     (: dna=? : DNA DNA Integer -> Boolean)
     (define (dna=? d1 d2 i)
       (cond
         [(= i (vector-length d1))
             (if (symbol=? (vector-ref d1 (- i 1))
                           (vector-ref d2 (- i 1))) #t #f)]
         [else (dna=? d1 d2 (+ 1 i))]))
     }
    (dna=? d2 (vector-map get-complementary d1) 0)))

(: dna1 : DNA)
(define dna1 (vector 'A 'T 'G 'T))

(: dna2 : DNA)
(define dna2 (vector 'T 'A 'C 'G))
(complementary? dna1 dna2)

(: cross-over! : DNA DNA Integer Integer -> Void)
(define (cross-over! d1 d2 start end)
  (local
    {
     (: swap! : DNA DNA Integer -> Void)
     (define (swap! d1 d2 i)
       (local
         {
          (: temp : Base)
          (define temp (vector-ref d1 i))}
         (begin
           (vector-set! d1 i (vector-ref d2 i))
           (vector-set! d2 i temp)
           (cond
             [(= i end) (void)]
             [else (swap! d1 d2 (+ i 1))]))))}
     (swap! d1 d2 start)))

(cross-over! dna1 dna2 1 2)
dna1
dna2

(define-type Vertex Integer)
(define-struct Graph
  ([n : Integer]
   [adj : (Vectorof (Listof Vertex))]))

(define graph (Graph 5 (vector '(1 4) '(0 3) '(3 4) '(1 2) '(0 2))))
(: graph2 : Graph)
(define graph2 (Graph 6 (vector '(1 3) '(0 3) '(4 5) '(0 1) '(2 5) '(2 4))))

(: degree : Graph Vertex -> Integer)
(define (degree g v)
  (length (get-nbrs g v)))

(: get-nbrs : Graph Vertex -> (Listof Vertex))
(define (get-nbrs g v)
  (vector-ref (Graph-adj g) v))
(: all-vtxs : Graph -> (Listof Vertex))
(define (all-vtxs g)
  (build-list (Graph-n g) (lambda ([x : Integer]) x)))

(: connected? : Graph -> Boolean)
(define (connected? g)
  (local
    {
     (: reachable : (Vectorof Boolean))
     (define reachable (make-vector (Graph-n g) #f))}
    (begin (dfs! g 0 reachable)
           (andmap (lambda ([b : Boolean]) b) (vector->list reachable)))))

(: dfs! : Graph Vertex (Vectorof Boolean) -> Void)
;; initially, reachable should be all false
(define (dfs! g v visited)
  (begin
    (vector-set! visited v #t)
    (local
      {
       (: explore-list! : (Listof Vertex) -> Void)
       (define (explore-list! nbrs)
         (cond
           [(empty? nbrs) (void)]
           [else (begin (if (vector-ref visited (first nbrs))
                            (void)
                            (dfs! g (first nbrs) visited))
                        (explore-list! (rest nbrs)) )]))}
      (explore-list! (get-nbrs g v)))))

(: cycle? : Graph -> Boolean)
(define (cycle? g)
  (local
    {
     (: two-deg? : Vertex -> Boolean)
     (define (two-deg? v)
       (match (degree g v)
         [2 #t]
         [_ #f]))}
    (and
     (connected? g)
     (andmap two-deg? (all-vtxs g)))))

(cycle? graph)
(cycle? graph2)




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

(: change-turn : Checkers -> Checkers)
(define (change-turn game)
  (match game
    [(Checkers lst 'red cli) (Checkers lst 'black cli)]
    [(Checkers lst 'black cli) (Checkers lst 'red cli)]))

(change-turn (Checkers '() 'red 'none))


(: two-squares-away? : Checkers Loc -> Boolean)
(define (two-squares-away? game loc)
  (match (Checkers-clicked-piece game)
    [(Piece _ piece-loc)
     (and
      (or (= 2 (- (Loc-row piece-loc) (Loc-row loc)))
          (= -2 (- (Loc-row piece-loc) (Loc-row loc))))
      (or (= 2 (- (Loc-col piece-loc) (Loc-col loc)))
          (= -2 (- (Loc-col piece-loc) (Loc-col loc)))))]))

(two-squares-away? (Checkers '() 'red (Piece 'red (Loc 2 4))) (Loc 0 1))

(define-struct (Tree A)
  ([value : A]
   [left-child : (U 'none (Tree A))]
   [right-child : (U 'none (Tree A))]))

(: dfs-order : (All (A) (U 'none (Tree A)) -> (Listof A)))
(define (dfs-order tree)
  (match tree
    ['none (list)]
    [(Tree v l r) (append (list v) (dfs-order l) (dfs-order r))]))

(dfs-order (Tree 1 (Tree 2 (Tree 3 (Tree 4 'none 'none) (Tree 5 'none 'none)) 'none) (Tree 6 (Tree 7 (Tree 8 'none 'none) (Tree 9 'none 'none)) (Tree 10 'none 'none))))


(: are-anagrams : Integer Integer -> Boolean)
(define (are-anagrams n1 n2)
  

(test)

