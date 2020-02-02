#lang typed/racket

(require typed/test-engine/racket-tests)
(require (only-in typed/racket/gui/base put-file get-file))
(require "../include/cs151-core.rkt")
(require "../include/cs151-image.rkt")
(require "../include/cs151-universe.rkt")



;; TABLE OF CONENTS
;; 1---Data definitions
;; 2---Some test-board definitions
;; 3---miscellaneous, widely used functions
;; 4---physical->logical and helpers
;; 5---logical->string and helpers
;; 6---board-copy and helpers begin
;; 7---board-ref and helpers
;; 8---board-set! and helpers
;; 9---put-stone-at (Deleted)
;; 10--functions about validity and legality (includes valid-board-spec?)
;; 11--apply-move
;; 12--draw functions
;; 13--event handling and play
;; 14--ending the game
;; 15--saving and loading a game
;; 16--writing and reading files
;; 17--running and testing the program

;; -------------------------------------------------
;; 1---DATA DEFINITIONS
;; -------------------------------------------------
(define-struct (Some a)
  ([value : a]))

(define-type (Optional a)
  (U 'None (Some a)))

(define-type Stone
  (U 'black 'white))

(define-struct LogicalLoc
  ([col : Integer]
   [row : Integer]))

(define-type Board
  (Vectorof (Vectorof (Optional Stone))))

(define-struct Go
  ([board : Board]
   [next-to-play : Stone]
   [history : (Listof Board)]
   [last-turn-place : (Optional LogicalLoc)]
   [last-turn-opp-captures : (Listof LogicalLoc)]
   [last-turn-self-captures : (Listof LogicalLoc)]
   [consecutive-passes : Integer]))

(define-struct PhysicalLoc
  ([x-offset-from-left : Integer]
   [y-offset-from-top  : Integer]))

(define-struct BoardSpec
  ([background-color : Image-Color]
   [cell-size-pixels : Integer]
   [margin-pixels : Integer]
   [stone-radius-pixels : Integer]))

(define-struct World
  ([spec : BoardSpec]
   [game : Go]
   [status-message : String]
   [black-tenths : Integer]
   [white-tenths : Integer]
   [hover : (Optional LogicalLoc)]))

(define-struct Outcome
  ([black  : Integer]
   [white  : Integer]
   [winner : (U Stone 'draw)]))





;; -------------------------------------------------
;; 2---SOME TESTBOARD DEFINITIONS
;; -------------------------------------------------
(: test-go : Go)
(define test-go 
  (Go
   (vector
    (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
    (vector (Some 'white) (Some 'white) (Some 'white)(Some 'white))
    (vector 'None 'None 'None 'None)
    (vector 'None 'None 'None 'None))
   'white
   '()
   'None
   '()
   '()
   0))

;; defining a second testboard
(: test-go2 : Go)
(define test-go2 
  (Go
   (vector
    (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
    (vector (Some 'white) (Some 'white) (Some 'white) (Some 'white))
    (vector 'None 'None 'None 'None)
    (vector 'None 'None 'None 'None))
   'black
   '()
   'None
   '()
   '()
   0))
   




;; -------------------------------------------------
;; 3-MISCELLANEOUS, WIDELY USED FUNCTIONS BEGIN
;; -------------------------------------------------

;; logical-legit checks to see if a logical location is legitimate
(: logical-legit? : LogicalLoc Integer -> Boolean)
(define (logical-legit? loc dim)
  (and (< (LogicalLoc-col loc) dim)
       (< (LogicalLoc-row loc) dim)
       (>= (LogicalLoc-col loc) 0)
       (>= (LogicalLoc-row loc) 0)))

(check-expect (logical-legit? (LogicalLoc 10 10) 10) #f)

;; helper function loc-in-list? tells you if a LogicalLoc is in the list
;; of logical locations
(: loc-in-list? : (Listof LogicalLoc) LogicalLoc -> Boolean)
(define (loc-in-list? xs loc)
  (match xs
    ['() #f]
    [(cons hd tl)
     (or (and
          (= (LogicalLoc-col hd)(LogicalLoc-col loc))
          (= (LogicalLoc-row hd)(LogicalLoc-row loc)))
         (loc-in-list? tl loc))]))

(check-expect (loc-in-list?
               (list (LogicalLoc 0 1)
                     (LogicalLoc 3 3)
                     (LogicalLoc 0 0)
                     (LogicalLoc 40 50))
               (LogicalLoc 0 0)) #t)
(check-expect (loc-in-list?
               (list (LogicalLoc 0 1)
                     (LogicalLoc 3 3)
                     (LogicalLoc 0 0)
                     (LogicalLoc 40 50))
               (LogicalLoc 1 1)) #f)


;; logical-physical converts logical to physical location.
;; the integer argument is the dimension (locations per side) of the board.
(: logical->physical : LogicalLoc Integer BoardSpec -> PhysicalLoc)
(define (logical->physical logloc dim board)
  (match* (board logloc)
    [((BoardSpec _ cellsize margin _)(LogicalLoc col row))
     (if (logical-legit? logloc dim)
         (PhysicalLoc
          (+ margin (* col cellsize))
          (+ margin (* (- (- dim 1) row) cellsize)))
         (error "logical->physical: logical location invalid"))]))

(check-expect (logical->physical
               (LogicalLoc 1 1)
               4
               (BoardSpec "green" 100 10 7))(PhysicalLoc 110 210))               
(check-expect (logical->physical
               (LogicalLoc 0 0)
               10
               (BoardSpec "green" 10 0 1)) (PhysicalLoc 0 90))
(check-expect (logical->physical
               (LogicalLoc 0 0)
               15
               (BoardSpec "green" 10 5 1)) (PhysicalLoc 5 145))
(check-expect (logical->physical
               (LogicalLoc 5 4)
               10
               (BoardSpec "green" 10 0 1)) (PhysicalLoc 50 50))
(check-expect (logical->physical
               (LogicalLoc 8 9)
               10
               (BoardSpec "green" 10 5 1)) (PhysicalLoc 85 5))
               

;;---------------------------------------------------
;; 4-PHYSICAL->LOGICAL AND HELPERS BEGIN
;;---------------------------------------------------
;; helper function listx makes a list of all the x values of the
;; logical locations on the board.
(: listx : Integer BoardSpec -> (Listof Integer))
(define (listx dim board)
  (match board
    [(BoardSpec _ cellsize margin r)
     (build-list dim (lambda ([c : Integer])(+ margin (* c cellsize))))]))

(check-expect (listx 5 (BoardSpec "green" 10 5 1))
              (list 5 15 25 35 45))

;; helper function listy makes a list of all the y values of the
;; y locations on the board.
(: listy : Integer BoardSpec -> (Listof Integer))
(define (listy dim board)
  (match board
    [(BoardSpec _ cellsize margin r)
     (build-list dim
                 (lambda ([r : Integer])
                   (+ margin (* (- (- dim 1) r) cellsize))))]))

(check-expect (listy 5 (BoardSpec "green" 10 5 1))
              (list 45 35 25 15 5))


;; helper function distance calculates distance between two locations.
(: distance : PhysicalLoc PhysicalLoc -> Real)
(define (distance p1 p2)
  (match* (p1 p2)
    [((PhysicalLoc x1 y1)(PhysicalLoc x2 y2))
     (sqrt (+ (sqr (- x1 x2))(sqr (- y1 y2))))]))

(check-within (distance (PhysicalLoc 10 20)(PhysicalLoc 0 30))
              14.142 0.01)

;; helper closest calculates the closest value from a list to a value.
(: closest : Integer (Listof Integer) -> Integer)
(define (closest a xs)
  (match xs
    [(cons hd '()) hd]
    [(cons hd tl)(if (<= (abs (- hd a))(abs (- (closest a tl) a)))
                     hd
                     (closest a tl))]))

(check-expect (closest 4 (list 1 3 5 9)) 3)
(check-expect (closest -1 (list -2 1 6 7)) -2)


;; helper within-intersect checks to see if the point is within the given
;; distance of the intersection. The intersection is given by its
;; x and y values.
(: within-intersect : Integer Integer Integer PhysicalLoc -> Boolean)
(define (within-intersect x1 y1 r p)
  (> r (distance (PhysicalLoc x1 y1) p)))

(check-expect (within-intersect 1 1 2 (PhysicalLoc 2 2)) #t)
(check-expect (within-intersect 3 10 6 (PhysicalLoc 3 3)) #f)




;; given a physical location on an intersection point,
;; gives you the logical location
(: intersect->logical :  PhysicalLoc Integer BoardSpec -> LogicalLoc)
(define (intersect->logical ploc dim board)
  (match* (ploc board)
    [((PhysicalLoc x y)(BoardSpec _ cellsize margin _))
     (LogicalLoc (round (/ (- x margin) cellsize))
                 (round (- (sub1 dim)(/ (- y margin) cellsize))))]))

(check-expect (intersect->logical
               (PhysicalLoc 5 95)
               10
               (BoardSpec "green" 10 5 1))(LogicalLoc 0 0))
(check-expect (intersect->logical
               (PhysicalLoc 5 145) 
               15
               (BoardSpec "green" 10 5 1))(LogicalLoc 0 0))
(check-expect (intersect->logical
               (PhysicalLoc 55 55)
               10
               (BoardSpec "green" 10 5 1))(LogicalLoc 5 4))
(check-expect (intersect->logical
               (PhysicalLoc 85 5)
               10
               (BoardSpec "green" 10 5 1))(LogicalLoc 8 9))
               

;; converts physical to logical location.
;; the integer argument is the dimension (locations per side) of the board.
(: physical->logical :  PhysicalLoc Integer BoardSpec -> (Optional LogicalLoc))
(define (physical->logical ploc dim board)
  (if (not (valid-board-spec? board))
      (error "physical->logical: board invalid")
      (match ploc
        [(PhysicalLoc x y)
         (local
           {(define closest-intersect
              (PhysicalLoc (closest x (listx dim board))
                           (closest y (listy dim board))))}
           (if (< (distance closest-intersect ploc)
                  (BoardSpec-stone-radius-pixels board))
               (Some (intersect->logical closest-intersect dim board))
               'None))])))

(check-expect (physical->logical
               (PhysicalLoc 15 25)
               5
               (BoardSpec "green" 10 10 4)) 'None)
(check-expect (physical->logical
               (PhysicalLoc 20 25)
               5
               (BoardSpec "green" 10 10 4)) 'None)
(check-expect (physical->logical
               (PhysicalLoc 46 44)
               5
               (BoardSpec "green" 10 10 4)) 'None)
(check-expect (physical->logical
               (PhysicalLoc 48 38)
               5
               (BoardSpec "green" 10 10 4))(Some (LogicalLoc 4 1)))



;; -------------------------------------------------------------
;; 5-LOGICAL->STRING AND HELPERS BEGIN
;; -------------------------------------------------------------

;; Convert logical locations to strings such as "A1", "B3", etc.
;; Note the letter "I" is skipped in Go labeling.
;; When you get a column past "Z", use "AA", then "BB", then "CC", etc.
;; When you get past "ZZ", use "AAA", then "BBB", etc.
(: logical->string : LogicalLoc -> String)
(define (logical->string loc)
  (match loc
    [(LogicalLoc x y)
     (if (and (>= x 0)(>= y 0))
         (string-append (coord->string x)(number->string (add1 y)))
         (error "logical-string: invalid logical location"))]))

(check-expect (logical->string (LogicalLoc 0 0)) "A1")
(check-expect (logical->string (LogicalLoc 15 18)) "Q19")
(check-expect (logical->string (LogicalLoc 699 727))
              "ABZ728")
(check-error
 (logical->string (LogicalLoc -1 20))
 "logical-string: invalid logical location")



;; helper function that converts a number to its alphabetical representation
;; as described in logical->string.
(: coord->string : Integer -> String)
(define (coord->string n)
  (cond
    [(= 0 n) "A"]
    [(= 1 n) "B"]
    [(= 2 n) "C"]
    [(= 3 n) "D"]
    [(= 4 n) "E"]
    [(= 5 n) "F"]
    [(= 6 n) "G"]
    [(= 7 n) "H"]
    [(= 8 n) "J"]
    [(= 9 n) "K"]
    [(= 10 n) "L"]
    [(= 11 n) "M"]
    [(= 12 n) "N"]
    [(= 13 n) "O"]
    [(= 14 n) "P"]
    [(= 15 n) "Q"]
    [(= 16 n) "R"]
    [(= 17 n) "S"]
    [(= 18 n) "T"]
    [(= 19 n) "U"]
    [(= 20 n) "V"]
    [(= 21 n) "W"]
    [(= 22 n) "X"]
    [(= 23 n) "Y"]
    [(= 24 n) "Z"]
    [(< 24 n)
     (string-append
      (coord->string (sub1 (quotient n 25)))
      (coord->string (remainder n 25)))]
    [else (error "coord->string: invalid input")]))


(check-expect (coord->string 24) "Z")
(check-expect (coord->string 25) "AA")
(check-expect (coord->string 50) "BA")
(check-expect (coord->string 76) "CB")
(check-expect (coord->string 86) "CM")
(check-expect (coord->string 650) "AAA")
(check-expect (coord->string 699) "ABZ")
              











;; -------------------------------------------------------
;; 6-BOARD-COPY AND HELPERS BEGIN
;; -------------------------------------------------------
;; board-copy makes a copy of the board
(: board-copy : Board -> Board)
(define (board-copy b)
  (build-vector
   (vector-length b)
   (lambda
       ([n : Integer])
     (column-copy (vector-ref b n)))))

(check-expect (board-copy (Go-board test-go)) (Go-board test-go))

;; column-copy makes a copy of a column
(: column-copy : (Vectorof (Optional Stone)) -> (Vectorof (Optional Stone)))
(define (column-copy clmn)
  (build-vector
   (vector-length clmn)
   (lambda
       ([n : Integer])
     (vector-ref clmn n))))

(check-expect
 (column-copy
  (vector 'None (Some 'white)(Some 'black) 'None))
 (vector 'None (Some 'white)(Some 'black) 'None))

;; tests to make sure this is a different vector, not the same one,
;; come later after the board-set! functions.








;; --------------------------------------------------------
;; 7-BOARD-REF FUNCTIONS AND HELPERS BEGIN
;; --------------------------------------------------------

;; Return the stone at the specified location on the board,
;; or indicate it is unoccupied
(: board-ref : Go LogicalLoc -> (Optional Stone))
(define (board-ref g loc)
  (match loc
    [(LogicalLoc col row)
     (column-ref (vector-ref (Go-board g) col) row)]))

(check-expect (board-ref test-go (LogicalLoc 1 1)) (Some 'white))
(check-expect (board-ref test-go (LogicalLoc 0 3)) (Some 'black))
(check-expect (board-ref test-go (LogicalLoc 1 3)) (Some 'white))
(check-expect (board-ref test-go (LogicalLoc 3 3)) 'None)




;; board-ref2 returns the stone at the specified location on the
;; board, or indicates if it is unoccupied, but takes in a
;; board instead of a Go.
(: board-ref2  : Board LogicalLoc -> (Optional Stone))
(define (board-ref2 board loc)
  (match loc
    [(LogicalLoc col row)
     (column-ref (vector-ref board col) row)]))

(check-expect (board-ref2 (Go-board test-go) (LogicalLoc 1 1)) (Some 'white))

;; column-ref gives you the stone in a certain row of a certain column
(: column-ref : (Vectorof (Optional Stone)) Integer -> (Optional Stone))
(define (column-ref col row)
  (vector-ref col row))

(check-expect (column-ref
               (vector 'None 'None (Some 'white) 'None)
               2) (Some 'white))





;; --------------------------------------------------------
;; 8-BOARD-SET! AND HELPERS BEGIN
;; --------------------------------------------------------

;; board-set! places the specified stone, or no stone at all, at the
;; given location
(: board-set! : Go LogicalLoc (Optional Stone) -> Void)
(define (board-set! g loc stone)
  (match* (loc g)
    [((LogicalLoc col row)(Go board _ _ _ _ _ _))
     (vector-set!
      board
      col
      (begin
        (vector-set!
         (vector-ref board col) row stone)
        (vector-ref board col)))]))

;; tests:
(: test-go-3 : Go)
(define test-go-3
  (Go
   (vector
    (vector 'None 'None)
    (vector 'None 'None))
   'white
   '()
   'None
   '()
   '()
   0)
  )

(board-set! test-go-3 (LogicalLoc 1 1)  (Some 'black))
(check-expect
 test-go-3
 (Go
  (vector
   (vector 'None 'None)
   (vector 'None (Some 'black)))
  'white
  '()
  'None
  '()
  '() 0))



;; the test below also tests board-copy.
(: test-go-copy-1 : Go)
(define test-go-copy-1
  (Go (board-copy (Go-board test-go)) 'white '() 'None '() '() 0))
(board-set! test-go-copy-1 (LogicalLoc 0 0) 'None)
(check-expect
 test-go-copy-1
 (Go
  (vector
   (vector 'None (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'white) (Some 'white) (Some 'white) (Some 'white))
   (vector 'None 'None 'None 'None)
   (vector 'None 'None 'None 'None))
  'white
  '()
  'None
  '()
  '() 0))

(check-expect
 test-go
 (Go
  (vector
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'white) (Some 'white) (Some 'white) (Some 'white))
   (vector 'None 'None 'None 'None)
   (vector 'None 'None 'None 'None))
  'white
  '()
  'None
  '()
  '()
  0))



;; board-set-too! places the specified stone, or no stone,
;; at the given location --- except, the input for this
;; function is a board, not a go.
(: board-set-too! : Board LogicalLoc (Optional Stone) -> Void)
(define (board-set-too! board loc stone)
  (match loc
    [(LogicalLoc col row)
     (vector-set!
      board
      col
      (begin
        (vector-set!
         (vector-ref board col) row stone)
        (vector-ref board col)))]))

;; test:
(: test-board-copy-1 : Board)
(define test-board-copy-1 (board-copy (Go-board test-go)))
(board-set-too! test-board-copy-1 (LogicalLoc 0 0) 'None)
(check-expect
 test-board-copy-1
 (vector
  (vector 'None (Some 'black) (Some 'black) (Some 'black))
  (vector (Some 'white) (Some 'white) (Some 'white) (Some 'white))
  (vector 'None 'None 'None 'None)
  (vector 'None 'None 'None 'None)))








;; --------------------------------------------------------
;; 10-FUNCTIONS TO CHECK LEGALITY/VALIDITY BEGIN
;; --------------------------------------------------------

;; sees if the board is valid, by making sure the parameters are positive
;; and the radius isn't larger than the margin or half the cell size.
(: valid-board-spec? : BoardSpec -> Boolean)
(define (valid-board-spec? board)
  (match board
    [(BoardSpec _ cellsize margin r)
     (and
      (> cellsize 0)
      (> r 0)
      (< r (/ cellsize 2))
      (> margin r))]))

(check-expect (valid-board-spec? (BoardSpec "green" 10 6 5)) #f)
(check-expect (valid-board-spec? (BoardSpec "red" 20 5 5)) #f)
(check-expect (valid-board-spec? (BoardSpec "blue" 20 10 9)) #t)


;; legal-move? checks to see if the move is legal or not.
(: legal-move? : Go LogicalLoc -> Boolean)
(define (legal-move? g loc)
  (match* (g loc)
    [((Go board next hist ltp lopc lsc cp)(LogicalLoc x y))
     (cond
       [(or (< x 0)(< y 0)
            (>= x (vector-length board))
            (>= y (vector-length board)))
        #f]
       [(two-passes? g) #f]
       [(local
          {(: clone : Go)
           (define clone (Go (board-copy board) next hist ltp lopc lsc cp))}
          (not (check-history (apply-move clone loc)))) #f]
       [else
        (match (board-ref g loc)
          ['None #t]
          [else #f])])]))

(check-expect
 (legal-move?
  (Go
   (vector
    (vector 'None 'None (Some 'white))
    (vector 'None (Some 'black)(Some 'black))
    (vector 'None 'None 'None))
   'black
   '()
   'None
   '()
   '()
   2)
  (LogicalLoc 0 0)) #f)

(check-expect
 (legal-move? test-go (LogicalLoc 1 1)) #f)
(check-expect
 (legal-move? test-go (LogicalLoc 2 3)) #t)

(check-expect
 (legal-move?
  (Go
   (vector
    (vector 'None 'None (Some 'white))
    (vector 'None (Some 'black)(Some 'black))
    (vector 'None 'None 'None))
   'black
   (list
    (vector
     (vector 'None (Some 'black) 'None)
     (vector 'None (Some 'black)(Some 'black))
     (vector 'None 'None 'None)))
   'None
   '()
   '() 0)
  (LogicalLoc 0 1))
 #f)

(check-expect
 (legal-move?
  (Go
   (vector
    (vector 'None 'None (Some 'white))
    (vector 'None (Some 'black)(Some 'black))
    (vector 'None 'None 'None))
   'black
   (list
    (vector
     (vector 'None (Some 'black) (Some 'black))
     (vector 'None (Some 'black)(Some 'black))
     (vector 'None 'None 'None)))
   'None
   '()
   '()
   0)
  (LogicalLoc 0 1))
 #t)

  

;; check-history returns false if the current board state has been
;; attained before
(: check-history : Go -> Boolean)
(define (check-history g)
  (match g
    [(Go board next hist _ _ _ _)
     (local
       {(: helper : Board (Listof Board) -> Boolean)
        (define (helper b1 bds)
          (match bds
            ['() #t]
            [(cons hd tl)
             (if (board=? b1 hd) #f
                 (helper b1 tl))]))}
       (helper board hist))]))


(check-expect
 (check-history
  (Go
   (vector
    (vector  'None 'None)
    (vector (Some 'black) 'None))
   'black
   (list
    (vector
     (vector  'None 'None)
     (vector 'None 'None))
    (vector
     (vector  'None 'None)
     (vector (Some 'black) 'None)))
   'None
   '()
   '()
   0))
 #f)

(check-expect
 (check-history
  (Go
   (vector
    (vector  'None 'None)
    (vector (Some 'black) 'None))
   'black
   (list
    (vector
     (vector  'None 'None)
     (vector 'None 'None))
    (vector
     (vector  'None 'None)
     (vector  'None (Some 'black)))) 'None '() '() 0)) #t)
   
   
 
         


;; board=? checks to see if two boards are equal, assuming they
;; are the same length
(: board=? : Board Board -> Boolean)
(define (board=? board1 board2)
  (local
    {(: helper : Integer Board Board -> Boolean)
     (define (helper n b1 b2)
       (cond
         [(= n (vector-length b1)) #t]
         [else
          (if
           (not (vector=? (vector-ref b1 n)
                          (vector-ref b2 n)))
           #f
           (helper (add1 n) b1 b2))]))}
    (helper 0 board1 board2)))


(check-expect
 (board=?
  (vector
   (vector 'None 'None (Some 'white))
   (vector 'None (Some 'black) 'None)
   (vector (Some 'white) 'None (Some 'black)))
  (vector
   (vector 'None 'None (Some 'white))
   (vector 'None (Some 'black) 'None)
   (vector (Some 'white) 'None (Some 'black)))) #t)

(check-expect
 (board=?
  (vector
   (vector 'None 'None (Some 'white))
   (vector 'None (Some 'black) 'None)
   (vector (Some 'white) 'None (Some 'black)))
  (vector
   (vector 'None 'None (Some 'white))
   (vector 'None (Some 'black) 'None)
   (vector (Some 'white) (Some 'black) (Some 'black)))) #f)

   

;; vector=? checks to see if two vectors of stones are equal, assuming they
;; are the same length
(: vector=? : (Vectorof (Optional Stone))(Vectorof (Optional Stone))
   -> Boolean)
(define (vector=? vector1 vector2)
  (local
    {(: helper : Integer
        (Vectorof (Optional Stone))(Vectorof (Optional Stone)) -> Boolean)
     (define (helper n v1 v2)
       (cond
         [(= n (vector-length v1)) #t]
         [else
          (match* ((vector-ref v1 n)(vector-ref v2 n))
            [('None 'None)(helper (add1 n) v1 v2)]
            [((Some 'white)(Some 'white))(helper (add1 n) v1 v2)]
            [((Some 'black)(Some 'black))(helper (add1 n) v1 v2)]
            [(_ _) #f])]))}
    (helper 0 vector1 vector2)))

(check-expect
 (vector=?
  (vector 'None 'None (Some 'white))
  (vector 'None (Some 'black) (Some 'white))) #f)

(check-expect
 (vector=?
  (vector 'None 'None (Some 'white))
  (vector 'None 'None (Some 'white))) #t)











;; --------------------------------------------------------
;; 11-APPLY-MOVE AND HELPERS BEGIN
;; --------------------------------------------------------

;; apply-move  places a stone on behalf of the player whose turn it is
;; at the specified location, and performs game mechanics.
;; it assumes a legal move.
(: apply-move : Go LogicalLoc -> Go)
(define (apply-move g loc)
  (match* (g loc)
    [((Go board stone-to-place hist
          last-turn-loc last-op-caps last-self-caps _)
      (LogicalLoc x y))       
     (local
       {(define board2 (board-copy board))
        (: to-cap : (Optional (Listof LogicalLoc)))
        (define to-cap
          (begin
            (board-set-too! board2 loc (Some stone-to-place))  
            (Some
             (list-op
              board2
              (if (symbol=? stone-to-place 'white) 'black
                  'white)
              loc))))}
       (begin
         (remove-stones board2 to-cap)
         (local
           {(: to-self-cap : (Optional (Listof LogicalLoc)))
            (define to-self-cap
              (explore
               board2
               stone-to-place
               (cons loc '())
               '()))}
           (begin
             (remove-stones board2 to-self-cap)
             (Go
              board2
              (if (symbol=? stone-to-place 'white) 'black
                  'white)
              (cons board hist)
              (Some loc)
              (match to-cap
                ['None '()]
                [(Some x) (remove-duplicates x)])
              (match to-self-cap
                ['None '()]
                [(Some x) (remove-duplicates x)])
              0)))))]))
              
              
            
             
         




;; test capturing
(check-expect
 (apply-move
  (Go
   (vector
    (vector 'None 'None 'None 'None (Some 'black))
    (vector 'None (Some 'white) (Some 'white)(Some 'white) 'None)
    (vector (Some 'white) (Some 'black)(Some 'black)(Some 'black) 'None)
    (vector 'None (Some 'white) (Some 'white) (Some 'white)(Some 'black))
    (vector 'None 'None 'None 'None (Some 'white)))
   'white
   '()
   'None
   '()
   '()
   0)
  (LogicalLoc 2 4))
 (Go
  (vector
   (vector 'None 'None 'None 'None (Some 'black))
   (vector 'None (Some 'white) (Some 'white)(Some 'white) 'None)
   (vector (Some 'white) 'None 'None 'None (Some 'white))
   (vector 'None (Some 'white) (Some 'white) (Some 'white) 'None)
   (vector 'None 'None 'None 'None (Some 'white)))
  'black
  (list
   (vector
    (vector 'None 'None 'None 'None (Some 'black))
    (vector 'None (Some 'white) (Some 'white)(Some 'white) 'None)
    (vector (Some 'white) (Some 'black)(Some 'black)(Some 'black) 'None)
    (vector 'None (Some 'white) (Some 'white) (Some 'white)(Some 'black))
    (vector 'None 'None 'None 'None (Some 'white))))
  (Some (LogicalLoc 2 4))
  (list
   (LogicalLoc 3 4)
   (LogicalLoc 2 1)
   (LogicalLoc 2 2)
   (LogicalLoc 2 3))
  '()
  0))

;; testing regular capture preceding self capture
(check-expect
 (apply-move
  (Go
   (vector
    (vector 'None 'None 'None 'None 'None)
    (vector 'None (Some 'white) (Some 'white)(Some 'black) 'None)
    (vector (Some 'white) (Some 'black) 'None  (Some 'white) (Some 'black))
    (vector 'None (Some 'white) (Some 'white) (Some 'black) 'None)
    (vector (Some 'black) 'None 'None 'None 'None))
   'black
   '()
   'None
   '()
   '()
   0)
  (LogicalLoc 2 2))
 (Go
  (vector
   (vector 'None 'None 'None 'None 'None)
   (vector 'None (Some 'white) (Some 'white)(Some 'black) 'None)
   (vector (Some 'white) (Some 'black) (Some 'black)  'None (Some 'black))
   (vector 'None (Some 'white) (Some 'white) (Some 'black) 'None)
   (vector (Some 'black) 'None 'None 'None 'None))
  'white 
  (list
   (vector
    (vector 'None 'None 'None 'None 'None)
    (vector 'None (Some 'white) (Some 'white)(Some 'black) 'None)
    (vector (Some 'white) (Some 'black) 'None  (Some 'white) (Some 'black))
    (vector 'None (Some 'white) (Some 'white) (Some 'black) 'None)
    (vector (Some 'black) 'None 'None 'None 'None)))
  (Some (LogicalLoc 2 2))
  (list
   (LogicalLoc 2 3))
  '()
  0))

;; testing self capture
(check-expect
 (apply-move
  (Go
   (vector
    (vector 'None 'None 'None 'None 'None)
    (vector 'None (Some 'white) (Some 'white)(Some 'black) 'None)
    (vector (Some 'white) (Some 'black) 'None  (Some 'white) (Some 'white))
    (vector 'None (Some 'white) (Some 'white) (Some 'black) 'None)
    (vector (Some 'black) 'None 'None 'None 'None))
   'black
   '()
   'None
   '()
   '()
   0)
  (LogicalLoc 2 2))
 (Go
  (vector
   (vector 'None 'None 'None 'None 'None)
   (vector 'None (Some 'white) (Some 'white)(Some 'black) 'None)
   (vector (Some 'white) 'None 'None  (Some 'white) (Some 'white))
   (vector 'None (Some 'white) (Some 'white) (Some 'black) 'None)
   (vector (Some 'black) 'None 'None 'None 'None))
  'white 
  (list
   (vector
    (vector 'None 'None 'None 'None 'None)
    (vector 'None (Some 'white) (Some 'white)(Some 'black) 'None)
    (vector (Some 'white) (Some 'black) 'None  (Some 'white) (Some 'white))
    (vector 'None (Some 'white) (Some 'white) (Some 'black) 'None)
    (vector (Some 'black) 'None 'None 'None 'None)))
  (Some (LogicalLoc 2 2))
  '()
  (list
   (LogicalLoc 2 1)
   (LogicalLoc 2 2)) 0))



;; list-op returns the list of logical locations
;; of stones of a particular color with no liberties,
;; in areas adjacent to the given location.
(: list-op : Board Stone LogicalLoc -> (Listof LogicalLoc))
(define (list-op board color loc)
  (match loc
    [(LogicalLoc x y)
     (append
      (if (logical-legit? (LogicalLoc (add1 x) y)(vector-length board))
          (match (board-ref2 board (LogicalLoc (add1 x) y))
            ['None '()]
            [(Some 'black)
             (if (symbol=? color 'black)
                 (local
                   {(: exp-result : (Optional (Listof LogicalLoc)))
                    (define exp-result
                      (explore
                       board
                       'black
                       (cons (LogicalLoc (add1 x) y) '())
                       '()))}
                   (match exp-result    
                     ['None '()]
                     [(Some x) x]))
                 '())]
            [(Some 'white)
             (if (symbol=? color 'white)
                 (local
                   {(: exp-result : (Optional (Listof LogicalLoc)))
                    (define exp-result
                      (explore
                       board
                       'white
                       (cons (LogicalLoc (add1 x) y) '())
                       '()))}        
                   (match exp-result
                     ['None '()]
                     [(Some x) x]))
                 '())])
          '())
      (if (logical-legit? (LogicalLoc (sub1 x) y)(vector-length board))
          (match (board-ref2 board (LogicalLoc (sub1 x) y))
            ['None '()]
            [(Some 'black)
             (if (symbol=? color 'black)
                 (local
                   {(: exp-result : (Optional (Listof LogicalLoc)))
                    (define exp-result
                      (explore
                       board
                       'black
                       (cons (LogicalLoc (sub1 x) y) '())
                       '()))}
                   (match exp-result   
                     ['None '()]
                     [(Some x) x]))
                 '())]
            [(Some 'white)
             (if (symbol=? color 'white)
                 (local
                   {(: exp-result : (Optional (Listof LogicalLoc)))
                    (define exp-result
                      (explore
                       board
                       'white
                       (cons (LogicalLoc (sub1 x) y) '())
                       '()))}
                   (match exp-result
                     ['None '()]
                     [(Some x) x]))
                 '())])
          '())
      (if (logical-legit? (LogicalLoc x (add1 y))(vector-length board))
          (match (board-ref2 board (LogicalLoc x (add1 y)))
            ['None '()]
            [(Some 'black)
             (if (symbol=? color 'black)
                 (local
                   {(: exp-result : (Optional (Listof LogicalLoc)))
                    (define exp-result
                      (explore
                       board
                       'black
                       (cons (LogicalLoc x (add1 y)) '())
                       '()))}
                   (match exp-result
                     ['None '()]
                     [(Some x) x]))
                 '())]
            [(Some 'white)
             (if (symbol=? color 'white)
                 (local
                   {(: exp-result : (Optional (Listof LogicalLoc)))
                    (define exp-result
                      (explore
                       board
                       'white
                       (cons (LogicalLoc x (add1 y)) '())
                       '()))}
                   (match exp-result
                     ['None '()]
                     [(Some x) x]))
                 '())])
          '())
      (if (logical-legit? (LogicalLoc x (sub1 y))(vector-length board)) 
          (match (board-ref2 board (LogicalLoc x (sub1 y)))
            ['None '()]
            [(Some 'black)
             (if (symbol=? color 'black)
                 (local
                   {(: exp-result : (Optional (Listof LogicalLoc)))
                    (define exp-result
                      (explore
                       board
                       'black
                       (cons (LogicalLoc x (sub1 y)) '())
                       '()))}
                   (match exp-result
                     ['None '()]
                     [(Some x) x]))
                 '())]
            [(Some 'white)
             (if (symbol=? color 'white)
                 (local
                   {(: exp-result : (Optional (Listof LogicalLoc)))
                    (define exp-result
                      (explore
                       board
                       'white
                       (cons (LogicalLoc x (sub1 y)) '())
                       '()))}
                   (match exp-result
                     ['None '()]
                     [(Some x) x]))
                 '())])
          '()))]))
      
(check-expect
 (list-op
  (vector
   (vector 'None 'None 'None 'None 'None)
   (vector 'None (Some 'black) 'None (Some 'black) 'None)
   (vector (Some 'black) (Some 'white) (Some 'black) (Some 'white)
           (Some 'black))
   (vector 'None (Some 'black) 'None (Some 'black) 'None)
   (vector 'None 'None 'None 'None 'None))
  'white
  (LogicalLoc 2 2))
 (list (LogicalLoc 2 3)(LogicalLoc 2 1)))
  

 


;; explore looks to see if a stone has liberties, the stone being
;; specified by color and location.
(: explore : Board Stone
   (Listof LogicalLoc)(Listof LogicalLoc) -> (Optional (Listof LogicalLoc)))
(define (explore board color to-visit visited)
  (match to-visit
    ['() (Some visited)]
    [(cons hd tl)
     (if (filter-none-adjacent
          board (adjacents (vector-length board) hd visited))
         'None
         (explore
          board
          color
          (append (filter-color-adjacent
                   board
                   (adjacents (vector-length board) hd visited)
                   color) tl)
          (cons hd visited)))]))

(check-expect
 (explore (Go-board test-go) 'black (cons (LogicalLoc 0 1) '()) '())
 (Some (list
        (LogicalLoc 0 0)
        (LogicalLoc 0 3)
        (LogicalLoc 0 2)
        (LogicalLoc 0 1)
        )))

(check-expect
 (explore
  (vector
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'black) (Some 'black) (Some 'black) 'None))
  'black
  (cons (LogicalLoc 0 0) '())
  '()) 'None)

(check-expect
 (explore
  (vector
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'white)))
  'black
  (cons (LogicalLoc 0 0) '())
  '())
 (Some (list
        (LogicalLoc 0 1)
        (LogicalLoc 1 1)
        (LogicalLoc 2 1)
        (LogicalLoc 3 2)
        (LogicalLoc 2 2)
        (LogicalLoc 1 2)
        (LogicalLoc 0 3)
        (LogicalLoc 1 3)
        (LogicalLoc 0 3)
        (LogicalLoc 1 3)
        (LogicalLoc 2 3)
        (LogicalLoc 3 2)
        (LogicalLoc 2 2)
        (LogicalLoc 1 2)
        (LogicalLoc 0 2)
        (LogicalLoc 0 1)
        (LogicalLoc 1 1)
        (LogicalLoc 2 1)
        (LogicalLoc 3 1)
        (LogicalLoc 3 0)
        (LogicalLoc 2 0)
        (LogicalLoc 1 0)
        (LogicalLoc 0 0))))


(check-expect
 (explore
  (vector
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'black) (Some 'black) (Some 'white) 'None))
  'black
  (cons (LogicalLoc 0 0) '())
  '()) 'None)
  





;; adjacents gives you a list of adjacent LogicalLocs to the given one,
;; that are not in the second list.
;; it also requires the dimesions of the board.
(: adjacents : Integer LogicalLoc (Listof LogicalLoc) -> (Listof LogicalLoc))
(define (adjacents dim loc locs)
  (match loc
    [(LogicalLoc x y)
     (append
      (if
       (and (logical-legit? (LogicalLoc (add1 x) y) dim)
            (not (loc-in-list? locs (LogicalLoc (add1 x) y))))
       (cons (LogicalLoc (add1 x) y) '())
       '())
      (append
       (if
        (and (logical-legit? (LogicalLoc (sub1 x) y) dim)
             (not (loc-in-list? locs (LogicalLoc (sub1 x) y))))
        (cons (LogicalLoc (sub1 x) y) '())
        '())
       (append
        (if
         (and (logical-legit? (LogicalLoc x (add1 y)) dim)
              (not (loc-in-list? locs (LogicalLoc x (add1 y)))))
         (cons (LogicalLoc x (add1 y)) '())
         '())
        (append
         (if
          (and (logical-legit? (LogicalLoc x (sub1 y)) dim)
               (not (loc-in-list? locs (LogicalLoc x (sub1 y)))))
          (cons (LogicalLoc x (sub1 y)) '())
          '())
         '()))))]))

(check-expect (adjacents 10 (LogicalLoc 5 5) '())
              (list
               (LogicalLoc 6 5)
               (LogicalLoc 4 5)
               (LogicalLoc 5 6)
               (LogicalLoc 5 4)))

(check-expect
 (adjacents
  10
  (LogicalLoc 5 5)
  (list
   (LogicalLoc 6 5)
   (LogicalLoc 5 4)
   (LogicalLoc 5 5)))
 (list
  (LogicalLoc 4 5)
  (LogicalLoc 5 6)))

(check-expect
 (adjacents
  10
  (LogicalLoc 9 9)
  (list
   (LogicalLoc 9 8)))
 (list
  (LogicalLoc 8 9)))

(check-expect
 (adjacents
  10
  (LogicalLoc 0 0)
  (list
   (LogicalLoc 0 1)))
 (list
  (LogicalLoc 1 0)))





;; filter-none-adjacent takes in a board and list of logical locations,
;; that are supposed to be adjacent to a common logicalloc, and returns
;; true if some of the logical locations in the list are 'None on the
;; given go board.
(: filter-none-adjacent : Board (Listof LogicalLoc) -> Boolean)
(define (filter-none-adjacent board locs)
  (match locs
    ['() #f]
    [(cons hd tl)
     (match (board-ref2 board hd)
       ['None #t]
       [_ (filter-none-adjacent board tl)])]))


(check-expect
 (filter-none-adjacent
  (Go-board test-go)
  (list
   (LogicalLoc 0 0)
   (LogicalLoc 2 0)
   (LogicalLoc 1 1))) #t)

(check-expect
 (filter-none-adjacent
  (Go-board test-go)
  (list
   (LogicalLoc 0 1)
   (LogicalLoc 0 3)
   (LogicalLoc 1 2))) #f)

;; filter-color-adjacent takes in a board and list of LogicalLoc, that are
;; supposed to be adjacent to a common logicalloc and filled, and returns the
;; list of logical locations of a certain color on the board in the list.
(: filter-color-adjacent :
   Board (Listof LogicalLoc) Stone -> (Listof LogicalLoc))
(define (filter-color-adjacent board locs color)
  (match locs
    ['() '()]
    [(cons hd tl)
     (match (board-ref2 board hd)
       [(Some 'black)
        (if (symbol=? color 'black)
            (cons hd (filter-color-adjacent board tl color))
            (filter-color-adjacent board tl color))]
       [(Some 'white)
        (if (symbol=? color 'white)
            (cons hd (filter-color-adjacent board tl color))
            (filter-color-adjacent board tl color))]
       ['None (filter-color-adjacent board tl color)])]))



(check-expect
 (filter-color-adjacent
  (Go-board test-go)
  (list
   (LogicalLoc 0 1)
   (LogicalLoc 0 3)
   (LogicalLoc 1 2))
  'black)
 (list
  (LogicalLoc 0 1)
  (LogicalLoc 0 3)))
  

            

  
;; remove-stones removes stones from the board given a
;; list of logical locations. If it is given none, it does nothing.
(: remove-stones : Board (Optional (Listof LogicalLoc)) -> Void)
(define (remove-stones bd lcs)
  (local
    {(: helper : Board (Listof LogicalLoc) -> Void)
     (define (helper board locs) 
       (match locs
         ['() (void)]
         [(cons hd tl)
          (begin
            (board-set-too! board hd 'None)
            (helper board tl))]))}
    (match lcs
      ['None (void)]
      [(Some xs)(helper bd xs)])))

(: remove-stones-test-board : Board)
(define remove-stones-test-board
  (vector
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'white) (Some 'white) (Some 'white)(Some 'white))
   (vector 'None 'None 'None 'None)
   (vector 'None 'None 'None 'None)))

  
(remove-stones remove-stones-test-board
               (Some (list
                      (LogicalLoc 1 1)
                      (LogicalLoc 0 0))))

(check-expect
 remove-stones-test-board
 (vector
  (vector 'None (Some 'black) (Some 'black) (Some 'black))
  (vector (Some 'white) 'None (Some 'white)(Some 'white))
  (vector 'None 'None 'None 'None)
  (vector 'None 'None 'None 'None)))

(remove-stones remove-stones-test-board 'None)
(check-expect
 remove-stones-test-board
 (vector
  (vector 'None (Some 'black) (Some 'black) (Some 'black))
  (vector (Some 'white) 'None (Some 'white)(Some 'white))
  (vector 'None 'None 'None 'None)
  (vector 'None 'None 'None 'None)))
               
         
  




;; --------------------------------------------------------
;; 12-DRAW FUNCTIONS BEGIN
;; --------------------------------------------------------


;; draw-row draws a row of boxes given dim and board specifications
(: draw-row : Integer BoardSpec -> Image)
(define (draw-row dim board)
  (if (<= dim 0)(square 0 "solid" "black")
      (match board
        [(BoardSpec _ csize _ _)
         (beside (square csize "outline" "black")
                 (draw-row (sub1 dim) board))])))


;; stack-image stacks an image on top of itself a specified number of times.
(: stack-image : Integer Image -> Image)
(define (stack-image n tostack)
  (if (<= n 0)(square 0 "solid" "black")
      (above tostack
             (stack-image (sub1 n) tostack))))


;; add-background draws the square beneath the grid of squares
(: add-background : BoardSpec Image -> Image)
(define (add-background board grid)
  (match board
    [(BoardSpec color cellsize margin _)
     (place-image/align
      grid
      margin
      margin
      "left"
      "top"
      (square (+ (* 2 margin) (image-width grid)) "solid" color))]))




  
;; draw-stones-in-column draws all the stones in a particular column
(: draw-stones-in-column :
   Integer (Vectorof (Optional Stone)) BoardSpec -> Image)
(define (draw-stones-in-column col stones bspec)
  (local
    {(: helper : Integer -> Image)
     (define (helper row)
       (cond
         [(= row (vector-length stones))
          (square
           (+
            (* 2 (BoardSpec-margin-pixels bspec))
            (* (sub1 (vector-length stones))(BoardSpec-cell-size-pixels bspec)))
           "outline" "gold")]
         [else
          (match (vector-ref stones row)
            ['None (helper (add1 row))]
            [(Some 'white)
             (place-image
              (circle (BoardSpec-stone-radius-pixels bspec) "solid" "white")
              (PhysicalLoc-x-offset-from-left
               (logical->physical
                (LogicalLoc col row)
                (vector-length stones)
                bspec))
              (PhysicalLoc-y-offset-from-top
               (logical->physical
                (LogicalLoc col row)
                (vector-length stones)
                bspec))
              (helper (add1 row)))]
            [_
             (place-image
              (circle (BoardSpec-stone-radius-pixels bspec) "solid" "black")
              (PhysicalLoc-x-offset-from-left
               (logical->physical
                (LogicalLoc col row)
                (vector-length stones)
                bspec))
              (PhysicalLoc-y-offset-from-top
               (logical->physical
                (LogicalLoc col row)
                (vector-length stones)
                bspec))
              (helper (add1 row)))])]))}
    (helper 0)))



;; draw-stones draws all the stones given the board and the boardspec.
(: draw-stones : Board BoardSpec -> Image)
(define (draw-stones board bspec)
  (local
    {(: helper : Integer -> Image)
     (define (helper col)
       (cond
         [(= col (vector-length board))
          (square
           (+
            (* 2 (BoardSpec-margin-pixels bspec))
            (* (sub1 (vector-length board))(BoardSpec-cell-size-pixels bspec)))
           "outline" "gold")]
         [else
          (overlay
           (draw-stones-in-column col (vector-ref board col) bspec)
           (helper (add1 col)))]))}
    (helper 0)))


;; draw-captured draws the captured stones of a certain color.
(: draw-captured : Go BoardSpec -> Image)
(define (draw-captured go bspec)
  (match go
    [(Go board next hist lastturn caps scaps passes)
     (local
       {(: to-draw : (Listof LogicalLoc))
        (define to-draw (append caps scaps))
        (: helper : (Listof LogicalLoc) -> Image)
        (define (helper xs)
          (match xs
            ['()
             (square
              (+
               (* 2 (BoardSpec-margin-pixels bspec))
               (* (sub1 (vector-length board))
                  (BoardSpec-cell-size-pixels bspec))) "outline" "gold")]
            [(cons hd tl)
             (if (< (length tl)(length scaps))
                 (place-image
                  (circle
                   (BoardSpec-stone-radius-pixels bspec)
                   "outline"
                   (if (symbol=? next 'black) 'white 'black))
                  (PhysicalLoc-x-offset-from-left
                   (logical->physical hd (vector-length board) bspec))
                  (PhysicalLoc-y-offset-from-top
                   (logical->physical hd (vector-length board) bspec))
                  (helper tl))
                 (place-image
                  (circle
                   (BoardSpec-stone-radius-pixels bspec)
                   "outline" next)
                  (PhysicalLoc-x-offset-from-left
                   (logical->physical hd (vector-length board) bspec))
                  (PhysicalLoc-y-offset-from-top
                   (logical->physical hd (vector-length board) bspec))
                  (helper tl)))]))}
       (helper to-draw))]))
              





;; draw-hover takes in the logical location of the hover and 
;; the same parameters of draw-stones and a color,
;; then draws the transparent stone there.
(: draw-hover : (Optional LogicalLoc) Go BoardSpec -> Image)
(define (draw-hover loc go bspec)
  (match go
    [(Go board next _ _ _ _ _)
     (match loc
       ['None (draw-stones board bspec)]
       [(Some LogicalLoc)
        (if (and
             (logical-legit? (Some-value loc) (vector-length board))
             (legal-move? go (Some-value loc)))
            (place-image
             (circle (BoardSpec-stone-radius-pixels bspec) 128 next)
             (PhysicalLoc-x-offset-from-left
              (logical->physical
               (Some-value loc)
               (vector-length board)
               bspec))
             (PhysicalLoc-y-offset-from-top
              (logical->physical
               (Some-value loc)
               (vector-length board)
               bspec))
             (draw-stones board bspec))
            (draw-stones board bspec))])]))

   
           
    



;; draw-vert-nums draws the numbers to the right of the board
(: draw-vert-nums : Integer BoardSpec -> Image)
(define (draw-vert-nums n board)
  (if (= n 0)(square 0 "solid" "black")
      (above
       (overlay
        (text (number->string n) 12 "black")
        (square (BoardSpec-cell-size-pixels board) "outline" "white"))
       (draw-vert-nums (sub1 n) board))))





;; draw-letters draws the letters at the bottom of the board
(: draw-letters : Integer BoardSpec -> Image)
(define (draw-letters n board)
  (if (= n -1)(square 0 "solid" "black")
      (beside
       (draw-letters (sub1 n) board)
       (overlay
        (text (coord->string n) 12 "black")
        (square (BoardSpec-cell-size-pixels board) "outline" "white")))))




;; ticks->string is used to display the time in minutes and seconds
;; given the number of tenths of seconds
(: ticks->string : Real -> String)
(define (ticks->string t)
  (local
    {(: seconds : Integer)
     (define seconds (quotient (exact-floor t) 10))
     (: tenths : Integer)
     (define tenths (remainder (exact-floor t) 10))}
    (string-append
     (number->string (quotient seconds 60))
     ":"
     (if (< (remainder seconds 60) 10)
         (string-append "0" (number->string (remainder seconds 60)))
         (number->string (remainder seconds 60)))
     "."
     (number->string tenths))))

(check-expect (ticks->string 245.6778) "0:24.5")
(check-expect (ticks->string 616.78) "1:01.6")



;; draw draws the current world
(: draw : World -> Image)
(define (draw w)
  (match w
    [(World bspec game status btenths wtenths hover)
     (if (not (valid-board-spec? bspec))
         (error "draw: board invalid")
         (match game
           [(Go board next _ lastturn lastopcaps lastselfcaps _)
            (above
             (beside
              (overlay
               (match lastturn
                 ['None (draw-hover hover game bspec)]
                 [(Some LogicalLoc)
                  (place-image
                   (circle
                    (+ 1
                       (BoardSpec-stone-radius-pixels bspec))
                    "outline" "green")
                   (PhysicalLoc-x-offset-from-left
                    (logical->physical
                     (Some-value lastturn)
                     (vector-length board)
                     bspec))
                   (PhysicalLoc-y-offset-from-top
                    (logical->physical
                     (Some-value lastturn)
                     (vector-length board)
                     bspec))
                   (draw-hover hover game bspec))])
               (draw-captured game bspec)
               (add-background bspec
                               (stack-image
                                (sub1
                                 (vector-length board))
                                (draw-row
                                 (sub1 (vector-length board)) bspec))))
              (draw-vert-nums (vector-length board) bspec))
             (beside
              (draw-letters (sub1 (vector-length board)) bspec)
              (square (BoardSpec-cell-size-pixels bspec) "outline" "white"))
             (text/font status 11 "indigo" #f 'roman 'normal 'bold #f)
             (text/font (string-append "Black Time: " (ticks->string btenths))
                        13 "black" #f 'roman 'normal 'bold #f)
             (text/font (string-append "White Time: " (ticks->string wtenths))
                        13 "black" #f 'roman 'normal 'bold #f)
             (text/font
              (string-append "Total Time: "
                             (ticks->string (+ wtenths btenths)))
                        13 "black" #f 'roman 'normal 'bold #f)
             (text/font "Game programmed by Mason Wang"
                        11 "black" #f 'roman 'normal 'bold #f)
             
             )]))]))




;; --------------------------------------------------------
;; 13-FUNCTIONS THAT PLAY AND REACT TO EVENTS BEGIN
;; --------------------------------------------------------


;; play initiates a game, provided a board dimension and board specifications.
(: play : Integer BoardSpec -> World)
(define (play dim bspec)
  (if (or (< dim 2)(not (valid-board-spec? bspec)))
      (error "play: starting board invalid")
      (big-bang
          (World
           bspec
           (Go
            (local
              {(: no-stone : (Optional Stone))
               (define no-stone 'None)}
              (make-vector
               dim
               (make-vector
                dim no-stone)))
            'black
            '()
            'None
            '()
            '()
            0)                  
           "Welcome to Go! Black to move"
           0
           0
           'None) : World
        [to-draw draw]
        [on-key react-to-keyboard]
        [on-mouse react-to-click]
        [on-tick tick 1/10])))

;; tick reacts to ticks, which happen every tenth of a second
(: tick : (World -> World))
(define (tick w)
  (match w
    [(World bspec go status btenths wtenths hover)
     (match go
       [(Go _ next _ _ _ _ consec-passes)
        (cond
          [(= consec-passes 2) w]
          [(symbol=? next 'white)
           (World bspec go status btenths (+ 1 wtenths) hover)]
          [else (World bspec go status (+ 1 btenths) wtenths hover)])])]))

(check-expect
 (tick
  (World (BoardSpec "green" 20 10 7 ) test-go "yo" 0 0 (Some (LogicalLoc 0 0))))
 (World (BoardSpec "green" 20 10 7 ) test-go "yo" 0 1 (Some (LogicalLoc 0 0))))

(check-expect
 (tick
  (World
   (BoardSpec "green" 20 10 7 ) test-go2 "yo" 0 0 (Some (LogicalLoc 0 0))))
 (World (BoardSpec "green" 20 10 7 ) test-go2 "yo" 1 0 (Some (LogicalLoc 0 0))))
   
         



;; react-to-click reacts to clicks and updates the world
(: react-to-click (World Integer Integer Mouse-Event -> World))
(define (react-to-click w x y e)
  (if (>= (Go-consecutive-passes (World-game w)) 2)
      w
      (match e
        ["button-down"
         (match w
           [(World bspec g _ btenths wtenths hover)
            (match g
              [(Go board next hist _ _ _ _)
               (local
                 {(: click : (Optional LogicalLoc))
                  (define click
                    (physical->logical
                     (PhysicalLoc x y)
                     (vector-length board) bspec))}
                 (match click
                   ['None w]
                   [(Some (LogicalLoc x y))
                    (if (legal-move? g (Some-value click))
                        (World
                         bspec
                         (apply-move g (Some-value click))
                         (string-append
                          (symbol->string (Go-next-to-play g))
                          " moved to " 
                          (logical->string (Some-value click))
                          (if
                           (symbol=? (Go-next-to-play g) 'black)
                           ". white to move" ". black to move")
                          )
                         btenths
                         wtenths
                         click)
                        (World
                         bspec
                         g                                   
                         (if (symbol=? (Go-next-to-play g) 'white)
                             "Illegal Move! Try again, white"
                             "Illegal Move! Try again, black")
                         btenths
                         wtenths
                         click
                         ))]))])])]
        ["move"
         (match w
           [(World bspec g status btenths wtenths hover)
            (match g
              [(Go board next hist lplace lcaps lscaps passes)
               (local
                 {(: mouseloc : (Optional LogicalLoc))
                  (define mouseloc
                    (physical->logical
                     (PhysicalLoc x y)
                     (vector-length board) bspec))}
                 (World bspec g status btenths wtenths mouseloc))])])]
        [_ w])))

(check-expect
 (react-to-click
  (World
   (BoardSpec "green" 20 10 7 ) test-go "yo" 10 10 (Some (LogicalLoc 0 0)))
  1000 1000 "button-down")
 (World
  (BoardSpec "green" 20 10 7 ) test-go "yo" 10 10 (Some (LogicalLoc 0 0))))




;; react-to-keyboard reacts to keyboard events and updates the world
(: react-to-keyboard (World String -> World))
(define (react-to-keyboard w key)
  (match key
    ["p" (match w
           [(World bspec g status btenths wtenths hover)
            (match g
              [(Go board next hist _ _ _ conp)
               (cond
                 [(>= conp 2) w]
                 [(= conp 1)
                  (World
                   bspec
                   (Go board
                       (if (symbol=? next 'black) 'white 'black)
                       (cons board hist) 'None '() '() (add1 conp))
                   (local
                     {(define result : Outcome (outcome g))}
                     (string-append "Game Over! Black score: "
                                    (number->string (Outcome-black result))
                                    " White score: "
                                    (number->string (Outcome-white result))
                                    "\n"
                                    (match (Outcome-winner result)
                                      ['black "Black wins!"]
                                      ['white "White wins!"]
                                      ['draw "The Game is a Draw!"])))
                   btenths
                   wtenths
                   'None)]
                 [(symbol=? next 'black)
                  (World bspec
                         (Go board
                             'white
                             (cons board hist) 'None '() '() (add1 conp))
                         "black passed! white to move"
                         btenths
                         wtenths
                         hover)]           
                 [else
                  (World bspec
                         (Go board
                             'black
                             (cons board hist) 'None '() '() (add1 conp))
                         "white passed! black to move"
                         btenths
                         wtenths
                         hover)])])])]
    ["s" (begin
           (save-game! w)
           w)]
    ["l" (if
          (=
           (vector-length (Go-board (World-game w)))
           (vector-length
            (Go-board (World-game (load-game (World-spec w))))))
          (load-game (World-spec w))
          (error
           "dimensions of loaded game don't match dimensions of current game"))]
    [_ w]))



(check-expect
 (react-to-keyboard
  (World (BoardSpec "green" 20 10 7 ) test-go "yo" 0 0 (Some (LogicalLoc 0 0)))
  "p")
 (World
  (BoardSpec "green" 20 10 7 )
  (Go
   (vector
    (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
    (vector (Some 'white) (Some 'white) (Some 'white)(Some 'white))
    (vector 'None 'None 'None 'None)
    (vector 'None 'None 'None 'None))
   'black
   (list
    (vector
     (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
     (vector (Some 'white) (Some 'white) (Some 'white)(Some 'white))
     (vector 'None 'None 'None 'None)
     (vector 'None 'None 'None 'None)))
   'None
   '()
   '()
   1)
  "white passed! black to move"
  0 0 (Some (LogicalLoc 0 0))))

;; --------------------------------------------------------
;; 14-ENDING THE GAME
;; -------------------------------------------------------

;; reports if there have been two passes
(: two-passes? : Go -> Boolean)
(define (two-passes? go)
  (>= (Go-consecutive-passes go) 2))

(check-expect (two-passes? test-go) #f)



;; outcome provides the outcome of a game
(: outcome : Go -> Outcome)
(define (outcome go)
  (match go
    [(Go board next hist lastt lastopcap lastscap conp)
     (local
       {(: black-score : Integer)
        (define black-score
          (+ (count-stones-of-color board 'black)
             (length (tally-none board 'black (list-none board)))))
        (: white-score : Integer)
        (define white-score
          (+ (count-stones-of-color board 'white)
             (length (tally-none board 'white (list-none board)))))}
       (Outcome
        black-score
        white-score
        (cond
          [(> black-score white-score) 'black]
          [(< black-score white-score) 'white]
          [else 'draw])))]))

(check-expect
 (outcome test-go)
 (Outcome
  4 12 'white))

(check-expect
 (outcome
  (Go
   (vector
    (vector 'None 'None 'None  (Some 'white) 'None)
    (vector 'None (Some 'white) (Some 'white)  'None (Some 'white))
    (vector (Some 'white) 'None 'None (Some 'white) 'None)
    (vector 'None (Some 'white) (Some 'white) (Some 'black) 'None)
    (vector 'None 'None 'None 'None 'None))
   'black
   '()
   'None
   '()
   '()
   2))
 (Outcome
  1
  16
  'white))

(check-expect
 (outcome
  (Go
   (vector
    (vector 'None 'None 'None)
    (vector 'None (Some 'black) 'None)
    (vector 'None 'None 'None))
   'black
   '()
   'None
   '()
   '()
   2))
 (Outcome
  9
  0
  'black))

(check-expect
 (outcome
  (Go
   (vector
    (vector 'None 'None 'None)
    (vector 'None (Some 'black) 'None)
    (vector (Some 'white) 'None 'None))
   'black
   '()
   'None
   '()
   '()
   2))
 (Outcome
  1
  1
  'draw))
            

          
     


;; count-stones-of-color counts the stones of a particular color on
;; the board.
(: count-stones-of-color : Board Stone -> Integer)
(define (count-stones-of-color board color)
  (local
    {(: helper : Integer -> Integer)
     (define (helper col)
       (if (= col (vector-length board))
           0
           (+ (count-stones-in-column col board color)
              (helper (add1 col)))))}
    (helper 0)))

(check-expect
 (count-stones-of-color (Go-board test-go) 'white) 4)


(check-expect
 (count-stones-of-color (Go-board test-go) 'black) 4)

(check-expect
 (count-stones-of-color
  (vector
   (vector 'None 'None 'None  (Some 'white) 'None)
   (vector 'None (Some 'white) (Some 'white)  'None (Some 'white))
   (vector (Some 'white) 'None 'None (Some 'white) 'None)
   (vector 'None (Some 'white) (Some 'white) (Some 'black) 'None)
   (vector 'None 'None 'None 'None 'None)) 'black) 1)


(check-expect
 (count-stones-of-color
  (vector
   (vector 'None 'None 'None  (Some 'white) 'None)
   (vector 'None (Some 'white) (Some 'white)  'None (Some 'white))
   (vector (Some 'white) 'None 'None (Some 'white) 'None)
   (vector 'None (Some 'white) (Some 'white) (Some 'black) 'None)
   (vector 'None 'None 'None 'None 'None)) 'white) 8)

;; count-stones-in-column counts the stones of a particular color in a column.
(: count-stones-in-column : Integer Board Stone -> Integer)
(define (count-stones-in-column colpos board color)
  (local
    {(: helper : Integer -> Integer)
     (define (helper row)
       (if (= row (vector-length board))
           0
           (match (board-ref2 board (LogicalLoc colpos row))
             ['None (helper (add1 row))]
             [(Some 'white)
              (if (symbol=? color 'white)
                  (add1 (helper (add1 row)))
                  (helper (add1 row)))]
             [(Some 'black)
              (if (symbol=? color 'black)
                  (add1 (helper (add1 row)))
                  (helper (add1 row)))])))}
    (helper 0)))

(check-expect (count-stones-in-column 0 (Go-board test-go) 'black) 4)
(check-expect (count-stones-in-column 0 (Go-board test-go) 'white) 0)
(check-expect (count-stones-in-column 1 (Go-board test-go) 'black) 0)
(check-expect (count-stones-in-column 1 (Go-board test-go) 'white) 4)

;; tally-none takes in a list of all the none locations and gives you
;; a list of all the none locations part of the given color's territory.
(: tally-none : Board Stone (Listof LogicalLoc) -> (Listof LogicalLoc))
(define (tally-none board color nones)
  (match nones
    ['() '()]
    [(cons hd tl)
     (local
       {(: explore-result : (Optional (Listof LogicalLoc)))
        (define explore-result
          (explore2 board color (list hd) '()))}
       (match explore-result
         ['None (tally-none board color tl)]
         [(Some x)
          (remove-duplicates
           (append x
                   (tally-none board color tl)))]))]))

(check-expect
 (length
  (tally-none
   (vector
    (vector 'None 'None 'None  (Some 'white) 'None)
    (vector 'None (Some 'white) (Some 'white)  'None (Some 'white))
    (vector (Some 'white) 'None 'None (Some 'white) 'None)
    (vector 'None (Some 'white) (Some 'white) (Some 'black) 'None)
    (vector 'None 'None 'None 'None 'None))
   'white
   (list
    (LogicalLoc 0 0)
    (LogicalLoc 0 1)
    (LogicalLoc 0 2)
    (LogicalLoc 0 4)
    (LogicalLoc 1 0)
    (LogicalLoc 1 3)
    (LogicalLoc 2 1)
    (LogicalLoc 2 2)
    (LogicalLoc 2 4)
    (LogicalLoc 3 0)
    (LogicalLoc 3 4)
    (LogicalLoc 4 0)
    (LogicalLoc 4 1)
    (LogicalLoc 4 2)
    (LogicalLoc 4 3)
    (LogicalLoc 4 4)))) 8)

(check-expect
 (length
  (tally-none
   (vector
    (vector 'None 'None 'None  (Some 'white) 'None)
    (vector 'None (Some 'white) (Some 'white)  'None (Some 'white))
    (vector (Some 'white) 'None 'None (Some 'white) 'None)
    (vector 'None (Some 'white) (Some 'white) (Some 'black) 'None)
    (vector 'None 'None 'None 'None 'None))
   'white
   (list-none
    (vector
     (vector 'None 'None 'None  (Some 'white) 'None)
     (vector 'None (Some 'white) (Some 'white)  'None (Some 'white))
     (vector (Some 'white) 'None 'None (Some 'white) 'None)
     (vector 'None (Some 'white) (Some 'white) (Some 'black) 'None)
     (vector 'None 'None 'None 'None 'None))))) 8)



(check-expect
 (length
  (tally-none
   (vector
    (vector 'None 'None 'None  (Some 'white) 'None)
    (vector 'None (Some 'white) (Some 'white)  'None (Some 'white))
    (vector (Some 'white) 'None 'None (Some 'white) 'None)
    (vector 'None (Some 'white) (Some 'white) (Some 'black) 'None)
    (vector 'None 'None 'None 'None 'None))
   'black
   (list-none
    (vector
     (vector 'None 'None 'None  (Some 'white) 'None)
     (vector 'None (Some 'white) (Some 'white)  'None (Some 'white))
     (vector (Some 'white) 'None 'None (Some 'white) 'None)
     (vector 'None (Some 'white) (Some 'white) (Some 'black) 'None)
     (vector 'None 'None 'None 'None 'None))))) 0)
         
     

;; list-none gives you a list of all the empty spots on the board.
(: list-none : Board -> (Listof LogicalLoc))
(define (list-none board)
  (local
    {(: helper : Integer -> (Listof LogicalLoc))
     (define (helper colpos)
       (if (= colpos (vector-length board))
           '()
           (append
            (list-none-in-column colpos board)
            (helper (add1 colpos)))))}
    (helper 0)))

(check-expect
 (list-none (Go-board test-go))
 (list
  (LogicalLoc 2 0)
  (LogicalLoc 2 1)
  (LogicalLoc 2 2)
  (LogicalLoc 2 3)
  (LogicalLoc 3 0)
  (LogicalLoc 3 1)
  (LogicalLoc 3 2)
  (LogicalLoc 3 3)))

;; list-none-in-column gives you a list of all the empty spots in a column,
;; given the board and the column number.
(: list-none-in-column : Integer Board -> (Listof LogicalLoc))
(define (list-none-in-column colpos board)
  (local
    {(: helper : Integer -> (Listof LogicalLoc))
     (define (helper row)
       (if (= row (vector-length board))
           '()
           (match (board-ref2 board (LogicalLoc colpos row))
             ['None (cons (LogicalLoc colpos row)
                          (helper (add1 row)))]
             [_ (helper (add1 row))])))}
    (helper 0)))

(check-expect (list-none-in-column 2 (Go-board test-go))
              (list
               (LogicalLoc 2 0)
               (LogicalLoc 2 1)
               (LogicalLoc 2 2)
               (LogicalLoc 2 3)))

(check-expect (list-none-in-column 1 (Go-board test-go))
              '())
  
  



  


;; explore2 given the location of an empty space, provides a list of
;; surrounding stones if it is enclosed by a the stones of a given
;; color. It returns 'None if it is not enclosed by that single color.
(: explore2 : Board Stone
   (Listof LogicalLoc)(Listof LogicalLoc) -> (Optional (Listof LogicalLoc)))
(define (explore2 board color to-visit visited)
  (match to-visit
    ['() (Some (remove-duplicates visited))]
    [(cons hd tl)
     (local
       {(: not-stone : Stone)
        (define not-stone (if (symbol=? color 'black) 'white 'black))}
       (if (filter-stone-adjacent
            board (adjacents (vector-length board) hd visited) not-stone)
           'None
           (explore2
            board
            color
            (append (filter-stones-from-none-adjacent
                     board
                     (adjacents (vector-length board) hd visited)) tl)
            (cons hd visited))))]))

(check-expect
 (explore2
  (vector
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'white) (Some 'white) (Some 'white)(Some 'white))
   (vector 'None 'None 'None 'None)
   (vector 'None 'None 'None 'None))
  'white
  (list
   (LogicalLoc 2 2))
  '())
 (Some
  (list
   (LogicalLoc 2 1)
   (LogicalLoc 2 3)
   (LogicalLoc 3 0)
   (LogicalLoc 2 0)
   (LogicalLoc 3 1)
   (LogicalLoc 3 3)
   (LogicalLoc 3 2)
   (LogicalLoc 2 2))))

(check-expect
 (explore2
  (vector
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'white) (Some 'white) (Some 'white)(Some 'white))
   (vector 'None 'None 'None 'None)
   (vector 'None 'None 'None 'None))
  'black
  (list
   (LogicalLoc 2 2))
  '()) 'None)


(check-expect
 (explore2
  (vector
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector 'None (Some 'white) (Some 'white)(Some 'white))
   (vector 'None 'None 'None 'None)
   (vector 'None 'None 'None 'None))
  'white
  (list
   (LogicalLoc 2 2))
  '()) 'None)


   
 




;; filter-stone-adjacent takes in a board and list of logical
;; locations, and a color,
;; that are supposed to be adjacent to a common logicalloc, and returns
;; true if some of the logical locations in the list are of the color on the
;; given go board.
(: filter-stone-adjacent : Board (Listof LogicalLoc) Stone -> Boolean)
(define (filter-stone-adjacent board locs color)
  (match color
    ['black
     (match locs
       ['() #f]
       [(cons hd tl)
        (match (board-ref2 board hd)
          [(Some 'black) #t]
          [_ (filter-stone-adjacent board tl 'black)])])]
    ['white
     (match locs
       ['() #f]
       [(cons hd tl)
        (match (board-ref2 board hd)
          [(Some 'white) #t]
          [_ (filter-stone-adjacent board tl 'white)])])]))





(check-expect
 (filter-stone-adjacent
  (vector
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'white) (Some 'white) (Some 'white)(Some 'white))
   (vector 'None 'None 'None 'None)
   (vector 'None 'None 'None 'None))
  (list
   (LogicalLoc 1 1)
   (LogicalLoc 2 0)
   (LogicalLoc 3 1)
   (LogicalLoc 2 2)) 'white) #t)

(check-expect
 (filter-stone-adjacent
  (vector
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'white) (Some 'white) (Some 'white)(Some 'white))
   (vector 'None 'None 'None 'None)
   (vector 'None 'None 'None 'None))
  (list
   (LogicalLoc 1 0)
   (LogicalLoc 1 1)
   (LogicalLoc 1 2)
   (LogicalLoc 1 3)) 'white) #t)

(check-expect
 (filter-stone-adjacent
  (vector
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'white) (Some 'white) (Some 'white)(Some 'white))
   (vector 'None 'None 'None 'None)
   (vector 'None 'None 'None 'None))
  (list
   (LogicalLoc 3 3)
   (LogicalLoc 3 2)) 'white) #f)


(check-expect
 (filter-stone-adjacent
  (vector
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'white) (Some 'white) (Some 'white)(Some 'white))
   (vector 'None 'None 'None 'None)
   (vector 'None 'None 'None 'None))
  (list
   (LogicalLoc 1 1)
   (LogicalLoc 2 0)
   (LogicalLoc 3 1)
   (LogicalLoc 2 2)) 'black) #f)



;; filter-stones-from-none-adjacent takes in a list of logical locations and
;; removes the colored stones, leaving a list of nones.
(: filter-stones-from-none-adjacent :
   Board (Listof LogicalLoc) -> (Listof LogicalLoc))
(define (filter-stones-from-none-adjacent board locs)
  (match locs
    ['() '()]
    [(cons hd tl)
     (match (board-ref2 board hd)
       ['None (cons hd (filter-stones-from-none-adjacent board tl))]
       [_ (filter-stones-from-none-adjacent board tl)])]))


(check-expect
 (filter-stones-from-none-adjacent
  (vector
   (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
   (vector (Some 'white) (Some 'white) (Some 'white)(Some 'white))
   (vector 'None 'None 'None 'None)
   (vector 'None 'None 'None 'None))
  (list
   (LogicalLoc 1 1)
   (LogicalLoc 2 0)
   (LogicalLoc 3 1)
   (LogicalLoc 2 2)))
 (list
  (LogicalLoc 2 0)
  (LogicalLoc 3 1)
  (LogicalLoc 2 2)))



;; --------------------------------------------------------
;; 15-SAVING AND LOADING
;; --------------------------------------------------------

;; inter->string converts an intersection to a string
(: inter->string : Board LogicalLoc -> String)
(define (inter->string board loc)
  (match (board-ref2 board loc)
    ['None "_"]
    [(Some 'white) "o"]
    [(Some 'black) "*"]))

(check-expect (inter->string (Go-board test-go)(LogicalLoc 0 0)) "*")

;; col->string converts a column into a string
(: col->string : Board Integer -> String)
(define (col->string board colpos)
  (local
    {(: helper : Integer -> String)
     (define (helper row)
       (if (= row (vector-length board))
           ""
           (string-append
            (inter->string board (LogicalLoc colpos row))
            (helper (add1 row)))))}
    (helper 0)))

(check-expect
 (col->string
  (vector
   (vector 'None (Some 'white) (Some 'black))
   (vector 'None 'None 'None)
   (vector 'None 'None 'None))
  0) "_o*")

;; board->string converts a board into a string
(: board->string : Board -> String)
(define (board->string board)
  (local
    {(: helper : Integer -> String)
     (define (helper col)
       (if (= col (vector-length board))
           ""
           (string-append
            (col->string board col)
            "|"
            (helper (add1 col)))))}
    (substring (helper 0) 0 (sub1 (string-length (helper 0))))))

(check-expect (board->string (Go-board test-go))
              "****|oooo|____|____")

;; history->string converts the history list to string
(: history->string : (Listof Board) -> String)
(define (history->string bds)
  (match bds
    ['() ""]
    [(cons hd '())(board->string hd)]
    [(cons hd tl)(string-append (board->string hd) "!" (history->string tl))]))

(check-expect
 (history->string
  (list
   (vector
    (vector 'None (Some 'white))
    (vector (Some 'black) 'None))
   (vector
    (vector 'None (Some 'white))
    (vector (Some 'white) 'None))
   (vector
    (vector 'None 'None)
    (vector 'None 'None))))
 "_o|*_!_o|o_!__|__")

;; go->string converts a go structure into a string
(: go->string : Go -> String)
(define (go->string go)
  (match go
    [(Go board next hist lastturnplace lastcaps lastscaps conp)
     (string-append
      (if (symbol=? next 'black) "*" "o")
      "~"
      (board->string board)
      "~"
      (history->string hist)
      "~"
      (number->string conp))]))

(check-expect (go->string test-go)
              "o~****|oooo|____|____~~0")

;; world->string converts the current world into a string
(: world->string : World -> String)
(define (world->string w)
  (match w
    [(World spec game status btenths wtenths hover)
     (string-append
      (number->string btenths)
      "@"
      (number->string wtenths)
      "@"
      (go->string game))]))

(check-expect
 (world->string
  (World
   (BoardSpec "green" 20 10 7 )
   (Go
    (vector
     (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
     (vector (Some 'white) (Some 'white) (Some 'white)(Some 'white))
     (vector 'None 'None 'None 'None)
     (vector 'None 'None 'None 'None))
    'black
    (list
     (vector
      (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
      (vector (Some 'white) (Some 'white) (Some 'white)(Some 'white))
      (vector 'None 'None 'None 'None)
      (vector 'None 'None 'None 'None)))
    'None
    '()
    '()
    1)
   "white passed! black to move"
   0 0 (Some (LogicalLoc 0 0))))
 "0@0@*~****|oooo|____|____~****|oooo|____|____~1")




;; string->column converts a string into a column
(: string->column : String -> (Vectorof (Optional Stone)))
(define (string->column s)
  (build-vector
   (string-length s)
   (lambda
       ([x : Integer])
     (match (string-ref s x)
       [#\o (Some 'white)]
       [#\* (Some 'black)]
       [#\_ 'None]
       [_ (error "error in loading: file malformed")]))))

(check-expect (string->column "_o*")
              (vector 'None (Some 'white)(Some 'black)))

;; string->board converts a string into a board
(: string->board : String -> (Vectorof (Vectorof (Optional Stone))))
(define (string->board s)
  (local
    {(define split : (Listof String) (string-split s "|"))}
    (if (check-square split)
        (build-vector
         (length split)
         (lambda
             ([x : Integer])
           (string->column (list-ref split x))))
        (error "error in board storage - not square"))))

(check-expect (string->board (board->string (Go-board test-go)))
              (Go-board test-go))

(check-error
 (string->board (substring (board->string (Go-board test-go)) 0 14))
 "error in board storage - not square")

;; check-square checks to see if a board is square. If it is,
;; it returns true.
(: check-square : (Listof String) -> Boolean)
(define (check-square xs)
  (local
    {(define len : Integer (length xs))
     (: helper : (Listof String) -> Boolean)
     (define (helper ls)
       (match ls
         ['() #t]
         [(cons hd tl)(if (= (string-length hd) len)
                          (helper tl)
                          #f)]))}
    (helper xs)))

(check-expect
 (check-square
  (list
   "1234"
   "2341"
   "3412"
   "4123")) #t)

(check-expect
 (check-square
  (list
   "1234"
   "2341"
   "341"
   "4123")) #f)

(check-expect
 (check-square
  (list
   "1234"
   "2341"
   "4123")) #f)


;; string->hist converts a condensed history string into the a history.
(: string->hist : String -> (Listof Board))
(define (string->hist s)
  (local
    {(define split : (Listof String) (string-split s "!"))
     (: helper : (Listof String) -> (Listof Board))
     (define (helper bds)
       (match bds
         ['() '()]
         [(cons hd tl)
          (cons (string->board hd)(helper tl))]))}
    (if
     (check-consistent (helper split))
     (helper split)
     (error
      "error: malformed save file. Inconsistent board sizes in history"))))

(check-error
 (string->hist "_o|*_!_!__|__")
 "error: malformed save file. Inconsistent board sizes in history")


(check-expect
 (string->hist "_o|*_!_o|o_!__|__")
 (list
  (vector
   (vector 'None (Some 'white))
   (vector (Some 'black) 'None))
  (vector
   (vector 'None (Some 'white))
   (vector (Some 'white) 'None))
  (vector
   (vector 'None 'None)
   (vector 'None 'None))))

(check-error
 (string->hist "_o|*_!_o|o_!__|_")
 "error in board storage - not square")

;; check-consistent size checks to make sure all the boards in the history
;; are a consistent size.
(: check-consistent : (Listof Board) -> Boolean)
(define (check-consistent bds)
  (match bds
    ['() #t]
    [_
     (local
       {(define size : Integer (vector-length (first bds)))
        (: helper : (Listof Board) -> Boolean)
        (define (helper xs)
          (match xs
            ['() #t]
            [(cons hd tl)
             (if (= size (vector-length hd))
                 (helper tl)
                 #f)]))}
       (helper bds))]))

(check-expect
 (check-consistent
  (list
   (vector
    (vector 'None (Some 'white))
    (vector (Some 'black) 'None))
   (vector
    (vector 'None (Some 'white))
    (vector (Some 'white) 'None))
   (vector
    (vector 'None 'None)
    (vector 'None 'None))))
 #t)

(check-expect
 (check-consistent
  (list
   (vector
    (vector 'None))
   (vector
    (vector 'None (Some 'white))
    (vector (Some 'white) 'None))
   (vector
    (vector 'None 'None)
    (vector 'None 'None))))
 #f)







;; convert a text representation of an Integer to an Integer
;; raise an error if the string is not a number
;; return the integer part of the resulting number only
;; (this is intended only to be used with integers)
(: string->integer : String -> Integer)
(define (string->integer s)
  (local
    {(define conv : (U Complex False) (string->number s))}
    (if (complex? conv) 
        (exact-round (real-part conv))
        (error "string->integer: invalid integer"))))

(check-expect (string->integer "44") 44)

;; string->go converts a condensed go string back into a go.
(: string->go : String -> Go)
(define (string->go s)
  (local
    {(define split : (Listof String)(string-split s "~"))}
    (local
      {(define next : Stone
         (match (first split)
           ["*" 'black]
           ["o" 'white]
           [_ (error "malformed save file")]))
       (define board : Board
         (string->board (second split)))
       (define history : (Listof Board)
         (string->hist (third split)))
       (define conp : Integer 
         (string->integer (last split)))}
      (Go board next history 'None '() '() conp))))

(check-expect (string->go (go->string test-go)) test-go)
(check-error (string->go
              (substring (go->string test-go) 1
                         (string-length (go->string test-go))))
             "malformed save file")


;; string->world converts a condensed world string into a World
(: string->world : String BoardSpec -> World)
(define (string->world s bspec)
  (local
    {(define split : (Listof String) (string-split s "@"))}
    (local
      {(define wtenths : Integer (string->integer (first split)))
       (define btenths : Integer (string->integer (second split)))
       (define game : Go (string->go (last split)))}
      (World
       bspec
       game
       "Welcome back to Go!"
       btenths
       wtenths
       'None))))

(check-expect
 (string->world
  (world->string
   (World
    (BoardSpec "green" 20 10 7)
    (Go
     (vector
      (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
      (vector (Some 'white) (Some 'white) (Some 'white)(Some 'white))
      (vector 'None 'None 'None 'None)
      (vector 'None 'None 'None 'None))
     'black
     (list
      (vector
       (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
       (vector (Some 'white) (Some 'white) (Some 'white)(Some 'white))
       (vector 'None 'None 'None 'None)
       (vector 'None 'None 'None 'None)))
     'None
     '()
     '()
     1)
    "white passed! black to move"
    0 0 (Some (LogicalLoc 0 0))))
  (BoardSpec "green" 20 10 7 ))
 (World
  (BoardSpec "green" 20 10 7 )
  (Go
   (vector
    (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
    (vector (Some 'white) (Some 'white) (Some 'white)(Some 'white))
    (vector 'None 'None 'None 'None)
    (vector 'None 'None 'None 'None))
   'black
   (list
    (vector
     (vector (Some 'black) (Some 'black) (Some 'black) (Some 'black))
     (vector (Some 'white) (Some 'white) (Some 'white)(Some 'white))
     (vector 'None 'None 'None 'None)
     (vector 'None 'None 'None 'None)))
   'None
   '()
   '()
   1)
  "Welcome back to Go!"
  0 0 'None))
  

    
      
      
          
           
   




       
     
        

    
;; --------------------------------------------------------
;; 16-OPENING FILES
;; -------------------------------------------------------


;; prompt the user for an output file location
;; then, save the game to that file
;; do nothing if the user cancels
(: save-game! : World -> Void)
(define (save-game! w)
  (local
    {(define path : (U Path False) (put-file))}
    (if (path? path)
        (local
          {(define op : Output-Port (open-output-file path))}
          (begin (write-string (world->string w) op)
                 (close-output-port op)))
        (void))))


;; ask the user to choose a file
;; then load an in-progress game from that file
;; use the provided BoardSpec to make a new World
;; raise an error if the user cancels or if something goes wrong
(: load-game : BoardSpec -> World)
(define (load-game bs)
  (local
    {(define path : (U Path False) (get-file))}
    (if (path? path)
        (local
          {(define ip : Input-Port (open-input-file path))
           (define w : World
             (string->world  (port->string (open-input-file path)) bs))}
          (begin (close-input-port ip) w))
        (error "load-game: user cancelled"))))








;; --------------------------------------------------------
;; 17-RUNNING AND TESTING
;; -------------------------------------------------------

(test)
(play 19 (BoardSpec 'moccasin 20 10 7))