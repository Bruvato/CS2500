#lang htdp/isl

(require 2htdp/image)

(require 2htdp/universe)



; hw5b


; 1 ---------------------------------------------------------------

; *****
; A Coord is a (make-coord Integer Integer)
(define-struct coord [row col])
; and represents a coordinate on a QBertBoard

; Coord Template
; coord-temp : Coord -> ?
(define (coord-temp coord)
  (... (coord-row coord)
       (coord-col coord) ...))

; Coord Examples
(define COORD-0-0 (make-coord 0 0)) ; top of pyramid
(define COORD-2-1 (make-coord 2 1)) ; third row, second col

; *****
; A Block is a Natural
; and represents the count of a block on a QBertBoard

; Block Template
; block-temp : Block -> ?
(define (block-temp block)
  ...)

; Block Examples
(define BLOCK0 0) ; a block that has been cleared
(define BLOCK1 1) ; a block that needs one more jump

; *****
; A LOB is one of
; - '()
; - (cons Block LOB)
; and represents a list of blocks

; LOB Template
; lob-temp : LOB -> ?
(define (lob-temp lob)
  (cond
    [(empty? lob) ...]
    [(cons? lob)
     (... (first lob)
          (lob-temp (rest lob)) ...)]))

; LOB Examples
(define LOB0 '())
(define LOB1 (cons BLOCK1 LOB0))

; *****
; A QBertBoard is a (list (list Block))
; and represents a list of list of blocks
; (playing field of a particular game level)

; QBertBoard Template
; qbertboard-temp : QbertBoard -> ?
(define (qbertboard-temp qbertboard)
  (cond
    [(empty? qbertboard) ...]
    [(cons? qbertboard)
     (... (lob-temp (first qbertboard))
          (qbertboard-temp (rest qbertboard)) ...)]))

; QBertBoard Examples
(define QB1 (list (list 1)
                  (list 1 1)
                  (list 1 1 1))) ; a 3x3 board for level 1
(define QB2 (list (list 2)
                  (list 2 2)
                  (list 2 2 2)
                  (list 2 2 2 2))) ; a 4x4 board for level 2


; *****
; make-board : Number Number -> QBertBoard
; constructs a QBertBoard containing the initial state of that level
; takes a level number and a board size
(check-expect (make-board 1 3) QB1)
(check-expect (make-board 2 4) QB2)



(define (make-board level size)
  (local [; count : X -> Number
          ; gives the starting count of each block
          (define (count x)
            level)
          ; create-row : Number -> [List-of Number]
          ; creates a row of blocks with a count of level
          (define (create-row row)
            (build-list (add1 row) count))]
    
    (build-list size create-row) ))

                   

; 2 ---------------------------------------------------------------

; A QBertLevel is a (make-qbertlevel Number Coord Number QBertBoard)
(define-struct qbertlevel[level coord steps board])
; and represents all the information in a given level of the game where
; - level is the current level
; - coord is Qbert's x and y position (with 0,0 at the bottom left)
; - steps is the number of steps QBert has taken
; - board is the current board

; QBertLevel Template
; qbertlevel-temp : QBertLevel -> ?
(define (qbertlevel-temp qbertlevel)
  (... (posn-temp (qbertlevel-pos qbertlevel))
       (qbertlevel-steps qbertlevel)
       (qbertboard-temp (qbertlevel-board qbertlevel)) ...))

; QBertLevel Examples
(define QL1 (make-qbertlevel (make-posn 0 0) 0 QB1))
(define QL2 (make-qbertlevel (make-posn 1 2) 3 QB2))


; 3 ---------------------------------------------------------------

; move-qbert : QBertLevel String -> QBertLevel
; moves QBert in any of the four diagonal directions
(check-expect (move-qbert QL1 "up")
              (make-qbertlevel (make-posn 0 1) 1 QB1))
(check-expect (move-qbert QL2 "right")
              (make-qbertlevel (make-posn 2 1) 4 QB2))


(define (move-qbert qbertlevel dir)
  (local [; new-pos QBertLevel String -> Posn
          ; computes the new position of QBert
          (define new-pos
            (cond
              [(string=? dir "up") (posn-add (qbertlevel-pos qbertlevel) (make-posn 0 1))]
              [(string=? dir "down") (posn-add (qbertlevel-pos qbertlevel) (make-posn 0 -1))]
              [(string=? dir "left") (posn-add (qbertlevel-pos qbertlevel) (make-posn -1 1))]
              [(string=? dir "right") (posn-add (qbertlevel-pos qbertlevel) (make-posn 1 -1))]))]
    (make-qbertlevel new-pos
                     (add1 (qbertlevel-steps qbertlevel))
                     (qbertlevel-board qbertlevel))))

              
; posn-add : Posn Posn -> Posn
; adds two Posns (component wise)
(check-expect (posn-add (make-posn 1 2 ) (make-posn 3 4)) (make-posn 4 6))
(check-expect (posn-add (make-posn -1 2 ) (make-posn 3 -4)) (make-posn 2 -2))

(define (posn-add p1 p2)
  (make-posn (+ (posn-x p1) (posn-x p2))
             (+ (posn-y p1) (posn-y p2))))

; 4 ---------------------------------------------------------------

; level-won? : QBertLevel -> Boolean
; determines whether QBert has won the current QBertLevel
(check-expect (level-won? 0) 0)
               
(define (level-won? qbertlevel)
  (qbertboard-temp (qbertlevel-board qbertlevel)))

; all-blocks-zero? : QBertBoard -> Boolean
; determines whether the count of each and every block on a QBertBoard is 0
(check-expect (all-blocks-zero? 0) 0)

#;(define (all-blocks-zero? qbertboard)
  (local [
          (define lob
            (qbertboard-lob qbertboard))]
    (cond
      [(empty? lob) ...]
      [(cons? lob)
       (... (first lob)
            (lob-temp (rest lob)) ...)])))


    