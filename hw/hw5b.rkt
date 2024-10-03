#lang htdp/isl

(require 2htdp/image)

(require 2htdp/universe)



; hw5b


; 1 ---------------------------------------------------------------

; A Block is a (make-block Number Posn String)
(define-struct block [pos count color])
; and represents a block on a QBertBoard where
; - pos is a block's x and y position (with 0,0 at the bottom left)
; - count is the current count of the block
; - color is the color of the block

; Block Template
; block-temp : Block -> ?
(define (block-temp block)
  (... (posn-temp (block-pos block))
       (block-count block)  
       (block-color block) ...))

; Block Examples
(define BLOCK0 (make-block (make-posn 0 0) 0 "red"))
(define BLOCK1 (make-block (make-posn 1 0) 1 "orange"))


; A LOB is one of
; '()
; (cons Block LOB)
; and represents a list of blocks

; LOB Template
; lob-temp : LOB -> ?
(define (lob-temp lob)
  (cond
    [(empty? lob) ...]
    [(cons? lob)
     (... (first (block-temp lob))
          (lob-temp (rest lob)) ...)]))

; LOB Examples
(define LOB0 '())
(define LOB1 (cons BLOCK1 LOB0))


; A QBertBoard is a (make-qbertboard Number Number LOB)
(define-struct qbertboard [level size lob])
; and represents the playing field of a particular game level where
; - level is the is the level number
; - size is the size of the board
; - lob is a list of the blocks of the board

; QBertBoard Template
; qbertboard-temp : QbertBoard -> ?
(define (qbertboard-temp qbertboard)
  (... (qbertboard-level qbertboard)
       (qbertboard-size qbertboard)
       (lob-temp (qbertboard-lob qbertboard)) ...))

; QBertBoard Examples
(define QB1 (make-qbertboard 1 1 LOB0))
(define QB2 (make-qbertboard 2 5 LOB0))







; make-board : Number Number -> QBertBoard
; constructs a QBertBoard containing the initial state of that level
; takes a level number and a board size
(check-expect (make-board 1 1) QB1)
(check-expect (make-board 2 5) QB2)

(define (make-board level size)
  (make-qbertboard level
                   size
                   ))

; 2 ---------------------------------------------------------------

; A QBertLevel is a (make-qbertlevel Posn Number QBertBoard)
(define-struct qbertlevel[pos steps board])
; and represents all the information in a given level of the game where
; - pos is Qbert's x and y position (with 0,0 at the bottom left)
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

(define (all-blocks-zero? qbertboard)
  (local [
          (define lob
            (qbertboard-lob qbertboard))]
    (cond
      [(empty? lob) ...]
      [(cons? lob)
       (... (first lob)
            (lob-temp (rest lob)) ...)])))


    