#lang htdp/isl

(require 2htdp/image)

(require 2htdp/universe)


; hw5b


; 1 ---------------------------------------------------------------

; A QBertBoard is a (make-qbertboard Number Number)
(define-struct qbertboard [level size])
; and represents the playing field of a particular game level where
; - level is the is the level number
; - size is the size of the board

; QBertBoard Template
; qbertboard-temp : QbertBoard -> ?
(define (qbertboard-temp qbertboard)
  (... (qbertboard-level qbertboard)
       (qbertboard-size qbertboard) ...))

; QBertBoard Examples
(define QB1 (make-qbertboard 1 1))
(define QB2 (make-qbertboard 2 5))


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
     (... (first lob)
          (lob-temp (rest lob)) ...)]))

; LOB Examples
(define LOB0 '())
(define LOB1 (cons B1 LOB0))


; A Block is a (make-block Number String)
(define-struct block [count color])
; and represents a block on a QBertBoard where
; - count is the current count of the block
; - color is the color of the block

; Block Template
; block-temp : Block -> ?
(define (block-temp block)
  (... (block-count block)
       (block-color block) ...))

; Block Examples
(define B0 (make-block 0 "red"))
(define B1 (make-block 1 "orange"))




; make-board : Number Number -> QBertBoard
; constructs a QBertBoard containing the initial state of that level
; takes a level number and a board size
(check-expect (make-board 1 1) QB1)
(check-expect (make-board 2 5) QB2)

(define (make-board level size)
  (make-qbertboard level size))

; 2 ---------------------------------------------------------------

; A QBertLevel is a (make-qbertlevel Posn Number Board)
(define-struct qbertlevel[location steps board])
; and represents all the information in a given level of the game where
; - location
; - steps is the number of steps QBert has taken
; - board is the current board

; QBertLevel Template
(define (qbertlevel-temp qbertlevel)
  (... (qbertlevel-location qbertlevel)
       (qbertlevel-steps qbertlevel)
       (qbertlevel-board qbertlevel) ...))

; QBertLevel Examples
;(define QL (make-qbertleve

; 3 ---------------------------------------------------------------

; move-qbert : QBertLevel Posn -> _
(define (move-qbert qbertlevel dir)
  ...)