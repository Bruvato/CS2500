#lang htdp/isl

(require 2htdp/image)

(require 2htdp/universe)


; hw5b


; 1 ---------------------------------------------------------------

; A QBertBoard is a (make-qbertboard Number Number)
(define-struct qbertboard [level size])
; and represents the playing field of a particular game level where
; level is the is the level number
; size is the size of the board

; make-board : Number Number -> QBertBoard
; constructs a QBertBoard containing the initial state of that level
; takes a level number and a board size
(check-expect (make-board 1 1) (make-qbertboard 1 1))
(check-expect (make-board 2 5) (make-qbertboard 2 5))

(define (make-board level size)
  (make-qbertboard level size))

; 2 ---------------------------------------------------------------

; A QBertLevel is a (make-qbertlevel ... Number Board)
(define-struct qbertlevel[location steps board])
; and represents a all the information in a given level of the game where
; location
; steps is the number of steps QBert has taken
; board is the current board