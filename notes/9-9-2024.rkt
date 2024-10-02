#lang htdp/bsl

; A grade is s Number
(define (grade-temp g)
  (...))

; A TrafficLight is one of
; "red"
; "yellow"
; "green"

(define (tl-temp tl)
  (cond [(string=? tl "red") ...]))

; A Roll is one of
; 1
; 2
; 3 ...

(define (dice-roll-temp r)
  (cond [(= r 1) ...]))

; -------------------------




; struct is an answer
(define my-point (make-posn 3 4))

(define me (make-posn "Ben" "lerner"))


; A position is a (make-posn Number Number)

(define (position-temp p)
  (... (posn-x p)...(posn-y p)...))

; quad1? Position -> Boolean
; determines if the given position p is in quadrant one (including axes)
(define (quad1? p)
  (and (>= (posn-x p) 0) (>= (posn-y p) 0)))



(define (on-diagonal p)
  (= (posn-x p) (posn-y p)))


; -----Data-----
; 1 Data Definition
; A Position is a (make-posn Number Number)

; 2 Interpretation
; A (make-posn x y) represents the cartesian point (x,y) with +x right and +y up

; 3 Examples
(define POS-1-1 (make-posn 1 1))
(define ORIGIN (make-posn 0 0))


; -----Function-----
; 1 Signature
; quad1: Position -> Boolean

; 2  Purpose Statement
; Determines if given point is in Quadrant 1 (including axes)

; 3 Tests
(check-expect (quad1? (make-posn 3 4)) #true)






; -----structs-----

; A Student is a (make-student String String Number Boolean)
(define-struct student [first-name last-name gpa on-coop])
; 1. make-student
; 2. student-first-name
; 3. student-last-name
; 4. student-gpa
; 5. student-on-coop
; 6. student?

; Examples
(define RX (make-student "Raymond" "Xu" 4 #true))

; Template
(define (student-temp s)
  (...(student-first-name s)
      ...(student-last-name s)
      ...(student-gpa s)
      ...(student-on-coop s)))
