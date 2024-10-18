#lang htdp/isl+

(require "mimic.rkt")

; A [Counter X] is a [Mapping X PosInt]
; and represents a multiset (a set of elements where an element can appear more than once)
 
(define MARBLE-BAG (list (make-pair "green" 2) (make-pair "red" 5)))
; MARBLE-BAG represents a bag with 2 "green" marbles and 5 "red" ones


; add-to-counter : [Counter X] X -> [Counter X]
; Add 1 to x in c
(define (add-to-counter c x)
  (update-mapping c x add1 1))
(check-expect (add-to-counter MARBLE-BAG "green") (list (make-pair "green" 3)
                                                        (make-pair "red" 5)))
(check-expect (add-to-counter MARBLE-BAG "brown") (list (make-pair "green" 2)
                                                        (make-pair "red" 5)
                                                        (make-pair "brown" 1)))