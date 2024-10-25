#lang htdp/isl+

;midterm smallest
#;(define (smallest grid lt?)
    (largest grid (λ(x y) (not (it? x y)))))

; ----- chooser ------

; A Chooser is a {X} [X X -> X]

(define chooser1 (λ(x1 x2) x1))
(define chooser2 (λ(x1 x2) x2))

(define (chooser->bool chooser)
  (chooser #t #f))
; if chooser1 choose first = #t
; if chooser2 choose 2nd = #f

; Chooser Chooser -> Chooser
(define (and/chooser c1 c2)
  ;(c1 (c2 chooser1 chooser2) chooser2) ; if c1 true : pick 1st
  (c1 c2 c1))

(define (or/chooser c1 c2)
  (c1 c1 c2))

(define (not/chooser c)
  ;(c chooser2 chooser1)
  (λ(x1 x2) (c x2 x1))) ; same idea as smallest

; ===============================================
; more trees
; =====

; An [NaryTree-of X] is a (make-node X [Forest-of X])

; A [Forest-of X] is a [List-of [NaryTree-of X]]



; An Atom is one of:
; - Number
; - Boolean
; - String
; - Symbol

; A S-expression is one of
; - Atom
; - [List-of S-expression]

(list '+ 3 4)
'(+ 3 4)

; ----------------------------

; how many times does an atom a appear in S-exp ex?

; count-of-atom : Atom S-exp -> Natural
(define (count-of-atom a ex)
  (cond [(atom? ex) (if (atom=? a ex) 1 0)] ; atom takes in two atoms, so we confirme ex is an atom
        [(list? ex) (count-of-atom-in-list a ex)]))

(define (atom=? a1 a2)
  (cond [(and (number? a1) (number? a2)) (= a1 a2)]
        [(and (boolean? a1) (boolean? a2)) (boolean=? a1 a2)]
        [(and (string? a1) (string? a2)) (string=? a1 a2)]
        [(and (symbol? a1) (symbol? a2)) (symbol=? a1 a2)]
        [else #f]))

; count-of-atom-in-list : Atom [List-of S-exp] -> Nat
(define (count-of-atom-in-list a ex)
  (cond [(empty? ex) 0]
        [(cons? ex) (+ (count-of-atom a (first ex))
                       (count-of-atom-in-list a (rest ex)))]))

; ---------------------

; 
         
        




                    