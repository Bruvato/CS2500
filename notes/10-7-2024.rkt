#lang htdp/isl+

; 10-7-2024
; ---------
; λ
; Sets
; Equality
; ---------


#;(define (make-adder x)
  (local (
          (define (the-adder y) (+ x y)))
    the-adder))

; λ : isl+ : ctrl \

(define (make-adder x)
  (λ(y) (+ x y)))


(define add-five (make-adder 5))
(check-expect (add-five 10) 15)

(define add-one (make-adder 1))
(check-expect (add-one 1) 2)

(define (add-n-to-all n lon)
  (map (λ(y) (+ n y)) lon)) ; no signature or purpose

(check-expect (add-n-to-all 1 '(1 2 3 4 5)) '(2 3 4 5 6))

; ---

((lambda (f) (f 5)) (lambda (y) (+ y 10))) ; (F1 F2)
; ((λ(y) (+ y 10)) 5)
; > (+ 5 10)
; > 15


((lambda (f g h) (f g)) ; [[ [Number -> Number] -> Number] [Number -> Number] [Number -> Number] -> Number]
 (lambda (f) (f 10)) ; {X} [ [Number -> X] -> X]
 (lambda (x) (+ x 1)) ; [Number -> Number]
 (lambda (x) (+ x 2))) ; [Number -> Number]

; (f g)
; ((lambda (f) (f 10)) (lambda (x) (+ x 1)))
; ((lambda (x) (+ x 1) 10)
; (+ 10 1)
; 11



(((lambda (f)
    (lambda (x) (f x)))
  (lambda (x) (+ x 10)))
 100)
; 110



;  ------
(define (apply-to-10 f) (f 10))
(define (my-compose f g) (λ(x) (f (g x))))
(foldr my-compose 10 (list even? sqr add1 add1))
; an XtoY is a [X -> Y] (data def)

(define (f1 x y z)
           ...)
(define f2 (λ(x y z)
            ...))

; ------

(define A (lambda (x y) x))
(define B (lambda (x y) y))

(define C (lambda (b1 b2) (b1 A b2)))

(C A A)
(C B B)
(C A B)
(C B A)


; ==========SETS==================

; A Set of X is a [List-of X]
; where repeats are prohibeted and order doesn't matter

; contains?, union, intesec, complement, size


(define (contains? set element)
  (ormap (λ(x) (equal? x element)) set))
        
(define (intersection set1 set2)
  (filter (λ(elem1) (contains? set2 elem1)) set1))


(define (subset? set1 set2)
  ...)

(define (same? set1 set2) ; same? =/= equal?
  (and (subset? set1 set2)
       (subset? set2 set1)))

; ---

; A [Setof X] is a function [X -> Boolean]
; that returns true when the argument is in the set

(define empty-set (λ(dontcare) #f))

(define (union set1 set2)
  (λ(elem)
    (or (set1 elem)
        (set2 elem))))

(define even-nums even?)
