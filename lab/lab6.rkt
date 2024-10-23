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
; total-size : [Counter X] -> Number
; The total size of c
(define (total-size c)
  (foldr (λ(p acc) (+ (pair-y p) acc)) 0 c))

(check-expect (total-size MARBLE-BAG) 7)


; initiate-counter : X -> Counter
;
(check-expect (initiate-counter "blue") (list (make-pair "blue" 1)))

(define (initiate-counter x)
  (list (make-pair x 1)))


; expected-counts : [Counter X] Nat -> [List-of Number]
; Expected counts of elements when grabbing from the counter n times
(check-expect (expected-counts '() 100) '())
(check-expect (expected-counts MARBLE-BAG 1000)
              (list (* 2/7 1000) (* 5/7 1000)))

(define (expected-counts c n)
  (local [(define total (total-size c))]
    (map (λ(p) (* (/ (pair-y p) total) n)) c)))


; count : [List-of X] X -> Nat
; How many times does x appear in the list?
(check-expect (count '() "b") 0)
(check-expect (count (list "a" "b" "a") "a") 2)

(define (count l x)
  (foldr (λ(elem acc) (if (eq? elem x)
                          (add1 acc)
                          acc)) 0 l))

; count-grabs : [Counter X] [List-of X] -> [List-of Number]
; See how many times the elements from this counter are in this list
(check-expect (count-grabs '() '()) '())
(check-expect (count-grabs MARBLE-BAG (list "red" "green" "red" "red")) (list 1 3))

(define (count-grabs c l)
  (map (λ(p) (count l (pair-x p))) c))

; grab-random : [Counter X] -> X
; Randomly grab from this counter
(define (grab-random c)
  (local (; grab : Nat [Counter X] -> X
          ; Grab the first element in c if its count is larger than n,
          ; otherwise reduce n by the count and recur
          (define (grab n c)
            (cond [(< n (pair-y (first c))) (pair-x (first c))]
                  [else (grab (- n (pair-y (first c))) (rest c))])))
    (grab (random (total-size c)) c)))



; grab-n : [Counter X] Nat -> [List-of X]
; Grab from the counter n times
(check-within (count-grabs MARBLE-BAG (grab-n MARBLE-BAG 10000))
              (expected-counts MARBLE-BAG 10000)
              100)
(define (grab-n c n)
  (build-list n (λ(x) (grab-random c))))

         