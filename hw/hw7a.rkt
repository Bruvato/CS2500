#lang htdp/isl+

; An Alternator is a {X} [X X -> X]
; Interpretation: to be determined below...

; 1 ----------------------------------------

; alternator1: {X} X X -> X
(define (alternator1 x1 x2)
  x1)

(check-expect (alternator1 1 2) 1)
(check-expect (alternator1 "a" "b") "a")

; alternator2: {X} X X -> X
(define (alternator2 x1 x2)
  x2)

(check-expect (alternator2 1 2) 2)
(check-expect (alternator2 "a" "b") "b")

; 2 ----------------------------------------

; alternator->boolean: Alternator -> Boolean
; Converts a Alternator to a Boolean
(define (alternator->boolean alternator)
  (alternator #t #f))

(check-expect (alternator->boolean alternator1) #t)
(check-expect (alternator->boolean alternator2) #f)

; 3 ----------------------------------------

; and/alt: Alternator Alternator -> Alternator
; Functions analogously to and for Alternators
(define (and/alt a1 a2)
  (a1 a2 a1))

(check-expect (alternator->boolean (and/alt alternator1 alternator1)) #true)
(check-expect (alternator->boolean (and/alt alternator2 alternator1)) #false)
(check-expect (alternator->boolean (and/alt alternator1 alternator2)) #false)
(check-expect (alternator->boolean (and/alt alternator2 alternator2)) #false)

; 4 ----------------------------------------

; or/alt: Alternator Alternator -> Alternator
; Functions analogously to or for Alternators
(define (or/alt a1 a2)
  (a1 a1 a2))

(check-expect (alternator->boolean (or/alt alternator1 alternator1)) #true)
(check-expect (alternator->boolean (or/alt alternator2 alternator1)) #true)
(check-expect (alternator->boolean (or/alt alternator1 alternator2)) #true)
(check-expect (alternator->boolean (or/alt alternator2 alternator2)) #false)

; 5 ----------------------------------------

; not/alt: Alternator -> Alternator
; Functions analogously to not for Alternators
(define (not/alt alt)
  (λ (x1 x2) (alt x2 x1)))

(check-expect (alternator->boolean (not/alt alternator1)) #false)
(check-expect (alternator->boolean (not/alt alternator2)) #true)

; 6 ----------------------------------------

; two: {X} [X -> X] -> [X -> X]
; Creates a function that applies f to the output of f
(define (two f)
  (λ (x) (f (f x))))

(check-expect ((two add1) 0) 2)
(check-expect ((two sqr) 2) 16)

; 7 ----------------------------------------

; three: {X} [X -> X] -> [X -> X]
; Creates a function that applies f to the output of f of f
(define (three f)
  (λ (x) (f (f (f x)))))

(check-expect ((three add1) 0) 3)
(check-expect ((three sqr) 2) 256)

; 8 ----------------------------------------

; one: {X} [X -> X] -> [X -> X]
; Creates a function that is equivalent to f
(define (one f)
  (λ (x) (f x)))

(check-expect ((one add1) 0) 1)
(check-expect ((one sqr) 2) 4)

; zero: {X} [X -> X] -> [X -> X]
; Creates a function that produces its input
(define (zero f)
  (λ (x) x))

(check-expect ((zero add1) 0) 0)
(check-expect ((zero sqr) 2) 2)

; 9 ----------------------------------------

; A Repeater is a {X} [X -> X] -> [X -> X]
; It is a function that, when given a one-argument function f, outputs a
; new function that will repeatedly apply f some specific number of times
; to some argument (which will we'll often call x).

; rep->nat: Repeater -> Nat
; Produces the number of times a repeater repeats its argument
(define (rep->nat repeater)
  ((repeater add1) 0))

(check-expect (rep->nat zero) 0)
(check-expect (rep->nat one) 1)
(check-expect (rep->nat two) 2)
(check-expect (rep->nat three) 3)
(check-expect (rep->nat (λ (f) (λ (x) ((three f) ((two f) x))))) 5)

; 10 ----------------------------------------

; rep-add1: Repeater -> Repeater
; Increments the number of times a repeater repeats by one
(define (rep-add1 repeater)
  (λ (f) (λ (x) (f ((repeater f) x)))))

(check-expect (rep->nat (rep-add1 zero)) 1)
(check-expect (rep->nat (rep-add1 two)) 3)

; 11 ----------------------------------------

; nat->rep: Nat -> Repeater
; Creates a repeater that repeats n times
(define (nat->rep n)
  (cond
    [(zero? n) (λ (f) (λ (x) x))]
    [(positive? n) (rep-add1 (nat->rep (sub1 n)))]))

(check-expect (rep->nat (nat->rep 0)) 0)
(check-expect (rep->nat (nat->rep 1)) 1)
(check-expect (rep->nat (nat->rep 20)) 20)

; 12 ----------------------------------------

; rep+: Repeater Repeater -> Repeater
; Creates a repeater that repeats the sum of the number of times the two repeaters repeat
(define (rep+ repeater1 repeater2)
  (λ (f) (λ (x) ((repeater1 f) ((repeater2 f) x)))))

(check-expect (rep->nat (rep+ zero zero)) 0)
(check-expect (rep->nat (rep+ two three)) 5)

; 13 ----------------------------------------

; rep*: Repeater Repeater -> Repeater
; Creates a repeater that repeats the product of the number of times the two repeaters repeat
(define (rep* repeater1 repeater2)
  (λ (f) (λ (x) ((repeater1 (repeater2 f)) x))))

(check-expect (rep->nat (rep* zero zero)) 0)
(check-expect (rep->nat (rep* two three)) 6)

; 14 ----------------------------------------

; rep^: Repeater Repeater -> Repeater
; Creates a repeater that repeats the exponent of the number of times the two repeaters repeat
(define (rep^ repeater1 repeater2)
  (repeater2 (λ (f) (λ (x) ((repeater1 f) x)))))

(check-expect (rep->nat (rep^ zero zero)) 1)
(check-expect (rep->nat (rep^ two three)) 8)
