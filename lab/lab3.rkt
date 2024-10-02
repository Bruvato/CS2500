#lang htdp/bsl


(define-struct next [prev])
 ; An SNat (a struct-based natural number) is one of:
 ; - 0
 ; - (make-next SNat)
 
(define N0 0)
(define N1 (make-next N0))
(define N2 (make-next N1))
(define N3 (make-next N2))
 
 ; snat-temp : SNat -> ?
(define (snat-temp snat)
  (cond
    [(number? snat) ...]
    [(next? snat) (... (snat-temp (next-prev snat)) ...)]))

; 1 -------------------------------------------------------------

; A Nat is one of
; - 0
; - (add1 Nat)

(define (nat-temp nat)
  (cond [(zero? nat) ...]
        [(positive? nat) (... (nat-temp (sub1 nat )) ...)]))

; Sample Problem -------------------------------------------------------------

; zero?/snat : SNat -> Boolean
; Is the given SNat zero?
(define (zero?/snat snat)
  (and (number? snat) (zero? snat)))
 
; double : SNat -> SNat
; Doubles the SNat
(define (double snat)
  (cond
    [(zero?/snat snat) 0]
    [(next? snat) (make-next (make-next (double (next-prev snat))))]))
 
(check-expect (double N0) 0)
(check-expect (double N1) (make-next (make-next 0)))
(check-expect (double N2) (make-next (make-next (make-next (make-next 0)))))

; 2 -------------------------------------------------------------

; snat->nat : SNat -> Nat
; purposes
(check-expect (snat->nat (double N0)) 0)
(check-expect (snat->nat (double N1)) 2)
(check-expect (snat->nat (double N2)) 4)

(define (snat->nat snat)
  (cond [(number? snat) 0]
        [(next? snat) (add1 (snat->nat (next-prev snat)))]))

; 3 -------------------------------------------------------------

; even?/snat : SNat -> Boolean
; determines if a SNat is even
(check-expect (even?/snat N0) #true)
(check-expect (even?/snat N1) #false)
(check-expect (even?/snat N2) #true)
(check-expect (even?/snat N3) #false)

(define (even?/snat snat)
  (cond
    [(number? snat) #true]
    [(next? snat) (not (even?/snat (next-prev snat)))]))

; 4 -------------------------------------------------------------

; +/snat : Snat Snat -> Snat
; sums two snats
(check-expect (+/snat N0 N0) N0)
(check-expect (+/snat N0 N1) N1)
(check-expect (+/snat N1 N0) N1)
(check-expect (+/snat N1 N1) N2)

(define (+/snat snat1 snat2)
  (cond
    [(number? snat1) snat2]
    [(number? snat2) snat1]
    [(next? snat1) (... (+/snat (next-prev snat)) ...)]))