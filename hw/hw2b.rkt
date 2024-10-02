#lang htdp/bsl

; 1 ---------------------------------------------------------

; A Position is one of:
; - "origin"
; - PositiveNumber
; - Posn
; INTERP:
; - "origin" is the center for our coordinate system
; - PositiveNumber is a distance on the +x axis
; - Posn is any Cartesian coordinate

; position-temp: Position -> ?
(define (position-temp p)
  (cond [(string? p) ...] ; only string is "origin"
        [(number? p) ...] ; only number is positive number
        [(posn? p) (... (posn-temp p) ...)]))

; distance-to-0: Position -> Number
; Computes the distance from a Position to the origin
(define (distance-to-0 p)
  (cond [(string? p) 0]
        [(number? p) p]
        [(posn? p) (dist-to-0 p)]))

(check-expect (distance-to-0 "origin") 0)
(check-expect (distance-to-0 5) 5)
(check-expect (distance-to-0 (make-posn 3 4)) 5)

; dist-to-0: Posn -> Number
; Computes the distance form a Posn to the origin
(define (dist-to-0 p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

(check-expect (distance-to-0 (make-posn 0 0)) 0)
(check-expect (distance-to-0 (make-posn 0 5)) 5)
(check-expect (distance-to-0 (make-posn 3 4)) 5)

; 2 ---------------------------------------------------------

; manhattan-distance: Posn -> Number
; Copmutes the Manhattan distance of a given Posn p to the origin
(define (manhattan-distance p)
  (+ (abs (posn-x p)) (abs (posn-y p))))

(check-expect (manhattan-distance (make-posn 0 0)) 0)
(check-expect (manhattan-distance (make-posn 0 5)) 5)
(check-expect (manhattan-distance (make-posn 3 4)) 7)
(check-expect (manhattan-distance (make-posn -3 -4)) 7)



; 3 ---------------------------------------------------------

; usd->nzd: Number -> Number
; converts a value x in US Dollars to New Zealand Dollars (x >= 0)
(define (usd->nzd x)
  (cond [(< x 5) 0]
        [else (- (/ (* x 8) 5) 8)]))

(check-expect (usd->nzd 3) 0)
(check-expect (usd->nzd 10) 8)
(check-expect (usd->nzd 15) 16)
(check-expect (usd->nzd 20) 24)
(check-expect (usd->nzd 25) 32)


; 4 ---------------------------------------------------------

; leap-year?: NaturalNumber -> Boolean
; Determines if a natural number n represents a leap year in the Gregorian Calendar
(define (leap-year? n)
  (and (= (remainder n 4) 0) (or (not (= (remainder n 100) 0)) (= (remainder n 400) 0))))

(check-expect (leap-year? 0) #true)
(check-expect (leap-year? 123) #false)
(check-expect (leap-year? 1600) #true)
(check-expect (leap-year? 2000) #true)
(check-expect (leap-year? 1700) #false)
(check-expect (leap-year? 1800) #false)