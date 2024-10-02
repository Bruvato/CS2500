#lang htdp/bsl

; A ListofPosns LOP is one of
; - '()
; (cons Posn LOP)

(define (lop-temp lop)
  (cond [(empty? lop) ...]
        [(cons? lop)
         (...(posn-temp (first lop))
             (lop-temp (rest lop))...)]))

(define lop0 '())
(define lop1 (cons (make-posn 1 1) lop0))
(define lop2 (cons (make-posn 3 4) lop1))
(define lop3 (cons (make-posn 2 0) lop2))

; max-dist-to-origin : LOP -> NonnegNumber
; purpose
(check-expect (max-dist-to-origin lop3) 5)

(define (max-dist-to-origin lop)
  (cond [(empty? lop) 0]
        [(cons? lop)
         (max (dist-to-origin (first lop))
              (max-dist-to-origin (rest lop)))]))

(define (dist-to-origin p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

; ----------------------------------------------

; Add Togethor all Posns in the given LOP

(define ORIGIN (make-posn 0 0))
; add-posns : LOP -> Posn
; purpose
(define (add-posns lop)
  (cond [(empty? lop) ORIGIN]
        [(cons? lop)
         (combine-posn (first lop)
                       (add-posns (rest lop)))]))

; combine-posn : Posn Posn -> Posn
; purpose
(define (combine-posn p1 p2)
  (make-posn
   (+ (posn-x p1) (posn-x p2))
   (+ (posn-y p1) (posn-y p2))))