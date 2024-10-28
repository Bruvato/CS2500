#lang htdp/isl+

; An Op is a symbol of an arithmetic operator


; op? : Any -> Boolean
; determines whether argument is in fact an Op
(define (op? s)
  (member s '(+ - * /)))
 
(define-struct e-op [op left right])
(define-struct e-in [])
 
; An Exp is one of
; - Number
; - (make-e-op Op Exp Exp)
; - (make-e-in)
; and represents an expression that a calculator might process: either a
; single number, an operator applied to subexpressions, or user input.
 
(define E0 3)
(define E1 (make-e-op '+ (make-e-op '- 1 (make-e-op '+ 2 2))
                      (make-e-op '* 3 (make-e-op '/ 3 4))))
(define E2 (make-e-op '+ 1 (make-e-op '- 2 3)))
(define E3 (make-e-op '+ (make-e-in) 2))


; An S-Exp is one of
; - Atom
; - LoSExp

(define S0 3)
(define S1 '(+ (- 1 (+ 2 2)) (* 3 (/ 3 4))))
(define S2 '(+ 1 (- 2 3)))
(define S3 '(+ (input) 2))

; s-exp-temp : S-Exp -> ?
(define (s-exp-temp s-exp)
  (cond [(atom? s-exp) (... (atom-temp s-exp) ...)]
        [(list? s-exp) (...(los-temp s-exp) ... )]))


; An Atom is one of
; - Number
; - String
; - Boolean
; - Symbol

(define A0 3)
(define A1 "hi")
(define A2 #t)
(define A3 'a)

; atom-temp : Atom -> ?
(define (atom-temp atom)
  (cond [(number? atom) ...]
        [(string? atom) ...]
        [(boolean? atom) ...]
        [(symbol? atom) ...]))


; An LoSExp is a [List-of S-Exp]

(define LOS0 '())
(define LOS1 '(1))
(define LOS2 '(1 2 3))
(define LOS3 '('a 'b 'c))
(define LOS4 '("apple" "banana" "orange"))
(define LOS5 '(0 (#t #f) "hi" ('a)))
(define LOS6 '(#f (3 ("hi" ('a)))))

(define (los-temp los)
  (cond [(empty? los) ...]
        [(cons? los) (... (s-exp-temp (first los))
                          (los-temp (rest los)) ...)]))


; atom? : Any -> Boolean
(define (atom? x)
  (or (number? x)
      (string? x)
      (boolean? x)
      (symbol? x)))

(check-expect (atom? A0) #t)
(check-expect (atom? A1) #t)
(check-expect (atom? A2) #t)
(check-expect (atom? A3) #t)
(check-expect (atom? '(1 2 3)) #f)


; parse : S-Exp -> Exp
; pareses a S-Exp to produce a Exp
(define (parse s)
  (cond [(atom? s) (if (or (number? s) (symbol? s)) s (error "Invalid S-Exp"))]
        [(list? s) (parse-los s)]))

(define (parse-los los)
  (cond [(empty? los) ...]
        [(cons? los) (if (= (length los) 3)
                         (make-
                         (parse (first los))
                         (parse-los (rest los))

(check-expect (parse S0) E0)
(check-expect (parse S1) E1)
(check-expect (parse S2) E2)
(check-expect (parse S3) E3)
(check-error (parse #t))
(check-error (parse '(+ 1 hello)))



