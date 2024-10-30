#lang htdp/isl+

; An Op is a symbol of an arithmetic operator

(define OP0 '+)
(define OP1 '*)
(define OP2 '-)

(define (op-temp op)
  (... op ...))

; op? : Any -> Boolean
; determines whether argument is in fact an Op
(define (op? s)
  (member s '(+ - * /)))

 
; An Exp is one of
; - Number
; - (make-e-op Op Exp Exp)
(define-struct e-op [op left right])
; - (make-e-in)
(define-struct e-in [])
; and represents an expression that a calculator might process: either a
; single number, an operator applied to subexpressions, or user input.
 
(define E0 3)
(define E1 (make-e-op '+ (make-e-op '- 1 (make-e-op '+ 2 2)) (make-e-op '* 3 (make-e-op '/ 3 4))))
(define E2 (make-e-op '+ 1 (make-e-op '- 2 3)))
(define E3 (make-e-op '+ (make-e-in) 2))

; exp-temp : Exp -> ?
(define (exp-temp exp)
  (cond [(number? exp) ...]
        [(e-op? exp) (... (op-temp (e-op-op exp))
                          (exp-temp (e-op-left exp))
                          (exp-temp (e-op-right exp)) ...)]
        [(e-in? exp) ...]))


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
        [(list? s-exp) (... (los-temp s-exp) ...)]))


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


; 1 -------------------------------------

; parse : S-Exp -> Exp
; pareses a given S-Exp to produce a Exp
(define (parse s)
  (cond [(atom? s) (if (number? s) s (error "Invalid S-Exp"))]
        [(list? s) (parse-los s)]))

(check-expect (parse S0) E0)
(check-expect (parse S1) E1)
(check-expect (parse S2) E2)
(check-expect (parse S3) E3)
(check-expect (parse '(+ 1 2)) (make-e-op '+ 1 2))

(check-error (parse #t))
(check-error (parse "hi"))
(check-error (parse '(+ 1 hello)))
(check-error (parse '+))
(check-error (parse '(+)))

; parse-los : [List-of S-Exp] > Exp
; parses a given List-of S-Exp to produce a Exp
(define (parse-los los)
  (cond [(empty? los) '()]
        [(cons? los) (if (and (= (length los) 3) (op? (first los)))
                         (local [(define op (first los))
                                 (define exp1 (parse (second los)))
                                 (define exp2 (parse (third los)))]
                           (make-e-op op exp1 exp2))
                         (if (and (= (length los) 1) (symbol=? (first los) 'input))
                             (make-e-in)
                             (error "Invalid S-Exp")))]))

(check-expect (parse-los S1) E1)
(check-expect (parse-los S2) E2)
(check-expect (parse-los S3) E3)
(check-expect (parse-los '(+ 1 2)) (make-e-op '+ 1 2))




; 2 ---------------------------------------------

; eval-op : {X} Op -> [X X -> X]
; gives the corresponding operation to a given Op
(define (eval-op op)
  (cond [(symbol=? op '+) +]
        [(symbol=? op '-) -]
        [(symbol=? op '*) *]
        [(symbol=? op '/) /]))

(check-expect ((eval-op '+) 1 1) 2)
(check-expect ((eval-op '-) 5 2) 3)
(check-expect ((eval-op '*) 2 3) 6)
(check-expect ((eval-op '/) 4 2) 2)

; eval: Number Exp -> Number
; evaluates a given Exp with a fixed Number input
(define (eval n exp)
  (cond [(number? exp) exp]
        [(e-op? exp) ((eval-op (e-op-op exp))
                      (eval n (e-op-left exp))
                      (eval n (e-op-right exp)))]
        [(e-in? exp) n]))

(check-expect (eval 0 E0) 3)
(check-expect (eval 0 E1) (+ (- 1 (+ 2 2)) (* 3 (/ 3 4))))
(check-expect (eval 0 E2) (+ 1 (- 2 3)))
(check-expect (eval 0 E3) (+ 0 2))
(check-expect (eval 1 E3) (+ 1 2))
(check-expect (eval 1 (make-e-op '+ 1 (make-e-op '- 5 (make-e-in)))) (+ 1 (- 5 1)))

; 3 ----------------------------------------------------------------

; simplify : Exp -> Exp
; simplifies an expression

(define (simplify exp)
  (cond [(number? exp) exp]
        [(e-op? exp) (local [(define op (e-op-op exp))
                             (define exp1 (simplify (e-op-left exp)))
                             (define exp2 (simplify (e-op-right exp)))
                             (define helper
                               (cond [(or (symbol=? op '+) (symbol=? op '-))
                                      (cond [(and (number? exp1) (zero? exp1)) exp2]
                                            [(and (number? exp2) (zero? exp2)) exp1]
                                            [else (make-e-op op exp1 exp2)])]
                                     [(or (symbol=? op '*) (symbol=? op '/))
                                      (cond [(and (number? exp1) (= exp1 1)) exp2]
                                            [(and (number? exp2) (= exp2 1)) exp1]
                                            [else (make-e-op op exp1 exp2)])]))]
                       helper)]
        [(e-in? exp) exp]))

(check-expect (simplify E0) E0)
(check-expect (simplify (make-e-in)) (make-e-in))
(check-expect (simplify (make-e-op '+ E0 0)) E0)
(check-expect (simplify (make-e-op '+ 0 E0)) E0)
(check-expect (simplify (make-e-op '- 0 E0)) E0)
(check-expect (simplify (make-e-op '- E0 0)) E0)
(check-expect (simplify (make-e-op '* 1 E0)) E0)
(check-expect (simplify (make-e-op '* E0 1)) E0)
(check-expect (simplify (make-e-op '/ 1 E0)) E0)
(check-expect (simplify (make-e-op '/ E0 1)) E0)
(check-expect (simplify (make-e-op '+ E1 0)) E1)
(check-expect (simplify (make-e-op '+ E2 0)) E2)
(check-expect (simplify (make-e-op '+ (make-e-op '+ 0 3) E0)) (make-e-op '+ 3 3))
(check-expect (simplify (make-e-op '+ (make-e-op '+ 0 E1) E1)) (make-e-op '+ E1 E1))
(check-expect (simplify (make-e-op '+ E3 0)) E3)
(check-expect (simplify (make-e-op '+ (make-e-op '* 2 1)
                                   (make-e-op '- 5 3))) (make-e-op '+ 2 (make-e-op '- 5 3)))
(check-expect (simplify (make-e-op '+ 0 0)) 0)
(check-expect (simplify (make-e-op '+ (make-e-op '+ 0 0) (make-e-op '+ 0 0))) 0)


; 4 --------------------------------------------

; compact : Exp -> Exp
(define (compact exp)
  ...)

; 5 -----------------------------

(define (run n s)
  ...)
        


  











