#lang htdp/isl+

; ===
; hw8b;
; ===

; A SExpr is one of:
; - Symbol
; - [List-of SExpr]

; SExpr -> ?
(define (sexpr-temp sexpr)
  (cond
    [(symbol? sexpr) ...]
    [(list? sexpr) (... (los-temp sexpr) ...)]))

(define sexpr1 'a)
(define sexpr2 '(a b c))
(define sexpr3 '(a (b c) d))

; A LOS (List-of SExpr) is one of:
; - '()
; (cons SExpr LOS)

(define (los-temp los)
  (cond
    [(empty? los) ...]
    [(cons? los) (... (sexpr-temp (first los))
                      (los-temp (rest los)) ...)]))




; 1 =============================================================
; topsy-turvy : SExpr -> SExpr
; inverts the order of an SExpr
(check-expect (topsy-turvy 'a) 'a)
(check-expect (topsy-turvy '(a b c)) '(c b a))
(check-expect (topsy-turvy '(a (b c) d)) '(d (c b) a))
(check-expect (topsy-turvy '((a b) c d)) '(d c (b a)))

#;(define (topsy-turvy sexpr)
    (cond
      [(symbol? sexpr) sexpr]
      [(empty? sexpr) '()]
      [(cons? sexpr)
       (reverse (cons (topsy-turvy (first sexpr))
                      (reverse (topsy-turvy (rest sexpr)))))]))

#;(define (topsy-turvy sexpr)
    (cond [(symbol? sexpr) sexpr]
          [(list? sexpr) (map (λ(sexpr) (if (symbol? sexpr)
                                            sexpr
                                            (topsy-turvy sexpr))) (reverse sexpr))]))

(define (topsy-turvy sexpr)
  (cond
    [(symbol? sexpr) sexpr]
    [(list? sexpr) (rev-los sexpr)]))

(define (rev-los los)
  (cond
    [(empty? los) '()]
    [(cons? los) (reverse (cons (topsy-turvy (first los))
                                (rev-los (rest los))))]))




; 2 =============================================================

; A [Maybe X] is one of
; - X
; - #false
; INTERPRETATION: represents only *possibly* having a value of a particular type

; find-path : SExpr Symbol -> [List-of [Maybe X]]
; outputs the path to the leftmost occurence of a symbol in a SExpr
; (outputs #false if that symbol is not present)

(check-expect (find-path '() 'a) #f)
(check-expect (find-path 'a 'a) '())
(check-expect (find-path '(a) 'a) '(0))
(check-expect (find-path '(a b) 'b) '(1))
(check-expect (find-path '(a b c) 'c) '(2))
(check-expect (find-path '(a (b c) d) 'd) '(2))
(check-expect (find-path '(a (b)) 'b) '(1 0))
(check-expect (find-path '(a (b c (d))) 'd) '(1 2 0))
(check-expect (find-path '(a b c) 'e) #f)

(define (find-path sexpr s)
  (cond
    [(symbol? sexpr) (if (symbol=? sexpr s) '() #f)]
    [(empty? sexpr) #f]
    [(cons? sexpr)
     (cons (find-path (first sexpr) s)
           (find-path (rest sexpr) s))]))













