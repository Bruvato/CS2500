#lang htdp/isl+

; ===
; hw8b;
; ===

; A SExpr is one of:
; - Symbol
; - [List-of SExpr]

; sexpr-temp : SExpr -> ?
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

; los-temp : [List-of SExpr] -> ?
(define (los-temp los)
  (cond
    [(empty? los) ...]
    [(cons? los) (... (sexpr-temp (first los))
                      (los-temp (rest los)) ...)]))

(define los1 '())
(define los2 '(a))
(define los3 '(a b c))


; 1 =============================================================
; topsy-turvy : SExpr -> SExpr
; inverts the order of an SExpr
(define (topsy-turvy sexpr)
  (cond
    [(symbol? sexpr) sexpr]
    [(list? sexpr) (rev-los sexpr)]))

(check-expect (topsy-turvy 'a) 'a)
(check-expect (topsy-turvy '(a b c)) '(c b a))
(check-expect (topsy-turvy '(a (b c) d)) '(d (c b) a))
(check-expect (topsy-turvy '((a b) c d)) '(d c (b a)))

; rev-los : [List-of SExpr] -> [List-of SExpr]
; reverses the order of a [List-of SExpr]
(define (rev-los los)
  (cond
    [(empty? los) '()]
    [(cons? los) (reverse (cons (topsy-turvy (first los))
                                (reverse (rev-los (rest los)))))]))

(check-expect (rev-los '()) '())
(check-expect (rev-los '(a)) '(a))
(check-expect (rev-los '(a b)) '(b a))
(check-expect (rev-los '(a b c)) '(c b a))
(check-expect (rev-los '(a (b c) d)) '(d (c b) a))
(check-expect (rev-los '((a b) c d)) '(d c (b a)))

; 2 =============================================================

; A [Maybe X] is one of
; - X
; - #false
; INTERPRETATION: represents only *possibly* having a value of a particular type


; find-path : SExpr Symbol -> [Maybe [List-of Natural]]
; outputs the path to the leftmost occurence of a symbol in a SExpr
; (outputs #false if that symbol is not present)
(define (find-path sexpr s)
  (cond
    [(symbol? sexpr) (if (symbol=? s sexpr) '() #f)]
    [(list? sexpr) (find-path-in-list sexpr s 0)]))

(check-expect (find-path '() 'a) #f)

(check-expect (find-path 'a 'a) '())
(check-expect (find-path 'a 'b) #f)

(check-expect (find-path '(a) 'a) '(0))
(check-expect (find-path '((a)) 'a) '(0 0))
(check-expect (find-path '(((a))) 'a) '(0 0 0))

(check-expect (find-path '(a) 'a) '(0))
(check-expect (find-path '(b a) 'a) '(1))
(check-expect (find-path '(c b a) 'a) '(2))

(check-expect (find-path '(a b) 'a) '(0))
(check-expect (find-path '(a (b c) d) 'd) '(2))
(check-expect (find-path '(a (b)) 'b) '(1 0))
(check-expect (find-path '(a (b c (d))) 'd) '(1 2 0))
(check-expect (find-path '(a b c) 'e) #f)
(check-expect (find-path '(a (b c (d))) 'e) #f)

; find-path-in-list : [List-of SExpr] Symbol Natural -> [Maybe [List-of Natural]] 
; outputs the path to the leftmost occurence of a symbol in a [List-of SExpr]
; (outputs #false if that symbol is not present)
(define (find-path-in-list los s index)
  (cond
    [(empty? los) #f]
    [(cons? los) (local [(define first-result (find-path (first los) s))]
                   (if (list? first-result)
                       (cons index first-result)
                       (find-path-in-list (rest los) s (add1 index))))]))

(check-expect (find-path-in-list '() 'a 0) #f)
(check-expect (find-path-in-list '(a) 'a 0) '(0))
(check-expect (find-path-in-list '((a)) 'a 0) '(0 0))
(check-expect (find-path-in-list '(((a))) 'a 0) '(0 0 0))

(check-expect (find-path-in-list '(a) 'a 0) '(0))
(check-expect (find-path-in-list '(b a) 'a 0) '(1))
(check-expect (find-path-in-list '(c b a) 'a 0) '(2))

(check-expect (find-path-in-list '(a b) 'a 0) '(0))
(check-expect (find-path-in-list '(a (b c) d) 'd 0) '(2))
(check-expect (find-path-in-list '(a (b)) 'b 0) '(1 0))
(check-expect (find-path-in-list '(a (b c (d))) 'd 0) '(1 2 0))
(check-expect (find-path-in-list '(a b c) 'e 0) #f)
(check-expect (find-path-in-list '(a (b c (d))) 'e 0) #f)



; 3 =============================================================

; unfold :: {A B} [A -> Boolean] [A -> B] [A -> A] A -> [List-of B]
(define (unfold pred? f g val)
  (if (pred? val)
      '()
      (cons (f val) (unfold pred? f g (g val)))))


; make-list-unfold : {X} Natural X -> [List-of X]
; constructs a list of n copies of a X (n is a Natural Number)
(define (make-list-unfold n x)
  (unfold (λ (val) (>= val n)) (λ (val) x) add1 0))

(check-expect (make-list-unfold 0 "a") '())
(check-expect (make-list-unfold 3 "a") '("a" "a" "a"))
(check-expect (make-list-unfold 4 7) '(7 7 7 7))

; 4 =============================================================

; map-unfold : {X Y} [X -> Y] [List-of X] -> [List-of Y]
; constructs a new list by applying a function f to each item on one or more existing lists
(define (map-unfold f l)
  (unfold empty? (λ (val) (f (first val))) rest l))

(check-expect (map-unfold add1 '()) '())
(check-expect (map-unfold add1 '(0 1 2)) '(1 2 3))
(check-expect (map-unfold string-length '("a" "aa" "aaa")) '(1 2 3))
(check-expect (map-unfold not '(#t #f #t)) '(#f #t #f))










