#lang htdp/isl+

; A LON
; - '()
; - (cons Number LON)

; A LOLON
; - '()
; - (cons LON LOLON)

; all-contained-in? : lolon1 lolon2
;
(check-expect (all-contained-in? '() '()) #t)
(check-expect (all-contained-in? (list '()) (list '())) #t)
(check-expect (all-contained-in? (list '(1)) (list '(1))) #t)
(check-expect (all-contained-in? (list '(1)) (list '(1) '(2))) #t)
(check-expect (all-contained-in? (list '(1 2) '(3 4)) (list '(0 0) '(3 4) '(1 2))) #t)
(check-expect (all-contained-in? (list '(1 2) '(3 4)) (list '(0 0) '(1 2))) #f)

(define (all-contained-in? lolon1 lolon2)
  (andmap (λ(lon) (lon-contained-in? lon lolon2)) lolon1))

(define (lon-contained-in? lon lolon)
  (ormap (λ(inner-lon) (equal-lon? lon inner-lon)) lolon))

(define (equal-lon? lon1 lon2)
  (and (subset? lon1 lon2)
       (subset? lon2 lon1)))

(define (subset? lon1 lon2)
  (andmap (λ(x) (element-of? x lon2)) lon1))

(define (element-of? x lon)
  (ormap (λ(elem) (= elem x)) lon))

; ==================================================

(define (append-from-fold l1 l2)
  (foldr (λ(x acc) (cons x acc)) l2 l1))

(append-from-fold '(1 2 3) '(4 5 6))



; ==================================================


; [Set-of X] -> [Set-of [Set-of X]]
(define (powerset set)
  ; (1 2 3) -> (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3)
  (cond [(empty? set) '(())] ; 
        [(cons? set)
         (local [(define ps-of-rest (powerset (rest set)))
                 (define (add-first-to-ps ps) (cons (first set) ps))]
           (append (map add-first-to-ps ps-of-rest)
                   ps-of-rest))]))



(define (powerlist lox)
  (cond [(empty? lox) '(())]
        [(cons? lox)
         (local [(define por (powerlist (rest lox)))
                 (define portx
                   (map (λ(sublist) (cons (first lox) sublist)) por))]
           (append por portx))]))

(powerlist '(1 2 3))



; empty? -> '(())
; cons? -> P(rest) + first cons onto each of P(rest)

(define (powerlist2 lox)
  (cond [(empty? lox) (list '())]
        [(cons? lox) (map (λ(l) (cons (first l))) (powerlist2 lox))]))



(map add1 '(1 2 3))

(define (my-andmap p? l)
  (cond [(empty? l) #t]
        [(cons? l) (and (p? (first l))
                        (my-andmap p? (rest l)))]))

(my-andmap positive? '(1 2 -1))

(define (my-foldr f base l)
  (cond [(empty? l) base]
        [(cons? l) (f (first l)
                      (my-foldr f base (rest l)))]))

(my-foldr cons '() '(1 2 3))

(define (m-foldl f base l)
  (cond
    [(empty? l) base]
    [(cons? l) (m-foldl f (f (first l) base) (rest l))]))

(m-foldl cons '() '(1 2 3))



