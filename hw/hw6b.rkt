#lang htdp/isl

; 1 -----------------------------------------

; sum-0-to-n : Number -> Number
; computes the sum of integers from 0 to n (inclusive)
(check-expect (sum-0-to-n 3) 6)
(check-expect (sum-0-to-n 5) 15)

(define (sum-0-to-n n)
  (foldr + 0 (build-list (add1 n) identity)))

(check-expect (sum-0-to-n-v2 3) 6)
(check-expect (sum-0-to-n-v2 5) 15)

(define (sum-0-to-n-v2 n)
  (local [;build-l : Number -> [List-of Number]
          ; constructs a list of Numbers from 0 to n (inclusive)
          (define (build-l n)
            (cond
              [(= 0 n) '()]
              [else (cons n (build-l (sub1 n)))]))]
    (foldr + 0 (build-l n))))

; sum-squared-0-to-n : Number -> Number
; computes the sum of squares of integers from 0 to n (inclusive)
(check-expect (sum-square-0-to-n 3) 14)
(check-expect (sum-square-0-to-n 5) 55) 

(define (sum-square-0-to-n n)
  (foldr + 0 (build-list (add1 n) sqr)))

; compute-sum : [Number -> Number] Number -> Number
; computes the sum of f(i) for i from 0 to n (inclusive)
; where f is a function that takes a Number and outputs a Number
(check-expect (compute-sum sqr 3) 14)
(check-expect (compute-sum add1 3) 10)

(define (compute-sum f n)
  (foldr + 0 (build-list (add1 n) f)))


; 2 ---------------------------------------------------

; my-map : [X -> Y] [List-of X] -> [List-of Y]
; constructs a list by applying a function f to each item on a given list l
(check-expect (my-map add1 '(1 2 3 4 5)) '(2 3 4 5 6))
(check-expect (my-map sub1 '(1 2 3 4 5)) '(0 1 2 3 4))

(define (my-map f l)
  (local [; map-helper : X [List-of Y] -> [List-of Y]
          ; applies a function f to x
          ; and then applies cons to that and an accumulated value
          (define (map-helper x acc)
            (cons (f x) acc))]
    (foldr map-helper '() l)))
  

#;(define (my-foldr f base l)
  (cond
    [(empty? l) base]
    [[cons? l] (f (first l)
                  (my-foldr f base (rest l)))]))

#;(define (my-map f l)
  (cond
    [(empty? l) '()]
    [(cons? l) (cons (f (first l))
                     (my-map f (rest l)))]))

; 3 ---------------------------------------------------

; my-filter : [X -> Boolean] [List-of X] -> [List-of X]
; constructs a list from all those items on a list l
; for which the predicate p? holds
(check-expect (my-filter positive? '(-2 -1 0 1 2)) '(1 2))
(check-expect (my-filter negative? '(-2 -1 0 1 2)) '(-2 -1))
(check-expect (my-filter even? '(1 2 3 4 5)) '(2 4))
(check-expect (my-filter odd? '(1 2 3 4 5)) '(1 3 5))

(define (my-filter p? l)
  (local [; filter-helper : X [List-of X] -> [List-of X]
          ; applies cons to x and an accumulated value
          ; if a predicate p? holds true
          ; otherwise give the accumulated value
          (define (filter-helper x acc)
            (cond
              [(p? x) (cons x acc)]
              [else acc]))]
    (foldr filter-helper '() l)))

#;(define (my-filter p? l)
  (cond
    [(empty? l) '()]
    [(cons? l) (if (p? (first l))
                   (cons (first l) (my-filter p? (rest l)))
                   (my-filter p? (rest l)))]))

; 4 ---------------------------------------------------

; my-ormap : [X -> Boolean] [List-of X] -> Boolean
; determines whether a predicate p? holds
; for at least one item of a list l
(check-expect (my-ormap even? '(2 4 6 8)) #t)
(check-expect (my-ormap even? '(1 3 5 7)) #f)
(check-expect (my-ormap odd? '(1 3 5 7)) #t)
(check-expect (my-ormap even? '(1 3 5 7 8)) #t)
(check-expect (my-ormap even? '(2 "hi")) #t)
(check-expect (my-ormap odd? '(3 "hi")) #t)

(define (my-ormap p? l)
  (local [; my-ormap-helper : X Boolean -> Boolean
          ; determines if p? holds for x or an accumulated value
          ; (shortcircuits to true for the first x for which p? holds)
          (define (my-ormap-helper x acc)
            (if acc
                #t
                (or (p? x) acc)))]
    (foldl my-ormap-helper #f l)))
          

#;(define (my-ormap p? l)
  (cond
    [(empty? l) #f]
    [(cons? l) (or (p? (first l))
                   (my-ormap p? (rest l)))]))

; 5 ---------------------------------------------------

; my-andmap : [X -> Boolean] [List-of X] -> Boolean
; determines whether a predicate p? holds
; for all items of a list l
(check-expect (my-andmap even? '(2 4 6 8)) #t)
(check-expect (my-andmap odd? '(1 3 5 7)) #t)
(check-expect (my-andmap even? '(2 4 6 8 9)) #f)
(check-expect (my-andmap even? '(3 "hi")) #f)
(check-expect (my-andmap odd? '(2 "hi")) #f)

(define (my-andmap p? l)
  (local [; my-andmap-helper : X Boolean -> Boolean
          ; determines if p? holds for x and an accumulated value
          ; (shortcircuits to false for the first x for which p? doesn't hold)
          (define (my-andmap-helper x acc)
            (if (not acc)
                #f
                (and (p? x) acc)))]
    (foldl my-andmap-helper #t l)))
                

#;(define (my-andmap p? l)
  (cond
    [(empty? l) #t]
    [(cons? l) (and (p? (first l))
                    (my-andmap p? (rest l)))]))

; 6 ---------------------------------------------------

; foldr takes in a function, a final value, and a list, and outputs a single value.
; build-list takes in a Natural and a function, and outputs a list.
; since foldr works with only a list and not a single Natural,
; build-list cannot be implemented using foldr without having a pre-existing list.

; 7 ---------------------------------------------------

; A Nat is one of
; - 0
; - (add1 Nat)

(define (nat-temp nat)
  (cond
    [(= 0 nat) ...]
    [else ...]))

(define NAT0 0) ; 0
(define NAT1 (add1 NAT0)) ; (add1 0)
(define NAT2 (add1 NAT1)) ; (add1 (add1 0))
(define NAT3 (add1 NAT2)) ; (add1 (add1 (add1 0)))

; fold-nat : [Nat Nat -> X] Nat -> X
; converts (add1   (add1    (add1    (add1 ...... (add1 0)))))
; to (f   N  (f (N-1) (f (N-2) (f (N-3) ... (f 1 base)))))
(check-expect (fold-nat + 0 NAT3) (+ 3 (+ 2 (+ 1 0))))
(check-expect (fold-nat - 0 NAT3) (- 3 (- 2 (- 1 0))))
(check-expect (fold-nat cons '() NAT3) (cons 3 (cons 2 (cons 1 '()))))

(define (fold-nat f base n)
  (cond
    [(= 0 n) base]
    [else (f n (fold-nat f base (sub1 n)))]))

; my-build-list : Natural [Natural -> X] -> [List-of X]
; constructs a list by applying a function f to the numbers between 0 and (- n 1) (inclusive)
(check-expect (my-build-list 5 add1) '(1 2 3 4 5))
(check-expect (my-build-list 5 sub1) '(-1 0 1 2 3))
(check-expect (my-build-list 5 number->string) '("0" "1" "2" "3" "4"))

(define (my-build-list n f)
  (local [; my-build-list-helper : Natural [List-of X] -> [List-of X]
          ; applies a function f to a Natural
          ; and then applies cons to that and an accumulated value
          (define (my-build-list-helper nat acc)
            (cons (f nat) acc))]
  (reverse (fold-nat my-build-list-helper
                     (cons (f 0) '())
                     (sub1 n)))))



