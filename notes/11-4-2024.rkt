#lang htdp/isl+

; sort a lislt of numbers in ascending order

; sort-nums : [List-of Number] -> [List-of Number]
(define (sort-nums nums)
  (cond [(empty? nums) ...]
        [(cons? nums) (insert (first nums)
                              (sort-nums (rest nums)) ...)]))

; insert : Number [List-of Number] -> [List-of Number]
; insert a number into a "sorted" list of numebrs
(define (insert n slon)
  (cond [(empty? slon) (list n)]
        [(cons? slon)
         (if (< n (first slon))
             (cons n slon)
             (cons (first slon)
                   (insert n (rest slon))))]))

(check-expect (insert 3 '()) '(3))
(check-expect (insert 2 '(3 4 5)) (cons 2'(3 4 5)))
(check-expect (insert 5 '(2 4 6)) (cons 2 (insert 5 '(4 6))))


(check-expect (sort-nums '()) '())
(check-expect (sort-nums '(1 3 4 2 4)) '(1 2 3 4 5))



; ======= GENERATIVE RECURSION =================
; accumulation - starts at bottom, keep going up until you can't
; genreative - start at top, keep going down until you can't



; idea : pick a landmark value called pivot
; split all the values so that smaller than pivot goes one way
; and larger than pivot goes the other


; TERMINATION : eac recursive call is on a strictyly shorter list
; and the lists can't get shorter forever
(define (sort-faster lon)
  (cond [(empty? lon) '()]
        [(cons? lon) 
         (local [(define pivot (first lon))
                 (define smallers (filter (λ(n) (< n pivot)) lon))
                 (define sames    (filter (λ(n) (= n pivot)) lon))
                 (define biggers  (filter (λ(n) (> n pivot)) lon))
                 ]
           (append (sort-faster smallers) sames (sort-faster biggers)))]))

#;(sort-faster '(4 7 2 5 8 3 6 9 1) '(1 2 3 4 5 6 7 8 9))

; generative template:

#;(define (gen-temp thing)
    (cond [(easy-case1? thing) ...]
          [(easy-case2? thing) ...]
          ...
          [(tricky-case1? thing)
           ...also recur on subproblems ...]
          ...))


; ===============================================

; assume :
; - switch signs
; - continuous

; deriv [Number -> Number] -> [Number -> Number]
(define DELTA 0.0000001)
(define (deriv f)
  (λ(x)
    (local [(define rise (- (f (+ x DELTA)) (f x)))
            (define run DELTA)]
      (/ rise run))))

(sin pi)
(cos pi)
((deriv sin) pi)

(define (find-root f min max)
  ...)























          


