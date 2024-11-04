#lang htdp/isl+

; design a function to reverse a list
; use only structural recursion

#|
; [listof X] -> [listof X]
(define (rev-structural l)
  (cond [(empty? l) '()]
        [(cons? l) (cons (get-last l)
                         (rev-structural (remove-last l)))]))
; [listof x] -> x
(define (get-last l)
  (cond [(empty? (rest l)) (first l)]
        [(cons? (rest l)) (get-last (rest l))]))

; [NElistof x] -> [list-of x]
(define (remove-last l)
  (cond [(empty? (rest l)) '()]
        [(cons? (rest l)) (cons (first l) (remove-last (rest l)))]))

|#


#|
; [list-of x] -> [list-of x]
(define (rev-structural l)
  (cond [(empty? l) '()]
        [(cons? l) (snoc (rev-structural (rest l) (first l)))]))

; [list-of x] x -> [list-of x]
(define (snoc front last)
  (cond [(empty? front) (list last)] ; '(,last)
        [(cons? front) (cons (first front) (snoc (rest front) last))]))

; (cons 1 (cons 2 (cons 3 empty)))
; (snoc (snoc (snoc empty 3) 2) 1)
;             (cons 3 empty)
;         (cons 3 (snoc 2 empty))
|#


; 1) what does the acc represent?
;    the elements from the front of the list *IN REVERSE ORDER
; 2) where does the acc start?
;    '()
; 3) how do we update the acc?
;    conds teh first of the list onto the front of the acc
; 4) how do we use the acc?
;    e

#;(define (rev l)
  (local [; acc represents the reversed front of the list (1)
          ; [list-of x] [list-of x] -> [list-of x]
          (define (rev/acc l accum) ; accum = rev-front
            (cond [(empty? l) accum] ; (4)
                  [(cons? l) (rev/acc (rest l)
                                      (cons (first l) accum))]) ; (3)
              )]
    (rev/acc l '()))) ; (2)
          


; ===== in general ============

; foldl

; [X Y -> Y] Y [List-of X] -> Y
(define (fold/acc f acc l)
  (cond [(empty? l) acc]
        [(cons? l)
         (fold/acc f (f (first l) acc) (rest l))]))

; (cons a (cons b (cons c '())))
; (f c (f b (f a base)))

(define (rev l)
  (fold/acc cons '() l))

