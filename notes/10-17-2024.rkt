#lang htdp/isl+


; === case 1 two structural inputs but one doesnt matter for now

; add-at-end {X} [List-of X] [List-of X] -> [List-of X]
(check-expect (add-at-end '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(check-expect (add-at-end '(1 2 3) '(4 5 6)) '(4 5 6 1 2 3))

(define (add-at-end l1 l2)
  (cond [(empty? l2) l1]
        [(cons? l2) (cons (first l2)
                          (add-at-end (rest l2)))]))



; === case 2 : Parallel Traversal

; Supppose I have words: [Lis-of String]
; meanings : [List-of String]
; assume (length words) = (length menaings)
(define (lookup words meanings word)
  (cond [(empty? words) (error ...)]
        [(cons? words)
         (if (string=? (first words) word)
             (first meanings)
             (lookup (rest words) (rest meanings) word))]))

; keep-if-marked : {X} [List-of X] [List-of Boolean] -> [List-of X]
; assums that items and marks are same size
(define (keep-if-marked items marks)
  (cond [(empty? items) '()]
        [(cons? items)
         (if (first marks)
             (cons (first items) (keep-if-marked (rest items) (rest marks)))
             (keep-if-marked (rest items) (rest marks)))]))



; === case 3 : Cross Product Case

; get-nth : {X} [List-of X] Nat -> X
(define (get-nth items n)
  (cond [(and (empty? items) (zero? n))    (error ...)]
        [(and (empty? items) (positive? n)) (error ...)]
        [(and (cons? items) (zero? n))     (first items)]
        [(and (cons? items) (positive? n))  (get-nth (rest items) (sub1 n))]))

; list=? : [List-of Number] [List-of Number] -> Boolean
; list=? : [List-of X] [List-of X] -> Boolean
(define (list=? same l1 l2)
  (cond [(and (empty l1) (empty l2)) #t]
        [(and (empty l1) (cons l2)) #f]
        [(and (cons l1) (empty l2)) #f]
        [(and (cons l1) (cons l2)) (and (same (first l1) (first l2))
                                        (list=? same (rest l1) (rest l2)))]))

(list=? (λ(lon1 lon2) (list=? = lon1 lon2)) lolon1 lolon2)

; [X X -> Bool] -> [[List-of X] [List-of X] -> Bool]
(define (make-list=? same)
  (λ(l1 l2)
    (cond ...)))

(define lon=? (make-lis=? =))
(define lolon=? (make-list=? (make-list=? =)))

 