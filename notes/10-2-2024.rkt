#lang htdp/isl

; A [Pair-of X Y] is a (make-pair x y)
(define-struct pair [left right])

(make-pair 1 #t)
(make-pair #t "hi")

; [Pair-of [List-of String] Boolean]
(define (polosb-temp polosb)
  (... (los-temp (pair-left pair))
       (pair-right pair) ...))


; ------------

; add-n-all : Number -> ([List-of Number] -> [List-of Number])

(define add-10-all (add-n-all 10))
(check-expect (add-10-all (list 1 2 3 4)) (list 11 12 13 14))

(define (add-n-all n)
  ; [List-of Number] -> [List-of Number]
  (local [(define (adds-n-to-all lon)
            (cond [(empty? lon) ...]
                  [(cons? lon)
                   (cons (+ n (first lon))
                         (adds-n-to-all (rest lon)))]))]
    adds-n-to-all))

(define (add-n-all-with-map n)
  (local [; Number -> Number
          (define (add-n-to a-num)
            (+ n a-num))
          (define (func-adds-n-to-all lon)
            (map add-n-to lon))]
    func-adds-n-to-all))

; ----------------------------------------------

; build-list : {Y} Natural [Natural -> Y] -> [List-of Y]

; (build-list N f)
; ====>
; (list (f 0) (f 1) (f 2) ... (f (N-1))

(check-expect
 (build-list 3 add1)
 (list (add1 0) (add1 1) (add1 2) (add1 3)))




; ----

(check-expect
 (times-table 3 4)
 '((0 0 0 0)
   (0 1 2 3)
   (0 2 4 6)))

#|
; Natural Natural -> [List-of [List-of Natural]]
(define (times-table r c)
  (local [; Natural -> [List-of Natural]
          (define (build-row row)
            (build-list c *))
  (build-list r build-row)]))
|#

; Natural -> [List-of Natural]
(define (build-n-multiples-of-k N K)
  (local [(define (nth-multiple n)
            (* n K))]
  (build-list N nth-multiple)))



; Natural -> (Natural -> {List-of Natural])
(define (Kth-row-table K)
  (local [; Natural -> [List-of Natural]
          (define (table-row-of-length len)
            (build-n-multiples-of-k len K))]
    table-row-of-length))

; Natural Natural -> 
(define (times-table r c)
  (build-list r (Kth-row-table c)))

