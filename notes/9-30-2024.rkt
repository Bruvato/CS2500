#lang htdp/isl

(require 2htdp/image)

(require 2htdp/universe)

; add-hello : los -> los
; adds hello to each
(define (add-hello los)
  (cond
    [(empty? los) '()]
    [(cons? los) (cons (string-append (first los)
                                      (add-hello (rest los))))]))


; do-to-all : {X, Y} [X -> Y] [ListOfX] -> [ListOf Y]
; collection of a thing to another collection
(define (do-to-all op lox)
  (cond [(empty? lox) '()]
        [(cons? lox)
         (cons (op (first lox)
                   (do-to-all op (rest lox))))]))


(define (blue-circles lon)
  (local [(define (blue-circle radius)
            (circle radius "solid" "blue"))]
    (do-to-all blue-circle lon)))


; (cons a (cons b (cons c '()))) -> (cons (op a) (cons (op b) (cons (op c) '())))

; ----------------


; keep-evens : given LON, keeps the even numbers

(define (keep-evens lon)
  (cond [(empty? lon) '()]
        [(cons? lon)
         (if (even? (first lon))
             (cons (first lon) (keep-evens (rest lon)))
             (keep-evens (rest lon)))]))
         


; keep-square: LOI -> LOI
; keeping the square images


(define (keep loi)
  (cond [(empty? loi) '()]
        [(cons? loi)
         (if (square-img? (first loi))
             (cons (first loi) (keep (rest loi)))
             (keep (rest loi)))]))

; Image -> Boolean
(define (square-img? i)
  ...)


; keep-if : [X -> Boolean] [ListOfX] -> [ListOfX]
(define (keep-if good? lox)
  (cond [(empty? lox) ...]
        [(cons? lox)
         (if (good? (first lox))
             (cons (first lox) (keep-if good? (rest lox)))
             (keep-if good? (rest lox)))]))


; (cons a (cons b (cons c '())))
; (summarize op base) ->
; (op a (op b (op c base)))


; (do to all) : map
; (keep-if) : filter
; (summarize) : foldr vs foldl

(map (lambda (num) (+ num 1)) '(1 2 3 4 5))