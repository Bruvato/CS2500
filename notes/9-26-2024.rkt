#lang htdp/isl

(require 2htdp/image)
; A Note on Notation
; this
(cons 5 (cons 4 (cons 6 (cons 8 '()))))
; is the same thing as this
(list 5 4 6 8)

(cons 1 (list 2 3 4 5))

(define (biggest nel)
  (cond [(empty? (rest nel)) (first nel)]
        [else (max (first nel) (biggest (rest nel)))]))


; -----------------------

; a ListOfStrings (LOS) is one of
; - '()
; - (cons String '())
; and represensts a list of stirngs

(define LOS0 '())
(define LOS1 (list "a" "b" "c"))



; add-hello : LOS -> LOS
; purpse
(check-expect (add-hello LOS1) (list "ahello" "bhello" "chello"))

(define (add-hello los)
  (cond [(empty? los) '()]
        [(cons? los)
         (cons (string-append "hello" (first los))
               (add-hello (rest los)))]))

(define (add-prefix prefix los)
  (cond [(empty? los) '()]
        [(cons? los) (cons (string-append prefix (first los))
                    (add-prefix prefix (rest los)))]))

; ----------------------------------------------------

(define (add-sum lon)
  (cond [(empty? lon) 0]
        [(cons? lon)
         (+ (first lon)
            (add-sum (rest lon)))]))

; summarize : [X X -> X] X [List-of X] -> X

; summarize : For all X, Y, [X Y -> Y] Y [List-of X] -> Y
(define (summarize op base lst)
  (cond [(empty? lst) base]
        [(cons? lst)
         (op (first lst)
             (summarize op base (rest lst)))]))

; LON -> Number
(define (product lon)
  (summarize * 1 lon))

(summarize string-append
           "Batman"
           (list "ne" "ne" "ne"))

; A [List-of X] is one of
; - '()
; (cons X [List-of X])

; Num Image -> Image
(define (draw-circle-onto rad img)
  (overlay (circle rad "outline" "black")
           img))


(summarize draw-circle-onto
           (empty-scene 200 200)
           (list 75 50 25 10))

(define (string+length s l)
  (+ (string-length s) l))

(summarize string+length
           0
           (list "hi" "bye"))