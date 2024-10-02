#lang racket/base

(list 1 2 3)

'(1 2 3)

;; Strings, Numbers, Booleans, Other Lists...
;; anything that can reduce down to a value

(list "a" "b")

'((+ 2 3))

'((= 1 0))

(define x 5)
(list x)
'(x)

(quote x)
(quote (+ 2 3))
(quote (3 4 5 6))

;; Symbols

(quasiquote (unquote x)) ; escapes in a quasiquote

`(,x)


`(1 2 3 4 5)

`(1 2 3 4 ,(+ 2 3))

(quote 2)
(quote "string")
(quote #t)

; ' turns things into values

'()

