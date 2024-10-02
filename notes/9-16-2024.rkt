#lang htdp/bsl

; A Airplane is one of
; - "pilot"
; - (make-passenger String Airplane)

(define-struct passenger [name others])

; Interp: A (make-passenger String Airplane) represents the name of a passenger
; and the other poeple on the same flight

; "pilot"
; (make-passenger "carl" "pilot")
; (make-passenger "sam" (make-passenger "Bill" "Pilot"))

(define (airplane-temp a)
  (cond [(string? a) ...] ; string=? takes in two stirngs
        [(passenger? a) (... (passenger-name a) ... (airplane-temp (passenger-others a)))]))

; templates go with data definitons NOT structs

; num-passenger : Airplane -> Natural
(define (num-passenger a)
  (cond [(string? a) 0]
        [(passenger? a) (add1 (num-passenger (passenger-others a)))]))

; An Optword is one of
; - #false
; - "hi"

; A Guess is a (make-guess Optword Optword Optword)
(define-struct guess [one two three])


; --------------------------------------

(define-struct last (word))
(define-struct more (word next))
; A Sequence is one of
; - (make-last String)
; - (make-more String Sequence)

(define (seq-temp seq)
  (cond [(last? seq) (last-word seq)]
        [(more? seq)
         (... (more-seq seq)
              (seq-temp (more-next seq)))]))
#|
(define ex1 (make-last "hi"))
(define ex2 (make-more "goodbye" (make-last "hello")))
(define ex3 (make-more "goodbye" ex1))
(define ex4 (make-more "greetings" ex3))


;   inpu        last-word in    more-word in      more-next in     num-word (next-word in)   output
; ex1 -> 1
; ex2 -> 2
; ex3 ->

(check-expect (num-words ex1) 1) ; last
(check-expect (num-words ex3) (add1 (num-words ex1))) ;more
(check-expect (num-words ex3) 2)
(check-expect (num-words ex4) (add1 (num-words ex3)))
|#

(define (num-words seq)
  (cond [(last? seq) 1]
        [(more? seq) (add1 (num-words (more-next seq)))]))


; Design a function excited that adds an exclamation makr to each word in a sequence

(define ex1 (make-last "hello"))
(define ex2 (make-more "goodbye" ex1))
(define ex3 (make-more "maybe" ex2))

; excite : Sequence -> Sequence

(check-expect (excite ex1) (make-last "hello!"))
(check-expect (excite ex2) (make-more (string-append (more-word ex2) "!") (excite ex1)))
(check-expect (excite ex3) (make-more (string-append (more-word ex3) "!") (excite ex2)))

(define (excite seq)
  (cond [(last? seq) (make-last (string-append (last-word seq) "!"))]
        [(more? seq) (make-more (string-append (more-next seq)) "!")]))

(define (excite-word w) (string-append w "!"))

;; TABLE METHOD