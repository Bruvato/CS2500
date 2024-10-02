#lang htdp/bsl
(require 2htdp/image)

; Exercise 1

; Number Nummber Number -> Number
; computes euclidean distance oof point (x,y,z) to the origin
; given: 2 for x, 2 for y, 1 for z
; expected: 3
(define (dist x y z)
  (sqrt (+ (sqr x) (sqr y) (sqr z))))

(check-expect (dist 2 2 1) 3)


; Excercise 2

; Boolean Boolean -> Boolean
; returns #true if sunny and beach are both true or both false, and #false otherwise
; given: #true for sunny, #true for beach
; expected: #true
; given: #false for sunny, #false for beach
; expected: #true
; given: #false for sunny, #true for beach
; expected: #false
(define (iff sunny beach)
  (and (if sunny beach #t) (if beach sunny #t)))

(check-expect (iff #true #true) #true)
(check-expect (iff #false #false) #true)
(check-expect (iff #false #true) #false)


; Excercise 3

; Image -> String
; returns "square", "fullscreen", "widescreen" or "portrait"
; according to the ratios of image i, and "too wide" otherwise
; given: (rectangle 10 10 "solid" "blue") for i
; expected: "square"
; given: (rectangle 16 12 "solid" "blue") for i
; expected: "fullscreen"
; given: (rectangle 32 18 "solid" "blue") for i
; expected: "widescreen"
; given: (rectangle 19 20 "solid" "blue") for i
; expected: "portrait"
(define (image-classify i)
  [cond
    [(equal? (image-width i) (image-height i)) "square"]
    [(equal? (/ (image-width i) (image-height i)) 4/3) "fullscreen"]
    [(equal? (/ (image-width i) (image-height i)) 16/9) "widescreen"]
    [(< (/ (image-width i) (image-height i)) 1) "portrait"]
    [else "too wide"]])

(check-expect (image-classify (rectangle 10 10 "solid" "blue")) "square")
(check-expect (image-classify (rectangle 16 12 "solid" "blue")) "fullscreen")
(check-expect (image-classify (rectangle 32 18 "solid" "blue")) "widescreen")
(check-expect (image-classify (rectangle 19 20 "solid" "blue")) "portrait")


; Excercise 4

; Image -> Number
; counts the number of pixels in a given image i
; given: (rectangle 3 4 "solid" "blue") for i
; expected: 12
(define (image-area i)
  (* (image-width i) (image-height i)))

(check-expect (image-area (rectangle 3 4 "solid" "blue")) 12)