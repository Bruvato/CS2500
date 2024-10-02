#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

; A Line Segment is a (make-lineseg Position Position)
; INTERP: (make-lineseg start end) describes the start and end positions of a line segment
(define-struct lineseg[start end]) 

(define (lineseg-temp ls)
  (...(posn-temp(ls-start))...
      (posn-temp(ls-end))...))

; -----------------------------------------------------

; A Position is one of:
; - "origin"
; - PositiveNumber
; - Posn

; INTERP:
; - "origin" is the center for our coord sys
; - PosNum is a distance on the +x axis
; Posn is nay Cartesian coord

; posn-temp: Position -> ?
(define (posn-temp p)
  (cond [(string? p) ...] ; only string is "origin"
        [(number? p) ...] ; only number is positive number
        [(posn? p) (... (posn-temp p) ...)]))

; distance-to-0: Position -> Number
; 
(define (distance-to-0 p)
  (cond [(string? p) 0]
        [(number? p) p]
        [(posn? p) (dist-to-0 p)]))

(define (dist-to-0 p)
  (...))

; -------------------------------------------------------------


; A Typewriter is a (make-tw String Number)
; INTERP: (make-tw twxt cursor) tracks the text typed so far and the position of the cursor where 0 is the leftmos position
(define-struct typewriter [text cursor])

(define (typewriter-temp tw)
  (...(typewriter-text tw)...(typewriter-cursor tw)...))

(define TW1 (make-typewriter "hello world" 0))

; Design a function to handle key processes
; handle-key: Typewriter Keypress -> Typewriter

(check-expect (handle-key TW1 "left") TW1)
(check-expect (handle-key TW1 "up") TW1)
(check-expect (handle-key TW1 "right")
              (make-typewriter "hello world" 1))
(check-expect (handle-key (make-typewriter "" 0) "0")
              (make-typewriter "a" 1))
(check-expect (handle-key TW2 "1")
              (make-typewriter ""  0))

; move-cursor-left: Typewriter -> Typewriter
; 
(define (move-cursor-left tw)
  (make-typewriter (typewriter-text tw)
                   (min 0 (string-length (typewriter-text tw))
                   (- 1 (typewriter-cursor tw)))))

; move-cursor-right Typewriter -> Typewriter
(define (move-cursor-right tw)
  (make-typewriter (typewriter-text tw)
                   (min (string-length (typewriter-text tw))
                   (+ 1 (typewriter-cursor tw)))))


(check-expect (insert-string "1" "foo" "fighters" 4)
              "foo 1fighters")

; insert-string: String String Natural -> String
; insert string needle haystack index will insert the "needle" text into the "haystack" at the given index
(define (insert-string to-insert existing-text where)
  (string-append
   (substring existing-text 0 where)
   to-insert
   (substring existing-text where (string-length existing-text))))

; add-text: Typewriter String -> Typewriter
(define (add-text tw text)
  (make-typewriter (insert-string text (typewriter-text tw) (typewriter-cursor tw))
                   (+ (string-length text) (typewriter-cursor tw))))



(define (handle-key tw key)
  (cond [(string=? key "left") (move-cursor-left tw)]
        [(string=? key "right") (move-cursor-right tw)]
        [(string=? key "up") tw]
        [(string=? key "down") tw]
        [(string=? key "shift") tw]
        [else (add-text tw key)]))

; draw-typewriter: Typewriter -> Image
(define (draw-typewriter tw)
  (text (typewriter-text tw) 30 "red"))
  

(define (main initial-tw)
        (big-bang initial-tw
          (on-key handle-key)
          (to-draw draw-typewriter)))
