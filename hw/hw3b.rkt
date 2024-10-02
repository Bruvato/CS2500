#lang htdp/bsl

; A JDir is one of
; - "leftright"
; - "updown"
; INTERPRETATION: Represents the long direction the bricks are laying in
;   a level of a Jenga tower, when looking down at the tower from above.
; A Jenga is one of:
; - "top"
; - (make-level JDir Boolean Boolean Boolean Jenga)
(define-struct level [dir side1 mid side2 above])
; INTERPRETATION: Represents either an empty Jenga tower, or a level of a
;   Jenga tower with a block direction, the presence (#true) or absence
;   (#false) of the three blocks in that level (side1 is either the leftmost
;   or the uppermost block, depending on the direction), and the rest of the
;   tower above it.


; JDir Template
(define (jdir-temp jdir)
  (cond [(string=? jdir "leftright") ...]
        [(string=? jdir "updown") ...]))

; JDir Examples
(define LEFTRIGHT "leftright")
(define UPDOWN "updown")

; Jenga Template
(define (jenga-temp jenga)
  (cond [(string? jenga) ...]
        [(level? jenga) (... (jdir-temp (level-dir jenga))
                             ... (level-side1 jenga)
                             ... (level-mid jenga)
                             ... (level-side2 jenga)
                             ... (jenga-temp (level-above jenga)) ...)]))

; Jenga Examples
(define J0 "top")
(define J1 (make-level UPDOWN #true #true #true J0))
(define J2 (make-level LEFTRIGHT #true #false #true J1))
(define J3 (make-level UPDOWN #true #false #false J2))
(define J4 (make-level UPDOWN #false #false #false J3))
(define J5 (make-level UPDOWN #false #false #true J4))


; 1 --------------------------------------------------------------------------

; num-levels Jenga -> Number
; counts the total number of levels in a Jenga tower
(check-expect (num-levels J0) 0)
(check-expect (num-levels J1) 1)
(check-expect (num-levels J2) 2)
(check-expect (num-levels J3) 3)

(define (num-levels jenga)
  (cond [(string? jenga) 0]
        [(level? jenga) (+ 1 (num-levels (level-above jenga)))]))

; 2 --------------------------------------------------------------------------

; num-blocks : Jenga -> Number
; counts the total number of blocks in a Jenga tower
(check-expect (num-blocks J0) 0)
(check-expect (num-blocks J1) 3)
(check-expect (num-blocks J2) 5)
(check-expect (num-blocks J3) 6)

(define (num-blocks jenga)
  (cond [(string? jenga) 0]
        [(level? jenga) (+ (num-blocks-level jenga) (num-blocks (level-above jenga)))]))

; num-blocks-level : Level -> Number
; counts the total number of blocks in a level of a Jenga tower
(check-expect (num-blocks-level (make-level UPDOWN #true #true #true J0)) 3)
(check-expect (num-blocks-level (make-level UPDOWN #false #true #true J0)) 2)
(check-expect (num-blocks-level (make-level UPDOWN #true #false #false J0)) 1)
(check-expect (num-blocks-level (make-level UPDOWN #false #false #false J0)) 0)

(define (num-blocks-level level)
  (+ (if (level-side1 level) 1 0)
     (if (level-mid level) 1 0)
     (if (level-side2 level) 1 0)))

; 3 --------------------------------------------------------------------------

; alternating? : Jenga -> Boolean
; determines whether a Jenga tower correctly alternates the direction of each level or not
(check-expect (alternating? J0) #true)
(check-expect (alternating? J1) #true)
(check-expect (alternating? J2) #true)
(check-expect (alternating? J3) #true)
(check-expect (alternating? J4) #false)

(define (alternating? jenga)
  (cond [(string? jenga) #true]
        [(string? (level-above jenga)) #true]
        [(level? jenga) (and (not (string=? (level-dir jenga) (level-dir (level-above jenga))))
                             (alternating? (level-above jenga)))]))

; 4 --------------------------------------------------------------------------

; top-level : Jenga -> Level
; gives the topmost level of a Jenga tower or #f if no such level exists
(check-expect (top-level J0) #false)
(check-expect (top-level J3) J1)

(define (top-level jenga)
  (cond [(string? jenga) #false]
        [(string? (level-above jenga)) jenga]
        [(level? jenga) (top-level (level-above jenga))]))

; 5 --------------------------------------------------------------------------

; hovering? : Jenga -> Boolean
; determines whether or not any level in a Jenga tower contains no blocks at all
(check-expect (hovering? J0) #false)
(check-expect (hovering? J1) #false)
(check-expect (hovering? J4) #true)
(check-expect (hovering? J5) #true)

(define (hovering? jenga)
  (cond [(string? jenga) #false]
        [(level? jenga) (or (no-blocks-level? jenga)
                            (hovering? (level-above jenga)))]))

; no-blocks? : Level -> Boolean
; determines whether or not a level in a Jenga tower contains no blocks at all
(check-expect (no-blocks-level? (make-level UPDOWN #true #false #true J0)) #false)
(check-expect (no-blocks-level? (make-level UPDOWN #false #false #false J0)) #true)

(define (no-blocks-level? jenga)
  (= (num-blocks-level jenga) 0))

; 6 --------------------------------------------------------------------------

; unbalanced? : Jenga -> Boolean
; determines whether or not a Jenga tower will fall
; (any level in the tower except the topmost contains
; only a single block on either side and no others)
(check-expect (unbalanced? J3) #true)
(check-expect (unbalanced? J2) #false)

(define (unbalanced? jenga)
  (cond [(string? jenga) #false]
        [(level? jenga) (or (unbalanced-level? jenga)
                            (unbalanced? (level-above jenga)))]))

; unbalanced-level? : Level -> Boolean
; determines whether or not a level in a Jenga tower will fall
(check-expect (unbalanced-level? (make-level UPDOWN #true #true #true J0)) #false)
(check-expect (unbalanced-level? (make-level UPDOWN #false #false #true J0)) #true)
(check-expect (unbalanced-level? (make-level UPDOWN #true #false #false J0)) #true)

(define (unbalanced-level? level)
  (or (and (level-side1 level) (not (level-mid level)) (not (level-side2 level)))
      (and (not (level-side1 level)) (not (level-mid level)) (level-side2 level))))
