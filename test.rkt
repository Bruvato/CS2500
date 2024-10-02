#lang htdp/bsl


 (require 2htdp/image)

; -----Everyday I’m Shuffling-----

; -----Exercise 1-----

; A CSG (CoinShuffleGame) is a (make-csg CoinOrFalse CoinOrFalse CoinOrFalse)
(define-struct csg [left middle right])
; and represents the three cups in a coin shuffle game, and what is under them

; Examples
(define THREE-NO-COIN (make-csg #false #false #false))
(define THREE-PENNIES (make-csg 0.01 0.01 0.01))
(define ONE-QUARTERS-TWO-DIME (make-csg 0.25 0.10 0.10))

; Template
(define (csg-temp csg)
  (...(csg-left csg)...(csg-middle csg)...(csg-right csg)))

; A CoinOrFalse is one of:
; - #false
; - Number
; and represents either no coin or the coin's monetary value

; Examples
(define NO-COIN #false)
(define PENNY 0.01)
(define NICKEL 0.05)
(define DIME 0.10)
(define QUARTER 0.25)

; Template
(define (coin-or-false-temp coin-or-false)
  (cond [(false? coin-or-false) ...]
        [(equal? coin-or-false 0.01) ...]
        [(equal? coin-or-false 0.05) ...]
        [(equal? coin-or-false 0.10) ...]
        [(equal? coin-or-false 0.25) ...]))

; A Guess is one of:
; - "left"
; - "middle"
; - "right

; Examples
(define LEFT "left")
(define MIDDLE "middle")
(define RIGHT "right")

; Template
(define (guess-temp guess)
  (cond [(string=? guess "left") ...]
        [(string=? guess "middle") ...]
        [(string=? guess "right") ...]))

; -----Exercise 2-----

; shuffle-right: CSG -> CSG
; moves all cup values in a CSG csg to the right (right cup’s contents loop back to the left cup)
(define (shuffle-right csg)
  (make-csg (csg-right csg) (csg-left csg) (csg-middle csg)))

; given: #false for csg-left, 0.01 for csg-middle, #false for csg-right
; expected: (make-csg #false #false 0.01)
(check-expect (shuffle-right (make-csg NO-COIN PENNY NO-COIN)) (make-csg NO-COIN NO-COIN PENNY))

; -----Exercise 3-----

; csg-value: CSG, Guess -> Number
; Determines the monetary value of the guessed cup given a CSG csg and a Guess guess (A cup with no coins is worth 0)
(define (csg-value csg guess)
  (cond [(string=? guess LEFT)(if (false? (csg-left csg)) 0 (csg-left csg))]
        [(string=? guess MIDDLE)(if (false? (csg-middle csg)) 0 (csg-middle csg))]
        [(string=? guess RIGHT)(if (false? (csg-right csg)) 0 (csg-right csg))]))

(check-expect (csg-value (make-csg PENNY NO-COIN NO-COIN) LEFT) PENNY)
(check-expect (csg-value (make-csg NO-COIN PENNY NO-COIN) MIDDLE) PENNY)
(check-expect (csg-value (make-csg NO-COIN NO-COIN PENNY) RIGHT) PENNY)

; -----Exercise 4-----

; inflation: CSG, Number -> CSG
; adds a monetary value num to all of the coins in the cup given a CSG csg and a number num (leaves empty cups as is)
(define (inflation csg num)
  (make-csg (if (false? (csg-left csg)) #false (+ num (csg-left csg))) 
            (if (false? (csg-middle csg)) #false (+ num (csg-middle csg)))
            (if (false? (csg-right csg)) #false (+ num (csg-right csg)))))

(check-expect (inflation (make-csg PENNY NO-COIN QUARTER) DIME) (make-csg 0.11 NO-COIN 0.35))

; -----Challenge: Stacking bricks-----


; draw-brick-1: Number, Number, String, String -> Image
; draws a brick with height bh, motar length ml/2, color bc, motar color mc
(define (draw-brick-1 bh ml bc mc)
  (overlay (rectangle (* 2 bh) bh "solid" bc)
           (rectangle (+ (* 2 bh) ml) (+ bh ml) "solid" mc)))

; draw-blue-half-brick-1: Number Number, String, String, String -> Image
; draws a half brick with height bh, motar length ml/2, color bc, motar color mc aligned in a direction dir
(define (draw-half-brick-1 bh ml bc mc dir)
 (overlay/align dir "middle" (rectangle bh bh "solid" bc)
               (rectangle (+ bh (/ ml 2)) (+ bh ml) "solid" mc)))

(define (draw-row-1 bh ml bc mc)
  (beside
   (draw-brick-1 bh ml bc mc)
   (draw-brick-1 bh ml bc mc)
   (draw-brick-1 bh ml bc mc)
   (draw-brick-1 bh ml bc mc)
   (draw-brick-1 bh ml bc mc)))

(define (draw-half-row-1 bh ml bc mc)
  (beside
   (draw-half-brick-1 bh ml bc mc "left")
   (draw-brick-1 bh ml bc mc)
   (draw-brick-1 bh ml bc mc)
   (draw-brick-1 bh ml bc mc)
   (draw-brick-1 bh ml bc mc)
   (draw-half-brick-1 bh ml bc mc "right")))

; herringbone-1: Number, Number, String, String
; draws a staggered pattern with brick height bh, motar length ml/2 surrounding the brick, brick color bc, motar color mc
(define (staggered-1 bh ml bc mc)
  (above (draw-half-row-1 bh ml bc mc)
         (draw-row-1 bh ml bc mc)
         (draw-half-row-1 bh ml bc mc)
         (draw-row-1 bh ml bc mc)
         (draw-half-row-1 bh ml bc mc)
         (draw-row-1 bh ml bc mc)
         (draw-half-row-1 bh ml bc mc)
         (draw-row-1 bh ml bc mc)))

(check-expect (staggered-1 20 6 "blue" "grey") ...)


(define (draw-brick-2 bh ml bc mc) 
)
