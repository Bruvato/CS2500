#lang htdp/isl

; A FIAS (Finite Increasing Arithmetic Sequence) is a:
; (make-fias Number Number Positive)
(define-struct fias [min max step])
; where (make-fias min max step) represents all numbers
; of the form min + (k * step), where k is a natural number,
; such that min + (k * step) < max.
 
(define fias-empty (make-fias 1 1 0.25)) ; empty sequence, as min >= max
(define fias-1 (make-fias 0 1 0.25)) ; sequence with the elements (0, .25, .5, .75)
 
; fias-temp : FIAS -> ?
#;(define (fias-temp fias)
  (... (fias-min fias) ... (fias-max fias) ... (fias-step fias) ...))

; -----
; empty-fias? : FIAS -> Boolean
; determines if the given FIAS is empty
(define (empty-fias? fias)
  (>= (fias-min fias) (fias-max fias)))

; -----
; next-sequence : FIAS -> FIAS
; returns a new FIAS where the min is the original FIAS's min plus its step
(define (next-sequence fias)
  (make-fias
    (+ (fias-min fias) (fias-step fias))
    (fias-max fias)
    (fias-step fias)))

; -----
; fias-temp : FIAS -> ?
(define (fias-temp fias)
  (cond
    [(empty-fias? fias) ...]
    [else (... (fias-min fias) ... (fias-temp (next-sequence fias)) ...)]))

; 4 -----------------------------------------------------------

; sum-fias : FIAS -> Number
; sums the elements of a FIAS
(check-expect (sum-fias (make-fias 0 5 1)) 10) ; 0 1 2 3 4 

(define (sum-fias fias)
  (cond
    [(empty-fias? fias) 0]
    [else (+ (fias-min fias)
             (sum-fias (next-sequence fias)))]))


; 5 ---------------------------------

; product-fias : FIAS -> Number
; multiplies the elements of a FIAS
(check-expect (product-fias (make-fias 1 5 1)) 24)

(define (product-fias fias)
  (cond
    [(empty-fias? fias) 1]
    [else (* (fias-min fias)
             (product-fias (next-sequence fias)))]))

; 6 --------------------------------------

; list-fias : FIAS -> [List-of Numbers]
; lists the elements of FIAS
(check-expect (list-fias (make-fias 0 5 1)) '(0 1 2 3 4))

(define (list-fias fias)
  (cond
    [(empty-fias? fias) '()]
    [else (cons (fias-min fias)
                (list-fias (next-sequence fias)))]))

; 7 --------------------------------------------------

(check-expect (operate-fias (make-fias 1 5 1) 0 +) 10)

(define (operate-fias fias base op)
  (cond
    [(empty-fias? fias) base]
    [else (op (fias-min fias)
              (operate-fias (next-sequence fias) base op))]))

; 8 ---------------------------------------------

(check-expect (square-fias? (make-fias 1 5 1)) #t)
(check-expect (square-fias? (make-fias 2 3 1)) #f)

(define (square-fias? fias)
  (cond
    [(empty-fias? fias) #f]
    [else (or (integer? (sqrt (fias-min fias)))
              (square-fias? (next-sequence fias)))]))

; 9 -------------------------------------------

(check-expect (even-fias? (make-fias 1 5 1)) #t)
(check-expect (even-fias? (make-fias 1 5 2)) #f)

(define (even-fias? fias)
  (cond
    [(empty-fias? fias) #f]
    [else (or (even? (fias-min fias))
              (even-fias? (next-sequence fias)))]))

; 10 --------------------------------------------

(check-expect (condition-fias? (make-fias 1 5 1) even?) #t)


(define (condition-fias? fias condition)
  (cond
    [(empty-fias? fias) #f]
    [else (or (condition (fias-min fias))
              (condition-fias? (next-sequence fias) condition))]))

;  -----------------------------------------------

(define-struct circl [radius mode c])
(define-struct squar [side-length mode c])
(define-struct rectangl [width height mode c])
; A Shape is one of:
; - (make-circl Number Mode Color)
; - (make-squar Number Mode Color)
; - (make-rectangl Number Number Mode Color)
 
; and represents either:
; - the radius in pixels, mode, and color of a circle
; - the side length in pixels, mode, and color of a square
; - the width and height in pixels, the mode, and color of a rectangle

; Shape Examples
(define CIRCLE (make-circl 50 "solid" "blue"))
(define SQUARE (make-squar 100 "solid" "red"))
(define RECTANGLE (make-rectangl 50 100 "solid" "green"))

; Shape Tempalte
; shape-temp : Shape -> ?
(define (shape-temp shape)
  (cond
    [(circl? shape) ...]
    [(squar? shape) ...]
    [(rectangl? shape) ...]))
 
; A Mode is one of:
; - "solid"
; - "outline"

; 11 -----------------------------------------------


; 12 -----------------------------------------------

(check-expect (filter-circle (list CIRCLE SQUARE RECTANGLE)) (list CIRCLE))

; [List-of Shape] -> [List-of Shape]
(define (filter-circle los)
  (filter circl? los))

; 13 -------------------------------------------------

#;(define (stack-shapes los)
  (foldr above empty-image los))




; 14 --------------------------------------------

; [List-of Boolean] Number [Boolean -> Number] -> Number
(define (a supercut of us)
  (+ of
     (if (empty? supercut)
         (us #f)
         (us (first supercut)))))

; the X Y [X Y -> ] [ -> X] the Y
; the the i [the dark -> ] [ -> the] the dark

; the : {X,Y} Y X [Y X -> X] [X -> Y] Y X -> Z
; moments : Y
; i : X
; play :[Y X -> X]
; in : [X -> Y]
; the : Y
; dark : X
(define (the moments i play in the dark)
  (play (in (play the dark)) (play moments i)))

; [my -> Boolean] [heart -> String] [heart -> Boolean] heart

; come :
; home : []
; to : [_ -> String
; my : String
; heart : 
(define (come home to my heart)
  (cond [(home my) (to heart)]
        [(my heart) " "]
        [else ""]))
    
