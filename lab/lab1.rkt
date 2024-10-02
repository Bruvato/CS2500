#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)



; A Shape is one of:
; - "circle"
; - "square"
; - "triangle"
; and represents a kind of shape

(define CIRCLE "circle")
(define SQUARE "square")
(define TRIANGLE "triangle")

; Template
(define (shape-temp shape)
  (cond [(string=? shape "circle")   ...]
        [(string=? shape "square")   ...]
        [(string=? shape "triangle") ...]))




; draw: Shape -> Image
; Draw the shape
(define (draw shape)
  (cond [(string=? shape "circle")   (circle 10 "solid" "red")]
        [(string=? shape "square")   (square 5 "solid" "blue")]
        [(string=? shape "triangle") (triangle 7 "solid" "orange")]))
 
(check-expect (draw CIRCLE)   (circle 10 "solid" "red"))
(check-expect (draw SQUARE)   (square 5 "solid" "blue"))
(check-expect (draw TRIANGLE) (triangle 7 "solid" "orange"))

; draw/scene: Shape -> Image
; Overlays the shape on an empty scene
(define (draw/scene shape)
  (overlay (draw shape) (empty-scene 100 100)))

(check-expect (draw/scene CIRCLE) (overlay (draw CIRCLE) (empty-scene 100 100)))
(check-expect (draw/scene SQUARE) (overlay (draw SQUARE) (empty-scene 100 100)))
(check-expect (draw/scene TRIANGLE) (overlay (draw TRIANGLE) (empty-scene 100 100)))

; next-shape: Shape -> Shape
; Takes a shape and outputs the "next" shape

(define (next-shape shape)
  (cond [(string=? shape CIRCLE) SQUARE]
        [(string=? shape SQUARE) TRIANGLE]
        [(string=? shape TRIANGLE) CIRCLE]))

(check-expect (next-shape CIRCLE) SQUARE)
(check-expect (next-shape SQUARE) TRIANGLE)
(check-expect (next-shape TRIANGLE) CIRCLE)


; main: Shape -> void
; Takes a shape and cycles through all 3 shapes
(define (main shape)
  (big-bang shape
    (on-tick next-shape 1/3)
    (to-draw draw/scene)))

(main CIRCLE)

; -------------------------------------------

(require 2htdp/image)
(require 2htdp/universe)

(define EASY-UP 5)
(define MEDIUM-UP 3)
(define HARD-UP 1)
(define HEIGHT 100)

; A Terrain is one of:
; - [0, 50)
; - [50, 80)
; - [80, 100]
; and represents a section of terrain with a certain difficulty

; place-terrain: Terrain -> Image
; takes a Terrain t representing the height of the climber
; and draws a shape on a scene tha tis HEIGHT tall
(define (place-terrain t)
  
  (above (rectangle 100 20 "solid" "red")
         (rectangle 100 30 "solid" "yellow")
         (rectangle 100 50 "solid" "green")))

