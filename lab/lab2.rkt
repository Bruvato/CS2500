#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

;; 1 2 3 4 5 -> 3
;; 1 2 2 3 4 -> 2.8
;; 9 -> 9
;; -> no average

;; average = sum / count

(define-struct current-average[sum count])
;; A current-average is a (make-current-average Rational PosInt)
;; which represents a sum and count respectively of an average
(define AVG1 (make-current-average 10 4))
(define AVG2 (make-current-average 9 1))
(define AVG3 (make-current-average 0 0))
(define AVG4 (make-current-average -5 2))

;; current-average-temp : current-average -> ?
(define (current-average-temp avg)
  (...(current-average-sum avg) ... (current-average-count avg) ...))

;; next-average : current-average number -> current-average
;; Computes an updated current-average that includes the new number

#|
(define (next-average avg num)
  (make-current-average (+ (current-average-sum avg) num)
                       (add1
|#

(check-expect (next-average AVG1 5) (make-current-average 15 5))
(check-expect (next-average AVG2 0) (make-current-average 9 2))
(check-expect (next-average AVG3 9) (make-current-average 9 1))

;; -----------------------------------------------------------------------

;; A Point is two numbers is a (make-point num num)
;; and represents a point in 2D space

(define-struct point [x y])
(define P1 (make-point 20 50 ))
(define P2 (make-point 90 10))

; temp


(define-struct board [p1 p2])

(define BOARD (make-board P1 P2))

(define (board-temp bd)
  (... (point-temp (board-p1 bd)...)
       (point-temp (board-p2 bd)...)...))


(define BOARD-POINT (circle 3 "solid" "blue"))
(define MID-POINT (circle 3 "solid" "blue"))
(define GUESS-POINT (circle 3 "solid" "blue"))

;; place-point : Point Image Image -> Image
;; places a

(define (place-point pt image bg)
  (place-image (point-x pt) (point-y pt) bg))


; draw-board Board -> Image
; rednders a given board

(define (draw-board bd)
  (place-point (board-p1 bd) BOARD-POINT
               (place-point (board-p2 bd) BOARD-POINT BOARD)))

