#lang htdp/isl+


; A GameOfLife is a square [Grid-of Boolean], i.e.
; a [List-of [List-of Boolean]]
; where the length of the outer list is the same as all of the inner lists
; and represents alive (#t) and dead (#f) cells
(define TINY-GAME
  '((#f #f #f #f)
    (#f #t #t #f)
    (#f #t #f #f)
    (#f #f #f #f)))

(define TINY-GAME+1
  '((#f #f #f #f)
    (#f #t #t #f)
    (#f #t #t #f)
    (#f #f #f #f)))

; neighbors : Number Number GameOfLife -> [List-of Boolean]
; The neighbors of position (r, c) in gol (r = row, c = column)
(define (neighbors r c gol)
  (list (get-cell (sub1 r) (sub1 c) gol)
        (get-cell (sub1 r) c        gol)
        (get-cell (sub1 r) (add1 c) gol)
        (get-cell r        (sub1 c) gol)
        (get-cell r        (add1 c) gol)
        (get-cell (add1 r) (sub1 c) gol)
        (get-cell (add1 r) c        gol)
        (get-cell (add1 r) (add1 c) gol)))
(check-expect (neighbors 1 1 TINY-GAME)
              (list #f #f #f
                    #f    #t
                    #f #t #f))
(check-expect (neighbors 3 1 TINY-GAME)
              (list #f #t #f
                    #f    #f
                    #f #f #f))

         
; get-cell : Number Number GameOfLife -> Boolean
; The value of the cell at y, x (looping around the ends of the list if necessary)
(define (get-cell y x gol)
  (local [(define n     (length gol))
          (define y-mod (modulo y n))
          (define x-mod (modulo x n))]
    (list-ref (list-ref gol y-mod) x-mod)))

(check-expect (get-cell 0 0 TINY-GAME) #f)
(check-expect (get-cell 4 4 TINY-GAME) #f)
(check-expect (get-cell 1 2 TINY-GAME) #t)
(check-expect (get-cell 5 6 TINY-GAME) #t)


; 4 ============================

; num-true : [List-of Booleans] -> Natural
; outputs the number of elements that are #t given a list of booleans
(define (num-true lob)
  (foldr (λ(b acc) (if b (add1 acc) acc)) 0 lob))

(check-expect (num-true (list #t #t #t)) 3)
(check-expect (num-true (list #t #f #f #t)) 2)
(check-expect (num-true (list #f #f #f)) 0)
(check-expect (num-true (list))0)


; 5 =======================================

; if alive & < 2 alive neighbors -> dies
; if alive & > 3 alive neighbors -> dies
; if alive & 2 or 3 alive nieghbors -> lives
; if dead & 3 alive nieghbors -> lives

; new-value/conway : Boolean [List-of Boolean] -> Boolean
; outputs the new value of a cell given a cell’s current value
; and the list of its neighbors’ current values
#;(define (new-value/conway b lob)
    (local [(define num-alive-neighbors (num-true lob))]
      (cond [(and b (< num-alive-neighbors 2)) #f]
            [(and b (> num-alive-neighbors 3)) #f]
            [(and b (or (= num-alive-neighbors 2) (= num-alive-neighbors 3))) #t]
            [(and (not b) (= num-alive-neighbors 3)) #t]
            [(and (not b) (not (= num-alive-neighbors 3))) #f])))


(check-expect (new-value/conway #t '(#t #t #t #t #f #f #f #f)) #f)
(check-expect (new-value/conway #t '(#t #f #f #f #f #f #f #f)) #f)
(check-expect (new-value/conway #t '(#t #t #f #f #f #f #f #f)) #t)
(check-expect (new-value/conway #t '(#t #t #t #f #f #f #f #f)) #t)
(check-expect (new-value/conway #f '(#t #t #t #f #f #f #f #f)) #t)
(check-expect (new-value/conway #f '(#t #t #f #f #f #f #f #f)) #f)

; 6 =====================================

; next-grid : GameOfLife -> GameOfLife
; outputs the new grid
(check-expect (next-grid TINY-GAME) TINY-GAME+1)

(define (next-grid gol)
  (local [(define rows (length gol))
          (define cols (length (first gol)))]
    (build-list rows (λ (row)
                       (build-list cols (λ (col)
                                          (new-value/conway
                                           (get-cell row col gol)
                                           (neighbors row col gol))))))))

; =====================================

(require 2htdp/image)
(require 2htdp/universe)

(define CELL-SIZE 5)
(define LIVE-CELL (color-frame "cadetblue" (square CELL-SIZE "solid" "seashell")))
(define DEAD-CELL (square CELL-SIZE "solid" "white"))

; 7 =======================

; draw-grid : GameOfLife -> Image
; Draw the game of life
(define (draw-grid gol)
  (foldr (λ(lob acc)
           (above (foldr (λ(b acc)
                           (beside (if b LIVE-CELL DEAD-CELL) acc)) empty-image lob) acc)) empty-image gol))

(check-expect (draw-grid TINY-GAME)
              (above (beside DEAD-CELL DEAD-CELL DEAD-CELL DEAD-CELL)
                     (beside DEAD-CELL LIVE-CELL LIVE-CELL DEAD-CELL)
                     (beside DEAD-CELL LIVE-CELL DEAD-CELL DEAD-CELL)
                     (beside DEAD-CELL DEAD-CELL DEAD-CELL DEAD-CELL)))

; main : GameOfLife -> GameOfLife
; Run conway's game of life
#;(define (main gol)
    (big-bang gol
      [on-tick next-grid 1/10]
      [to-draw draw-grid]))

#;(main
   (list (list #f #f #f #f #f #f #f)
         (list #f #f #f #f #f #f #f)
         (list #f #f #f #f #f #f #f)
         (list #f #f #f #f #f #f #f)
         (list #f #f #f #f #t #t #t)
         (list #f #f #f #f #t #f #f)
         (list #f #f #f #f #f #t #f)))

; ==================

; A Coord is a (list Number Number)

; A FirstState is a [List-of Coord]
; It represents a list of coordinates that are alive at the onset of the game

; 8 =============================

; initial-grid : Number FirstState -> GameOfLife
; A game of life with size grid-size with points in fs alive
; assume grid-size is larger than any number in fs
(define (initial-grid grid-size fs)
  (build-list grid-size (λ (row)
                          (build-list grid-size (λ (col)
                                                  (not (false? (member (list row col) fs))))))))

(check-expect (initial-grid 4 (list (list 1 1)
                                    (list 1 2)
                                    (list 2 1)))
              TINY-GAME)


; 9 ================================

; main : Natural FirstState -> GameOfLife
; Run conway's game of life
(define (main grid-size fs)
  (local [(define gol (initial-grid grid-size fs))]
    (big-bang gol
      [on-tick next-grid 1/10]
      [to-draw draw-grid])))

#;(main
   50
   (list (list 20 20)
         (list 20 21)
         (list 21 20)
         (list 22 20)
         (list 20 24)
         (list 21 24)
         (list 22 24)
         (list 22 23)))

; 10 =======================

; A CellUpdate is a (Boolean [List-of Boolean] -> Boolean) function
; It represents the rules by which a cell's new value is determined from its
; current value and its neighbors' current values

; A NeighborNum is a natural number in the range [0, 8]     
; make-cell-update : [List-of NeighborNum] [List-of NeighborNum] -> CellUpdate
; Output a cell update function with "birth" numbers and "survival" numbers
(define (make-cell-update birth survival) ...)

(define (new-value/conway b lob)
  (local [(define num-alive-neighbors (num-true lob))]
    (cond [(and b (< num-alive-neighbors 2)) #f]
          [(and b (> num-alive-neighbors 3)) #f]
          [(and b (or (= num-alive-neighbors 2) (= num-alive-neighbors 3))) #t]
          [(and (not b) (= num-alive-neighbors 3)) #t]
          [(and (not b) (not (= num-alive-neighbors 3))) #f])))








              




