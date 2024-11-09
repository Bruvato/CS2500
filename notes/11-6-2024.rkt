#lang htdp/isl+


; [Num -> Num] Num Num Num -> Num
; find the midpoint of the interval, check if its nearly a root,
; and it not, recur on one or the other half of the interval

; TERMINATION: the difference between min and max is so small that were close enough
; or we get lucky and find the root exactly at an endpoint
(define (find-root f min max d)
  (local [(define f-min (f min))
          (define f-max (f max))
          (define mid (/ (+ min max) 2))
          (define f-mid (f mid))
          (define min-mid-changes-sign (negative? (* f-min f-mid)))
          (define mid-max-changes-sign (negative? (* f-mid f-max)))]
    (cond [(roughly-zero? f-min 0) min]
          [(roughly-zero? f-max 0) max]
          [(roughly-zero? f-mid ) mid]
          [(roughly-zero? (- min max) d) mid]
          [(min-mid-changes-sign) (find-root f min mid d)]
          [(mid-max-changes-sign) (find-root f mid max d)]
          [else ...])))

(define (roughly-zero? n d)
  (< (abs n) d))

(define (f x)
  (* (- x 1) x (+ x 1)))

(find-root f -100 -0.5 0.00000001)
(check-within (f (find-root f -1.5 1.51 0.00000001)) 0 0.00000001)



; =====


; start with an initial guess x0 then iterate
; 
(define (newton-root f)
  (local [(define f-deriv (f deriv))
          (define (newton-root/guess guess)
            (if (roughly-zero? (f guess) DELTA)
                guess
                (local [(define new-guess
                          (- guess (/ (f guess) (f-deriv guess))))]
                  (newton-root/guess new-guess)))
            )]
    (newton-root/guess 0)))



(define DELTA 0.00000001)
(define (deriv f)
  ...)


; ======================

(define (add-sterpinski scene0 a b c)
  (cond
    [(too-small? a b c) scene0]
    [else
     (local
       ((define scene 1 (add-triangle scene0 a b c))
        (define mid-a-b (mid-point a b))
        

(define (add-triangle scene a b c)
  (add-line
   (add-line (add-line scene
                       (posn-x a) (posn-t