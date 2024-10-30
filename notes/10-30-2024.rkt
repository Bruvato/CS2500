#lang htdp/isl+

; 1) what does the accum represent?
; 2) where does it start?
; 3) how is it updated?
; 4) how can we use it?

(define (my-func a1 a2 a3)
  ; ACCUM: represents
  (local [(define (my-func/acc a1 a2 a3 acc)
            ...)]
    (my-func/acc a1 a2 a3 ...))) ; initial accumulator

; ----------------------

; path? : Graph Node Node -> Bool
(define (path? g s e)
  ; ACCUM: represents the nodes visited so far in the search (1)
  (local [(define (path?/acc g s e already-visited)
            (cond [(symbol=? s e) #t]
                  [(member s already-visited) #f]
                  [else (ormap (λ(next) (path?/acc g next e (cons s already-visited))) (neighbors-of s g))]) ; (3): (cons s already-visited)


            )]
    (reverse (path?/acc g s e '())))) ; (2): '()

; gets neighboring nodes
(define (neighbors-of s g)
  ...)

; ==============================================


; |---6---|--3--|-2-|---4---|-----7-----|1|

; 6, 6+3, 6+3+2, ...
; rel->abs
(define (rel->abs lon)
  (cond [(empty? lon) '()]
        [(cons? lon)
         (local [(define absrest (rel->abs (rest lon)))]
           (cons (first lon) (map (λ(n) (+ n (first lon))) absrest)))]))

(define (rel->abs2 lon)
  ; ACCUM : represents abs distance from start ; (1)
  ; purpose+signature
  (local [(define (rel->abs2/acc lon dist-so-far)
            (cond [(empty? lon) '()]
                  [(cons? lon)
                   (cons (+ (first lon) dist-so-far) ; (4): dist-so-far
                         (rel->abs2/acc (rest lon) (+ (dist-so-far (first lon)))))]) ; (3): (+ (dist-so-far (first lon)))

            )]
    (rel->abs2/acc lon 0))) ; (2): 0

; ---------------

; implement reverse list
        