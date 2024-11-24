#lang htdp/isl+


(require racket)
(require (only-in racket/control abort))

#|
(* 2 (+ 1 (abort (* 5 (- 6 2)))))
 
(string-append "Your total is: $"
            ((first (list (λ (n) (abort "Hi Mom!"))
                          number->string))
             80))
 
(string-append "Your total is: $"
            ((second (list (λ (n) (abort "Hi Mom!"))
                           number->string))
             80))
 
(map (λ (f) (f 3)) (list add1 sqr number->string abort odd?))
 
((λ (x) (+ 20 (abort x))) "Oopsies")
 
(rest (map identity (list abort)))
 
(map abort (list 1 2 3))
 
(local [(define (loop x)
          (loop x))]
  (loop (abort "Hello, World!")))
 
 (local [(define (loop x)
           (loop x))]
   (abort (loop "Hello, World!"))) ; watch out for this one!

|#


; abort : 


; product : [List-of Number] -> Number
; The product of lon
#;(define (product lon)
  (cond
    [(empty? lon) 1]
    [else (if (zero? (first lon))
              (abort 0)
              (* (first lon) (product (rest lon))))]))

(check-expect (product (list 5 8 1 0 2 9 3)) 0)



; ? -> ?
; [? -> ?] -> ?
; [[? -> ?] -> ?] -> ?
; {X} [[? -> ?] -> ?] -> X
; {X} [[? -> ?] -> X] -> X
; {X} [[X -> ?] -> X] -> X
; {X} [[X -> Y] -> X] -> X










