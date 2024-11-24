#lang htdp/isl+


; base case = empty list
; cannot terminate bc can never reach empty


; Counter ex (list 0)


; quick sort
; TERMIN
; base case + step (induction)
; base case : list contains all the same number

; step must get closer to the base case


; by def of the larger list, it will not include the pivot so the
; we can get a new pivot by reversing a list

; termination statement is bult into data def = structural

; generating new information = generative

; trivial case = non recursive call
; nontrivial case = recursive call

; every input is differnt

; base case = no recursive call
; 
; how each input is differnt
; how it gets closer to base case


; gcd-structural : PosInt PosInt -> PosInt
; given two positive integers, finds the greatest common divisor between them

; N[>= 1] N[>= 1] -> N
; finds the greatest common divisor of n and m
(define (gcd-structural a b)
  (local [(define smaller (min a b))
          (define larger (max a b))
          (define (greatest-div i)
            (if (= (remainder larger i) (remainder smaller i) 0)
                i
                (greatest-div (sub1 i))))]
    (greatest-div smaller)))

(check-expect (gcd-structural 6 25) 1)
(check-expect (gcd-structural 18 24) 6)

; N[>= 1] N[>= 1] -> N
; finds the greatest common divisor of n and m
(define (gcd-generative a b)
  (local [(define smaller (min a b))
          (define larger (max a b))]
    (cond [(= smaller larger) smaller]
          [(= (remainder larger smaller) 0) smaller]
          [else (gcd-generative smaller (remainder larger smaller))])))

(check-expect (gcd-generative 6 25) 1)
(check-expect (gcd-generative 18 24) 6)


; gcd(S,L) = gcd(S, remainder(L,S))

; ------------------------------------

(define (quick-sort lon)
  (cond [(empty? lon) '()]
        [(cons? lon) 
         (local [(define pivot (first lon))
                 (define smallers (filter (λ (n) (< n pivot)) lon))
                 (define equals (filter (λ (n) (= n pivot)) lon))
                 (define largers (filter (λ (n) (> n pivot)) lon))]
           (append (quick-sort smallers) equals (quick-sort largers)))]))

(check-expect (quick-sort '(1 1 1 1 1)) '(1 1 1 1 1))

; ---------------------------

(require 2htdp/image)

; Number -> Image
; creates Sierpinski triangle of size side
 
(define (sierpinski side)
  (cond [(<= side 10) (triangle side 'outline 'red)]
        [else
         (above (sierpinski (/ side 2)) (beside (sierpinski (/ side 2)) (sierpinski (/ side 2))))]))

(sierpinski 100)




