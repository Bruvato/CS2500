#lang htdp/isl

(require 2htdp/image)

; goal: the text image in the upper left cornoer of the screen
; why doesnt work



;(define txt (text "happy fourth week of school"  44 "red"))
;(define sce (empty-scene 600 200))
;(define lft (quotient (image-width txt) 2))
;(define top (quotient (image-height txt) 2))

;(place-image txt
;             lft top
;             sce)

#;(local [bunch of definitions]
  expression)


(define (scn+txt scn string)
  (local [(define txt (text string  44 "red"))
          (define lft (quotient (image-width txt) 2))
          (define top (quotient (image-height txt) 2))] 
    (place-image txt lft top scn)))

; uses nearest top left definition
; inner most over shadows outer

; in scope = in local
; out of scope = outside of local


(local [(define-struct s [x y])]
  (make-s 1 2))



(define something
  (local [(define (add-one x)
            (+ x 1))]
    add-one))

(something 10)

; we can mention functions without calling them

; NELoN non empty list of numbers
; - cons Number empty
; - cons Number NELoN

(define (nelon-temp nel)
  (cond [(empty? (rest nel))( ... (first nel) ...)]
        [(cons? (rest nel)) ... (first nel)
                            ... (nelon-temp (rest nel))]))

(define (largest nel)
  (cond [(empty? (rest nel)) (first nel)]
        [(cons? (rest nel))
         (if (> (first nel) (largest (rest nel)))
             (first nel)
             (largest (rest nel)))]))

(define (maximum nel)
  (cond [(empty? (rest nel)) (first nel)]
        [else (local ((define fst (first nel))
                      (define max (maximum (rest nel))))
                (cond ((< fst max) 
                      


(define (biggest nel)
  (cond [(empty? (rest nel)) (first nel)]
        [else (max (first nel) (biggest (rest nel)))]))
