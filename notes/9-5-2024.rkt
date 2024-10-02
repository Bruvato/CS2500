#lang htdp/bsl

(require 2htdp/image)
(require 2htdp/universe)

(define (f->c temp)
  (* 5/9 (- temp 32)))

;;(check-within (sqrt 2) 1.414 0.001)



(define SUN (circle 25 "solid" "yellow"))
(define SKY (rectangle 300 200 "solid" "lightblue"))
(define SKYDIM (rectangle 300 200 "solid" "blue"))
(define SKYDARK (rectangle 300 200 "solid" "darkblue"))

(define X 220)
(define Y0 70)

(define (sunset t)
  [cond
    [<= t 30] (place-image SUN X (+ Y0 t) SKY)
    [<= t 60] (place-image SUN X (+ Y0 t) SKYDIM)
    
(animate sunset)



;; A LetterGrade is one of A, B, C, D, F



(define (letter-grade-template lg)
  [cond [(string=? lg "A") ...]
        [(string=? lg "B") ...]
        [(string=? lg "C") ...]
        [(string=? lg "D") ...]
        [(string=? lg "F") ...]
        

;; num->grade : Number -> LetterGrade
(define (num->grade g)
  [cond
    [(>= g 90) "A"]
    [(>= g 80) "B"]
    [(>= g 70) "C"]
    [(>= g 60) "D"]
    [else "F"]

    ])

;; gpa-for-letter : LetterGrade -> Number
(define (gpa-for-garde letter-grade)
  [cond [(string=? lg "A") 4]
        [(string=? lg "B") 3]
        [(string=? lg "C") 2]
        [(string=? lg "D") 1]
        [(string=? lg "F") 0]


  ])

(define (num->gpa g)
  (gpa-for-grade (num->grade g)))