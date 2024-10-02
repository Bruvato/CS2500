#lang htdp/bsl


; A ListOfNumbers (LON) is one of
; - '()
; - (cons Number LON)
; interp...

; examples
(define lon0 '())
(define lon1 (cons 10 '()))
(define lon2 (cons 20 (cons 10 '())))
(define lon3 (cons 30 (cons 20 (cons 10 '()))))

; template
(define (lon-temp lon)
  (cond [(empty? lon) ...]
        [(cons? lon) (... (first lon) ... (lon-temp (rest lon)) ...)]))



; design a func sum-list, accepts LON and returns sum

; sum-list : LON -> Number
; purpose
(check-expect (sum-list lon0) 0)
(check-expect (sum-list lon1) (+ 10 0))
(check-expect (sum-list lon2) (+ 20 (+ 10 0)))
(check-expect (sum-list lon3) (+ 30 (+ 20 (+ 10 0))))

(define (sum-list lon)
  (cond [(empty? lon) 0]
        [(cons? lon)
         (+ (first lon)
            (sum-list (rest lon)))]))

; define a func num-numbers that counts the nums in a list

(check-expect (num-numbers lon0) 0)
(check-expect (num-numbers lon1) 1)
(check-expect (num-numbers lon2) 2)
(check-expect (num-numbers lon3) 3)

; num-numbers : LON -> Number
; purpse
(define (num-numbers lon)
  (cond [(empty? lon) 0]
        [(cons? lon)
         (+ 1
            (num-numbers (rest lon)))]))


; ---------------------

; A LOS list of strings is one of
; - '()
; - (cons String LOS)
;interp
; examples

(define los0 '())
(define los1 (cons "ben" los0))
(define los2 (cons "sam" los1))
(define los3 (cons "amal" los2))
(define los4 (cons "matthias" (cons "amal" (cons "ben" (cons '())))))

; template
(define (los-temp los)
  (cond [(empty? los) ...]
        [(cons? los) (... (first los)
                          (los-temp (rest los)))]))


; designa func num-strings that accepts a LOS and returns its length

; num-strings : LOS -> Number
(define (num-strings los)
  (cond [(empty? los) 0]
        [(cons? los)
         (+ 1
            (num-strings (rest los)))]))

; --------------------------------------

(define (count-letters los)
  (cond [(empty? los) 0]
        [(cons? los)
         (+ (string-length (first los))
            (count-letters (rest los)))]))

(check-expect (count-letters los3) 10)

; ---------------------------------------

; design the func contains-sum? that accepts LOS and returns whether or not it contains the stirng "sam"

(define (contains-sam? los)
  (cond [(empty? los) #false]
        [(cons? los)
         (or (string=? (first los) "sam")
             (contains-sam? (rest los)))]))

(check-expect (contains-sam? los2) #true)

; -----------------------------------------------------

(define (contains-name? los name)
  (cond [(empty? los) #false]
        [(cons? los)
         (or (string=? (first los) name)
             (contains-name? (rest los) name))]))

; -------------------------------------

; longest string first longest string

; longest-string : LOS -> String
(check-expect (longest-string los0) "")
(check-expect (longest-string los1) "amal")

(define (longest-string los)
  (cond [(empty? los) ""]
        [(cons? los)
         (pick-longer-string (first los)
              (longest-string (rest los)))]))

; pick-longer-string : String String -> String
; finds the longer string of the given strings, or second in case equal length
(check-expect (pick-longer-string "ab" "a") "ab")
(check-expect (pick-longer-string "ab" "ac") "ac")
(check-expect (pick-longer-string "a" "ac") "ac")

(define (pick-longer-string s1 s2)
  (if (> (string-length s1) (string-length s2)) s1 s2))

; TEMPLATES GO W DATA DEFENITIONS, NOT STRUCTS

; --------------------------------------------------------------------




