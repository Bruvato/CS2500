#lang htdp/bsl

(define-struct pair [element value])
; An Entry is a (make-pair String Nat)
; INTERPRETATION: represents an element with a count

(define (pair-temp p)
  (... (pair-element p) ... (pair-value p) ...))

; A Counter is one of
; - '()
; - (cons Entry Counter)
; INTERPRETATION: represents a multiset (a collection of elements
; like a set, except where an element can appear more than once)

(define (counter-temp counter)
  (cond [(empty? counter) ...]
        [(cons? counter) (... (pair-temp (first counter))
                              ... (counter-temp (rest counter))
                              ...)]))

(define empty-bag (list))
(define marble-bag (list (make-pair "green" 2) (make-pair "red" 5)))
; marble-bag represents a bag with 2 "green" marbles and 5 "red" ones
(define marble-bag2 (list (make-pair "blue" 1) (make-pair "yellow" 3) (make-pair "brown" 2)))


; get : Counter String -> Nat
; Get the count of the given element
(define (get counter element)
  (cond [(empty? counter) (error "not found")]
        [else (if (counts-element? (first counter) element)
                  (pair-value (first counter))
                  (get (rest counter) element))]))

(check-error (get (cons (make-pair "cats" 3) '()) "dogs") "not found")
(check-expect (get (cons (make-pair "cats" 3) (cons (make-pair "dogs" 4) '())) "dogs") 4)

; counts-element? : Entry String -> Boolean
; Does the pair hold a count for the element?
(define (counts-element? pair element)
  (string=? element (pair-element pair)))

(check-expect (counts-element? (make-pair "green" 2) "green") #true)
(check-expect (counts-element? (make-pair "red" 5) "blue") #false)


; --------------------------------------------------------------------

; add-to-counter : Counter String -> Counter
; Add one to the count associated with element or set it to 1
; if it hasn't been seen before
(define (add-to-counter counter element)
  (cond
    [(empty? counter) (cons (make-pair element 1) '())]
    [(cons? counter) (if (counts-element? (first counter) element)
                         (cons (increment-value (first counter))
                               (rest counter))
                         (cons (first counter)
                               (add-to-counter (rest counter) element)))]))

(check-expect (add-to-counter '() "blue") (cons (make-pair "blue" 1) '()))
(check-expect (add-to-counter marble-bag "red")
              (cons (make-pair "green" 2) (cons (make-pair "red" 6) '())))
(check-expect (add-to-counter marble-bag "green")
              (cons (make-pair "green" 3) (cons (make-pair "red" 5) '())))

; increment-value : Entry -> Entry
; Increment the value in pair
(define (increment-value pair)
  (make-pair (pair-element pair) (add1 (pair-value pair))))

(check-expect (increment-value (make-pair "green" 2)) (make-pair "green" 3))
(check-expect (increment-value (make-pair "red" 5)) (make-pair "red" 6))

; 1 -------------------------------------------------

; total-size : Counter -> Number
; grabs the total count of elements in a Counter
(check-expect (total-size marble-bag) 7)
(check-expect (total-size empty-bag) 0)
(check-expect (total-size marble-bag2) 6)

(define (total-size counter)
  (cond [(empty? counter) 0]
        [(cons? counter) (+ (pair-value (first counter))
                            (total-size (rest counter)))]))

; 2 -------------------------------------------------

; initiate-counter : String -> Counter
; creates a Counter with one copy of the provided element
(check-expect (initiate-counter "dog") (list (make-pair "dog" 1)))
(check-expect (initiate-counter "blue") (list (make-pair "blue" 1)))

(define (initiate-counter s)
  (list (make-pair s 1)))

; 3 -------------------------------------------------

; A ListOfString LOS is one of:
; - '()
; - (cons String LOS)
; and represents a list of strings

; all-elements : Counter -> LOS
; creates a ListOfString containing all of the elements a Counter has counted so far
(check-expect (all-elements marble-bag) (list "green" "green" "red" "red" "red" "red"))
(check-expect (all-elements marble-bag2) (list "blue" "yellow" "yellow" "yellow" "brown" "brown"))

(define (all-elements counter)
  (cond [(empty? counter) '()]
        [(cons? counter) (cons (pair-temp (first counter))
                               ... (all-elements (rest counter))
                               ...)]))
;(define (all-elements-helper (

; _____________________________________________


;; 5

(define (make-row lon)
  (cond
    [(empty? lon) '(tr)]
    [(cons? lon) ... (frist lon) ... (rest lon) ...]))

;; make-row : ListOfNumber -> HTML
;; makes an html table row made up of the given list of numbers

(check-expect (make-row '()) '(tr))

(define (make-row lon)
  (cond
    [(empty? lon) '(tr)]
    [(cons? lon) (cons (make-data (frist lon))
                       (make-row (rest lon)))]))

;; make-tds -> [List-of HTML]
(define (make-row lon)
  (cond
    [(empty? lon) '(tr)]
    [(cons? lon) (cons (make-data (frist lon))
                       (make-row (rest lon)))]))


; make-data : Number -> HTML
; make a new data entry given a number
(define (make-data num)
  '(td ,num))