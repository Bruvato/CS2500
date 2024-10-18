#lang htdp/isl+

(require "mimic.rkt")

; A [Counter X] is a [Mapping X PosInt]
; and represents a multiset (a set of elements where an element can appear more than once)

(define MARBLE-BAG (list (make-pair "green" 2) (make-pair "red" 5)))
; MARBLE-BAG represents a bag with 2 "green" marbles and 5 "red" ones

; add-to-counter: [Counter X] X -> [Counter X]
; Add 1 to x in c
(define (add-to-counter c x)
  (update-mapping c x add1 1))
(check-expect (add-to-counter MARBLE-BAG "green") (list (make-pair "green" 3) (make-pair "red" 5)))
(check-expect (add-to-counter MARBLE-BAG "brown")
              (list (make-pair "green" 2) (make-pair "red" 5) (make-pair "brown" 1)))

; total-size: [Counter X] -> Number
; The total size of c
(define (total-size c)
  (foldr (λ (p so-far) (+ (pair-y p) so-far)) 0 c))
(check-expect (total-size MARBLE-BAG) 7)

; initiate-counter: X -> [Counter X]
; Creates a counter with one copy of an element
(define (initiate-counter element)
  (list (make-pair element 1)))

(check-expect (initiate-counter "test") (list (make-pair "test" 1)))
(check-expect (initiate-counter 42) (list (make-pair 42 1)))

; expected-counts: [Counter X] Nat -> [List-of Number]
; Expected counts of elements when grabbing from the counter n times
(define (expected-counts counter n)
  (local [(define size (total-size counter))
          (define (pair-to-number pair)
            (* (/ (pair-y pair) size) n))]
         (map pair-to-number counter)))

(check-expect (expected-counts '() 100) '())
(check-expect (expected-counts MARBLE-BAG 1000) (list (* 2/7 1000) (* 5/7 1000)))

; count: [List-of X] X -> Nat
; How many times does x appear in the list?
(define (count lox x)
  (length (filter (lambda (item) (equal? item x)) lox)))

(check-expect (count '() "b") 0)
(check-expect (count (list "a" "b" "a") "a") 2)


; count-grabs : [Counter X] [List-of X] -> [List-of Number]
; See how many times the elements from this counter are in this list
(check-expect (count-grabs '() '()) '())
(check-expect (count-grabs MARBLE-BAG (list "red" "green" "red" "red")) (list 1 3))

(define (count-grabs counter lox)
  (map (λ(pair) (count lox (pair-x pair))) counter))


; grab-random : [Counter X] -> X
; Randomly grab from this counter
(define (grab-random c)
  (local (; grab : Nat [Counter X] -> X
          ; Grab the first element in c if its count is larger than n,
          ; otherwise reduce n by the count and recur
          (define (grab n c)
            (cond [(< n (pair-y (first c))) (pair-x (first c))]
                  [else (grab (- n (pair-y (first c))) (rest c))])))
    (grab (random (total-size c)) c)))


; grab-n : [Counter X] Nat -> [List-of X]
; Grab from the counter n times
(define (grab-n counter n)
  (build-list n (λ(x) (grab-random counter))))

(check-within (count-grabs MARBLE-BAG (grab-n MARBLE-BAG 10000))
              (expected-counts MARBLE-BAG 10000)
              100)

; =================================================================

; A WritingStyle is a [Mapping String [Counter String]]
; and represents how often some words follow another,
; along with what words start and end a sentence.
; The empty string is associated with words that start a sentence,
; and how many times a word ends a sentence can be
; determined by the count of "." in its associated Counter.
 
; A Sentence is a [List-of String]

(define STYLE-EXAMPLE
  '(("great" (("." 1))) ; "great" ends a sentence once
    ("am" (("great" 1) ("i" 1))) ; "am" is followed by "great" once and "i" once
    ("i" (("am" 1) ("." 1))) ; "i" is followed by "am" once and ends a sentence once
    ("" (("i" 1) ("how" 2))) ; "i" starts a sentence once and "how" starts a sentence twice
    ("how" (("am" 1) ("are" 1))) ; "how" is followed by "am" once and "are" once
    ("you" (("." 1))) ; "you" ends a sentence once
    ("are" (("you" 1))))) ; "are" is followed by "you" once

; add-to-ws : WritingStyle String String
; updates ws to indicate that first-word was followed by second-word once more than indicated in ws
(check-expect (add-to-ws STYLE-EXAMPLE "great" "am") '(("great" (("." 1))) ; "great" ends a sentence once
                                                      ("am" (("great" 2) ("i" 1))) ; "am" is followed by "great" once and "i" once
                                                      ("i" (("am" 1) ("." 1))) ; "i" is followed by "am" once and ends a sentence once
                                                      ("" (("i" 1) ("how" 2))) ; "i" starts a sentence once and "how" starts a sentence twice
                                                      ("how" (("am" 1) ("are" 1))) ; "how" is followed by "am" once and "are" once
                                                      ("you" (("." 1))) ; "you" ends a sentence once
                                                      ("are" (("you" 1))))) ; "are" is followed by "you" once

(define (add-to-ws ws first-word second-word)
  (local [; helper: Pair[String [Counter String]] -> Pair[String [Counter String]]
          ;
          (define (helper pair)
            (if (string=? (pair-x pair) second-word)
                (make-pair (pair-x pair) (add-to-counter (pair-y pair) first-word))
                pair))]
  (map helper ws)))


; update-ws : Sentence WritingStyle
;


            
                         

       