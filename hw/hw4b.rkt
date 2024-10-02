#lang htdp/bsl

; A ListOfNumbers (LON) is one of
; - '()
; - (cons Number LON)
; and represents a list of Numbers

; LON Examples
(define POS-INTS (list 1 2 3 4 5 6 7 8 9 10))
(define ODD (list 1 3 5 7 9))
(define EVEN (list 2 4 6 8 10))
(define PRIMES (list 2 3 5 7 11))

; LON Template
; lon-temp : LON -> ?
(define (lon-temp lon)
  (cond [(empty? lon) ...]
        [(cons? lon)
         (... (first lon)
              (lon-temp (rest lon)) ...)]))

; 1 -----------------------------------------------------------

; interleave : LON LON -> LON
; produces a single list, alternating from each of two LONs,
; beginning with the first.
; If the lists have different lengths,
; finish with all the remaining items of the longer list.
(check-expect (interleave ODD EVEN) (list 1 2 3 4 5 6 7 8 9 10))
(check-expect (interleave EVEN ODD) (list 2 1 4 3 6 5 8 7 10 9))
(check-expect (interleave POS-INTS ODD) (list 1 1 2 3 3 5 4 7 5 9 6 7 8 9 10))
(check-expect (interleave EVEN POS-INTS) (list 2 1 4 2 6 3 8 4 10 5 6 7 8 9 10))

(define (interleave lon1 lon2)
  (cond [(empty? lon1) lon2]
        [(cons? lon1)
         (cons (first lon1)
               (interleave lon2 (rest lon1)))]))

; 2 -----------------------------------------------------------

; prime-factors : Number -> LON
; produces a list of the prime factors of a positive integer n (n > 1)
; in ascending order (including duplicates)
(check-expect (prime-factors 2) (list 2))
(check-expect (prime-factors 4) (list 2 2))
(check-expect (prime-factors 12) (list 2 2 3))
(check-expect (prime-factors 15) (list 3 5))
(check-expect (prime-factors 100) (list 2 2 5 5))

(define (prime-factors n)
  (prime-factors-helper n 2))

; prime-factors-helper : Number Number -> LON
; produces a list of the prime factors of a positive integer n (n > 1)
; in ascending order (including duplicates),
; starting from a given divisor
(check-expect (prime-factors-helper 2 2) (list 2))
(check-expect (prime-factors-helper 4 2) (list 2 2))
(check-expect (prime-factors-helper 12 2) (list 2 2 3))
(check-expect (prime-factors-helper 15 2) (list 3 5))
(check-expect (prime-factors-helper 100 2) (list 2 2 5 5))

(define (prime-factors-helper n divisor)
  (cond [(> divisor n) '()]
        [(= (remainder n divisor) 0)
         (cons divisor (prime-factors-helper (/ n divisor) divisor))]
        [else (prime-factors-helper n (add1 divisor))]))

; 3 -----------------------------------------------------------

; A ListofListofNumbers (LOLON) is one of
; - (cons LON '())
; - (cons LON LOLON)
; and represents a non empty list of lists of numbers

; LOLON Examples
(define ODD-EVEN (list ODD EVEN))
(define EVEN-ODD (list EVEN ODD))
(define POS-INTS-PRIMES (list POS-INTS PRIMES))
(define POS-INTS-EMPTY (list POS-INTS '()))
(define EMPTY-POS-INTS (list '() POS-INTS))

; LOLON Template
; lolon-temp : LOLON -> ?
(define (lolon-temp lolon)
  (cond [(empty? (rest lolon)) (... (lon-temp (first lolon)) ...)]
        [(cons? (rest lolon))
         (... (lon-temp (first lolon))
              (lolon-temp (rest lolon)) ...)]))

; intersection : LOLON -> LON
; gives the numbers that appear in every sublist of a LOLON where
; elements are in the order they appear in the first list they are present in
; and each inner list does not repeat numbers
(check-expect (intersection (list '() '())) '())
(check-expect (intersection (list '() (list 1 2 3))) '())
(check-expect (intersection (list '() (list 1 2 3) (list 1 2))) '())
(check-expect (intersection (list (list 1 2 3) '())) '())
(check-expect (intersection (list (list 1 2 3) (list 2 4 5))) (list 2))
(check-expect (intersection (list (list 2 4 5) (list 1 2 3))) (list 2))
(check-expect (intersection (list (list 1 2 3) (list 2 4 3))) (list 2 3))
(check-expect (intersection (list (list 1 2 3) (list 2 4 5) (list 5 6 2))) (list 2))
(check-expect (intersection (list (list 1 2 5) (list 2 4 5) (list 5 6 2))) (list 2 5))
(check-expect (intersection (list POS-INTS)) POS-INTS)
(check-expect (intersection ODD-EVEN) '())
(check-expect (intersection POS-INTS-PRIMES) (list 2 3 5 7))
(check-expect (intersection POS-INTS-EMPTY) '())

(define (intersection lolon)
  (cond [(empty? (rest lolon)) (first lolon)]
        [(cons? (rest lolon))
         (intersection-helper (first lolon)
                              (intersection (rest lolon)))]))

; intersection-helper: LON LON -> LON
; gives the numbers that appear in both LONs
(check-expect (intersection-helper '() '()) '())
(check-expect (intersection-helper '() ODD) '())
(check-expect (intersection-helper EVEN '()) '())
(check-expect (intersection-helper EVEN ODD) '())
(check-expect (intersection-helper EVEN POS-INTS) (list 2 4 6 8 10))
(check-expect (intersection-helper POS-INTS PRIMES) (list 2 3 5 7))

(define (intersection-helper lon1 lon2)
  (cond [(empty? lon1) '()]
        [(cons? lon1)
         (if (contains-num? (first lon1) lon2)
             (cons (first lon1) (intersection-helper (rest lon1) lon2))
             (intersection-helper (rest lon1) lon2))]))

; contains? : Number LON
; determines whether a LON contains a Number num
(check-expect (contains-num? 2 (list 1 2 3)) #t)
(check-expect (contains-num? 1 (list 1 2 3)) #t)
(check-expect (contains-num? 3 (list 1 2 3)) #t)
(check-expect (contains-num? 0 (list 1 2 3)) #f)
(check-expect (contains-num? 0 '()) #f)

(define (contains-num? num lon)
  (cond [(empty? lon) #false]
        [(cons? lon)
         (or (= (first lon) num)
             (contains-num? num (rest lon)))]))

; 4 -----------------------------------------------------------

; all-contanied-in? : LOLON LOLON -> Boolean
; determines whether every list in the first LOLON is contained somewhere
; in the second LOLON

(check-expect (all-contained-in? (list ODD) ODD-EVEN) #t)
(check-expect (all-contained-in? ODD-EVEN (list ODD)) #f)
(check-expect (all-contained-in? ODD-EVEN ODD-EVEN) #t)
(check-expect (all-contained-in? ODD-EVEN POS-INTS-PRIMES) #f)
(check-expect (all-contained-in? ODD-EVEN (list ODD PRIMES EVEN POS-INTS)) #t)
(check-expect (all-contained-in? (list ODD PRIMES EVEN POS-INTS) ODD-EVEN) #f)
(check-expect (all-contained-in? POS-INTS-EMPTY EMPTY-POS-INTS) #t)
(check-expect (all-contained-in? (list (list)) (list '())) #t)
(check-expect (all-contained-in? (list (list 1 2 3)) (list (list 1 2 3) (list 4 5 6))) #t)
(check-expect (all-contained-in? (list (list 1 2 3)) (list (list 1 2 3) (list 4 5 6))) #t)
(check-expect (all-contained-in? (list (list 1 2 3)) (list (list 3 1 2))) #t)

(define (all-contained-in? lolon1 lolon2)
  (cond [(empty? (rest lolon1)) (contains-lon? (first lolon1) lolon2)]
        [(cons? (rest lolon1))
         (and (contains-lon? (first lolon1) lolon2)
              (all-contained-in? (rest lolon1) lolon2))]))

; contains-lon? : LON LOLON -> Boolean
; determines whether a LOLON contains a LON
(check-expect (contains-lon? EVEN EVEN-ODD) #t)
(check-expect (contains-lon? EVEN ODD-EVEN) #t)
(check-expect (contains-lon? ODD EVEN-ODD) #t)
(check-expect (contains-lon? PRIMES EVEN-ODD) #f)

(define (contains-lon? lon lolon)
  (cond [(empty? lolon) #false]
        [(cons? lolon)
         (or (same-lon? (first lolon) lon)
             (contains-lon? lon (rest lolon)))]))

; same-lon? : LON LON -> Boolean
; determines whether two LONs are the same
; (same if they both LONs have all the same elements in the same order with the same size)
(check-expect (same-lon? ODD ODD) #t)
(check-expect (same-lon? ODD EVEN) #f)
(check-expect (same-lon? '() '()) #t)
(check-expect (same-lon? (list 1 2) (list 2 1)) #f)

(define (same-lon? lon1 lon2)
  (cond [(empty? lon1) (empty? lon2)]
        [(cons? lon1)
         (and (cons? lon2)
              (= (first lon1) (first lon2))
              (same-lon? (rest lon1) (rest lon2)))]))

; 5 -----------------------------------------------------------

; powerlist : LON -> LOLON
; gives all possible sublists of a list of numbers.
; the given initial list contains no duplicates.
; sublists can occur in any order.(
(check-expect (powerlist '()) (list '()))

;(check-expect (powerlist (list 1)) (list '() (list 1)))
(check-expect (equal-lists? (powerlist (list 1))
                            (list '() (list 1))) #t)
(check-expect (equal-lists? (powerlist (list 1))
                            (list (list 1) '())) #t)

#;(check-expect (powerlist (list 1 2)) (list '() (list 1) (list 2) (list 1 2)))
(check-expect (equal-lists? (powerlist (list 1 2))
                            (list '() (list 1) (list 2) (list 1 2))) #t)
(check-expect (equal-lists? (powerlist (list 1 2))
                            (list '() (list 2) (list 1) (list 1 2))) #t)
(check-expect (equal-lists? (powerlist (list 1 2))
                            (list '() (list 1 2) (list 2) (list 1))) #t)

#;(check-expect (powerlist (list 1 2 3)) (list '()
                                               (list 1) (list 2) (list 3)
                                               (list 1 2) (list 1 3) (list 2 3)
                                               (list 1 2 3)))
(check-expect (equal-lists? (powerlist (list 1 2 3))
                            (list '()
                                  (list 1) (list 2) (list 3)
                                  (list 1 2) (list 1 3) (list 2 3)
                                  (list 1 2 3))) #t)
(check-expect (equal-lists? (powerlist (list 1 2 3))
                            (list '()
                                  (list 3) (list 2) (list 1)
                                  (list 2 3) (list 1 3) (list 1 2)
                                  (list 1 2 3))) #t)
(check-expect (equal-lists? (powerlist (list 1 2 3))
                            (list '()
                                  (list 1 2 3)
                                  (list 3) (list 2) (list 1)
                                  (list 2 3) (list 1 3) (list 1 2))) #t)

(define (powerlist lon)
  (cond [(empty? lon) (list '())]
        [(cons? lon)
         (append (powerlist (rest lon))
                 (add-to-each (first lon) (powerlist (rest lon))))]))

; add-to-each : Number LOLON -> LOLON
; adds the given number to the front of each list in the given LOLON
(check-expect (add-to-each 0 EVEN-ODD) (list (list 0 2 4 6 8 10) (list 0 1 3 5 7 9)))
(check-expect (add-to-each 0 ODD-EVEN) (list (list 0 1 3 5 7 9) (list 0 2 4 6 8 10)))

(define (add-to-each n lolon)
  (cond [(empty? lolon) '()]
        [(cons? lolon)
         (cons (cons n (first lolon))
               (add-to-each n (rest lolon)))]))

; equal-lists : LOLON LOLON -> Boolean
; determines whether or not two LOLONs are equal
; (all elements in both are the same and the number of elements is the same
(check-expect (equal-lists? ODD-EVEN ODD-EVEN) #t)
(check-expect (equal-lists? ODD-EVEN EVEN-ODD) #t)
(check-expect (equal-lists? ODD-EVEN POS-INTS-PRIMES) #f)

(define (equal-lists? lolon1 lolon2)
  (and (all-contained-in? lolon1 lolon2)
       (all-contained-in? lolon2 lolon1)))
