#lang htdp/isl+

; 10/9/2024
; ==================
; MORE SETS
; ==================


; A [Set-of X] is a [List-of X] with duplicates prohibited

; size : (X) [Set-of X] -> Nat
;(define size length)

(define (size set) (length set))

; contains? : {X} [Set-of X] X -> Boolean
; does the given set contain the given element?
(define (contains? set elem)
  (cond [(empty? set) #f]
        [(cons? set) (or (equal? elem (first set))
                         (contains? (rest set) elem))]))

(define (contains?.2 set elem)
  (local [; [X -> Boolean]
          ; check if the current element matches the requested element
          (define (matches-elem? cur)
            (equal? cur elem))]
    ; ormap : [X -> Boolean] [List-of X] -> Boolean
    ; 
    (ormap matches-elem? set)))

(define (contains?.3 set elem)
  (ormap (λ(cur) (equal? cur elem)) set))




; add-elem : {X} X [Set-of X] [Set-of X]
; constructs a new set w everything in the original set plus the gien item
(define (add-elem elem set)
  (if (contains?.3 set elem)
      set
      (cons elem set)))

; union : {X} [Set-of X] [Set-of X] -> [Set-of X]
; constructs a new set w all the elements from either set
(define (union set1 set2)
  ; (append set 1 (filter ste1-from-set2)))
  ; cons and append takes in lists
  ; map : {X Y} [X -> Y] [List-of X] -> [List-of Y]
  ; foldr : {X Y} [X Y -> Y] Y [List-of X] -> Y
  (foldr add-elem set1 set2))

; does the bigger set setB contain the smaller set setS
(define (subset? setS setB)
  ; are all the elemens in teh smaller set in the bigger set
  (local [;
          ;
          (define (is-in-setB? elemS)
            (contains?.3 setB elemS))]
    ; (andmap (λ(elemS) (contains?.3 setB elemS)
    (andmap is-in-setB? setS)))
  

(define (set=? set1 set2)
  (and (subset? set1 set2)
       (subset? set2 set1)))

(union '(1 2 3) '(3 4 5))
(union '(1 2 ) '(3 4))
(union '(1 2 ) '(3 4))

(define (intersection set1 set2)
  (filter (λ(elem1) (contains?.3 set2 elem1)) set1))


; [Set-of X] -> [Set-of [Set-of X]]
(define (powerset set)
  ; (1 2 3) -> (1) (2) (3) (1 2) (1 3) (2 3) (1 2 3)
  (cond [(empty? set) '(())] ; 
        [(cons? set)
         (local [(define ps-of-rest (powerset (rest set)))
                 (define (add-first-to-ps ps) (cons (first set) ps))]
           (append (map add-first-to-ps ps-of-rest)
                   ps-of-rest))]))

  